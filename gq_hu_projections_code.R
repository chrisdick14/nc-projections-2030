#### County level projections: GQ and HH separate

library(tidyverse)
library(tidycensus)
library(censusapi)

readRenviron("~/.Renviron")

options(scipen = 999)

census_api_key(key = Sys.getenv("CENSUS_API_KEY"), install = TRUE)

estimates_comp_gq <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv")
estimates_comp <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020.csv")
osbm_projections <- read_csv("https://www.osbm.nc.gov/demog/ncprojectionsbyhisp2020/download")

## Load Variable names for each census

vars_2000 <- load_variables(2000, "pl", cache = TRUE)
vars_2000_sf1 <- load_variables(2000, "sf1", cache = TRUE)
vars_2010 <- load_variables(2010, "pl", cache = TRUE)
vars_2010_sf1 <- load_variables(2010, "sf1", cache = TRUE)
vars_2020 <- load_variables(2020, "pl", cache = TRUE)

## Load Total Pop and GQ pop for each county from the PL

sf1_2000 <- get_decennial(geography = "county",
                          variables = c(total_pop = "P001001",
                                        gq_pop    = "P037001"),
                          year = 2000,
                          sumfile =  "sf1",
                          state = "NC") %>%
  rename(value_2000 = value)

sf1_2010 <- get_decennial(geography = "county",
                          variables = c(total_pop = "P001001",
                                        gq_pop    = "P042001"),
                          year = 2010,
                          sumfile = "sf1",
                          state = "NC") %>%
  rename(value_2010 = value)

pl_2020 <- get_decennial(geography = "county",
                          variables = c(total_pop = "P1_001N",
                                        gq_pop    = "P5_001N"),
                          year = 2020,
                          sumfile = "pl",
                          state = "NC") %>%
  rename(value_2020 = value)

### Trim and recode gq estimates
### Getting to April 1, 2020 requires assumption of smooth monthly change from 2019 to 2020

est_gq_2020 <- estimates_comp_gq %>%
  filter(SUMLEV == "050", STATE == "37") %>%
  select(STATE, COUNTY, GQESTIMATES2020, GQESTIMATES2019) %>%
  mutate(GEOID = paste0(STATE, COUNTY),
         gq = round(GQESTIMATES2019 + ((GQESTIMATES2020 - GQESTIMATES2019) / 12) * 9)) %>%
  select(-(1:4)) %>%
  relocate(GEOID)


### Trim and recode estimates file, merge in gq and calcualte household

est_2020 <- estimates_comp %>%
  filter(SUMLEV == "050" & STATE == "37") %>%
  select(STATE, COUNTY, POPESTIMATE042020) %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  select(-(1:2)) %>%
  relocate(GEOID) %>%
  full_join(est_gq_2020, by = c("GEOID")) %>%
  rename(total = POPESTIMATE042020) %>%
  mutate(hh = total - gq) %>%
  pivot_longer(cols = c(2:4), names_to = "type", values_to = "est_2020")

### Join files together and calcualte differences

full_file <- sf1_2000 %>%
  full_join(sf1_2010, by = c("GEOID", "NAME", "variable")) %>%
  full_join(pl_2020, by = c("GEOID", "NAME", "variable")) %>%
  pivot_wider(names_from = variable, values_from = c("value_2000", "value_2010", "value_2020")) %>%
  mutate(value_2000_hh_pop = value_2000_total_pop - value_2000_gq_pop,
         value_2010_hh_pop = value_2010_total_pop - value_2010_gq_pop,
         value_2020_hh_pop = value_2020_total_pop - value_2020_gq_pop) %>%
  pivot_longer(cols = c(3:11), names_to = "variable", values_to = "value") %>%
  separate(col = variable, sep = "_", into = c("pref", "year", "type", "pop")) %>%
  select(-6) %>%
  pivot_wider(names_from = c("pref", "year"), values_from = value) %>%
  full_join(est_2020, by = c("GEOID", "type")) %>%
  mutate(perc_dif = (value_2020 - est_2020) / est_2020)

gq_file <- full_file %>%
  filter(type == "gq" & perc_dif <= -0.1)

write_csv(gq_file, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/gq_table.csv")

# Build projection and fix 2020 population based on major differences between estimates and census
# This is where we will need to make decisions.

comp_based_proj <- full_file %>%
  filter(!(type == "total")) %>%
  mutate(fixed_2020 = ifelse((type == "gq" & perc_dif <= -0.1), est_2020,
                      ifelse((type == "hh" & perc_dif <= -0.03 & (value_2020-value_2010 < 0)), est_2020, value_2020)),
         new_base = round((fixed_2020 + value_2020)/2),
         num_chg_2000_to_2020 = (new_base-value_2000)/2,
         num_chg_2010_to_2020 = new_base - value_2010,
         num_chg_avg_2020 = (num_chg_2000_to_2020 + num_chg_2010_to_2020) / 2,
         value_2030 = ifelse((type == "gq"), round(fixed_2020),
                      ifelse((type == "hh" & (value_2020-value_2010 < 0)), 
                             round(new_base * (new_base/value_2010)),
                              round(fixed_2020 + num_chg_2010_to_2020)))) %>%
  group_by(GEOID, NAME) %>%
  summarize(value_2030 = sum(value_2030))

# Recode and trim OSBM projections to merge

# Create output file for GQ and HU recodes

map_file <- full_file %>%
  filter(!(type == "total")) %>%
  select(GEOID, type, perc_dif) %>%
  pivot_wider(names_from = type, values_from = perc_dif) %>%
  mutate(gq_flag = ifelse(gq <= -0.1, 1, 0),
         hh_flag = ifelse(hh <= -0.03, 1, 0))

write_csv(map_file, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/flag_map.csv")

osbm_comp <- osbm_projections %>%
  filter(hisp == "Total" & race == "Total" & sex == "Total" & !(fips == "37000")) %>%
  select(fips, tot2030) %>%
  rename(GEOID = fips,
         est_2030 = tot2030) %>%
  mutate(GEOID = as.character(GEOID))

# Final merge file

new_ts <- full_file %>%
  filter(type == "total") %>%
  select(-type) %>%
  full_join(comp_based_proj, by = c("GEOID", "NAME")) %>%
  full_join(osbm_comp, by = c("GEOID")) %>%
  mutate(proj_dif = (value_2030 - est_2030) / est_2030,
         growth = (value_2030 - value_2020) / value_2020) %>%
  arrange(growth)

write_csv(new_ts, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/county_total_proj.csv")

# calculate July 1, 2022 county totals

jul0122 <- new_ts %>%
  mutate(yearly_change = (value_2030-value_2020) / 10,
         july1_2022 = round(yearly_change*2.25 + value_2020)) %>%
  select(GEOID, NAME, july1_2022) %>%
  arrange(GEOID)

write_csv(jul0122, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/july012022_total_proj.csv")


### Appendix of data exploration.

state_total <- new_ts %>%
  summarize(state_2000 = sum(value_2000),
            state_2010 = sum(value_2010),
            state_2020 = sum(value_2020),
            state_est_2020 = sum(est_2020),
            state_2030 = sum(value_2030),
            state_est_2030 = sum(est_2030))
  

gq_diff <- full_file %>%
  filter(type == "gq") %>%
  arrange(desc(perc_dif))

state_gq <- gq_diff %>%
  summarize(state_2000 = sum(value_2000),
            state_2010 = sum(value_2010),
            state_2020 = sum(value_2020),
            state_est_2020 = sum(est_2020))
  
  
hh_diff <- full_file %>%
  filter(type == "hh") %>%
  arrange(desc(perc_dif))
