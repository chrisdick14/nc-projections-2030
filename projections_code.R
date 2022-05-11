### Program 1: Pull data from Census API for 2000, 2010, and 2020 and create / output projections


### Step 1: set libraries and set key options

library(tidyverse)
library(tidycensus)

readRenviron("~/.Renviron")

options(scipen = 999)

census_api_key(key = Sys.getenv("CENSUS_API_KEY"))


osbm_projections <- read_csv("https://www.osbm.nc.gov/demog/ncprojectionsbyhisp2020/download")
county_total_proj <- read_csv("/Users/chris/Documents/DA Advisors/CarolinaDemo/data/county_total_proj.csv")


## Load Variable names for each census

vars_2000 <- load_variables(2000, "pl", cache = TRUE)
vars_2010 <- load_variables(2010, "pl", cache = TRUE)
vars_2020 <- load_variables(2020, "pl", cache = TRUE)
vars_acs <- load_variables(2019, "acs5", cache = TRUE)

# Create variable vectors for each census to pull

list_length <- c(1:73)

list_char <- ifelse(list_length < 10, paste0("0", as.character(list_length)), as.character(list_length))

vec_2000 <- c(paste0("PL0020", list_char), paste0("PL0040", list_char))
vec_2010 <- c(paste0("P0020", list_char), paste0("P0040", list_char))
vec_2020 <- c(paste0("P2_0", list_char, "N"), paste0("P4_0", list_char, "N"))


## Pull data for each Census into a separate dataframe

redist_2000 <- get_decennial(geography = "county", 
                               variables = vec_2000, 
                               year = 2000, 
                               sumfile = "pl", 
                               state = "NC")

redist_2010 <- get_decennial(geography = "county", 
                             variables = vec_2010, 
                             year = 2010, 
                             sumfile = "pl", 
                             state = "NC")

redist_2020 <- get_decennial(geography = "county", 
                             variables = vec_2020, 
                             year = 2020, 
                             sumfile = "pl", 
                             state = "NC")

citizenship <- get_acs(geography = "county",
                       variables = c(white_tot_male = "B05003H_008",
                                     white_ncit_male = "B05003H_012",
                                     white_tot_female = "B05003H_019",
                                     white_ncit_female = "B05003H_023",
                                     black_tot_male = "B05003B_008",
                                     black_ncit_male = "B05003B_012",
                                     black_tot_female = "B05003B_019",
                                     black_ncit_female = "B05003B_023",
                                     aian_tot_male = "B05003C_008",
                                     aian_ncit_male = "B05003C_012",
                                     aian_tot_female = "B05003C_019",
                                     aian_ncit_female = "B05003C_023",
                                     asian_tot_male = "B05003D_008",
                                     asian_ncit_male = "B05003D_012",
                                     asian_tot_female = "B05003D_019",
                                     asian_ncit_female = "B05003D_023",
                                     nhpi_tot_male = "B05003E_008",
                                     nhpi_ncit_male = "B05003E_012",
                                     nhpi_tot_female = "B05003E_019",
                                     nhpi_ncit_female = "B05003E_023",
                                     sor_tot_male = "B05003F_008",
                                     sor_ncit_male = "B05003F_012",
                                     sor_tot_female = "B05003F_019",
                                     sor_ncit_female = "B05003F_023",
                                     two_tot_male = "B05003G_008",
                                     two_ncit_male = "B05003G_012",
                                     two_tot_female = "B05003G_019",
                                     two_ncit_female = "B05003G_023",
                                     hisp_tot_male = "B05003I_008",
                                     hisp_ncit_male = "B05003I_012",
                                     hisp_tot_female = "B05003I_019",
                                     hisp_ncit_female = "B05003I_023"),
                       year = 2019,
                       state = "NC")


### Calculate citizenship % by race by county

cit_county2 <- citizenship %>%
  select(-5) %>%
  separate(col = "variable", sep = "_", into = c("race", "cit", "sex")) %>%
  pivot_wider(names_from = sex, values_from = estimate) %>%
  mutate(pop = male+female) %>%
  select(-(5:6)) %>%
  pivot_wider(names_from = cit, values_from = pop) %>%
  pivot_wider(names_from = race, values_from = c("tot", "ncit")) %>%
  mutate(tot_other = tot_aian + tot_nhpi + tot_two,
         ncit_other = ncit_aian + ncit_nhpi + ncit_two) %>%
  select(-c("tot_aian", "tot_nhpi", "tot_sor", "tot_two", "ncit_aian", "ncit_nhpi", "ncit_sor", "ncit_two")) %>%
  pivot_longer(cols = c(3:12), names_to = "name", values_to = "value") %>%
  separate(col = "name", sep = "_", into = c("cit", "race")) %>%
  pivot_wider(names_from = cit, values_from = value) %>%
  mutate(cit = tot-ncit,
         prop_cit = ifelse(cit == 0, 0, cit / tot)) %>%
  select(-c(4:5))

cit_county <- cit_county2 %>%
  select(-4)


### 2000: Calculate race % share of each county and then the share of 18+ for each race by county

redist_wide_2000 <- redist_2000 %>%
  mutate(table = str_sub(variable, -4, -4),
         varnum = str_sub(variable, -2, -1),
         age = ifelse(table == "2", "total", "over17")) %>%
  select(-variable, -table) %>%
  pivot_wider(names_from = varnum, values_from = value) %>%
  mutate(total_all_count = `01`,
         total_hisp_count = `02`,
         white_nh_count = `05` + `14` + `16` + `17` + `34` + `35` + `38` + `58`,
         black_nh_count = `06`+`13`+`18`+`19`+`20`+`21`+`29`+`30`+`31`+`32`+`39`+`40`+`41`+`42`+`43`+`44`+`50`+`51`+`52`+`53`+`54`+`55`+`60`+`61`+`62`+`63`+`66`+`67`+`68`+`69`+`71`+`73`, 
         asian_nh_count = `08` + `15` + `22` + `25` +`26` + `33` + `36` + `37` + `45` + `46` + `48` + `56` + `57` + `59` + `64` + `70`,
         other_nh_count = `07` + `09` + `10` + `23` + `24` + `27` + `47`)

county_race_numbers_2000 <- redist_wide_2000 %>%
  select(-(4:77)) %>%
  rename(white = white_nh_count,
         black = black_nh_count,
         asian = asian_nh_count,
         other = other_nh_count,
         hisp  = total_hisp_count) %>%
  pivot_longer(cols = c(4:8), names_to = "race", values_to = "cen_2000") %>%
  pivot_wider(names_from = age, values_from = cen_2000) %>%
  rename(cen_2000_total = total,
         cen_2000_18p = over17)
  

county_race_2000 <- redist_wide_2000 %>%
  select(-(4:76)) %>%
  filter(age == "total") %>%
  mutate(white = white_nh_count / total_all_count,
         black = black_nh_count / total_all_count,
         asian = asian_nh_count / total_all_count,
         other = other_nh_count / total_all_count,
         hisp  = total_hisp_count / total_all_count) %>%
  select(-c(3:9)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_race_2000")

county_raceage_2000 <- redist_wide_2000 %>%
  select(-(4:77)) %>%
  pivot_wider(names_from = age, values_from = c(4:8)) %>%
  mutate(white = white_nh_count_over17 / white_nh_count_total,
         black = black_nh_count_over17 / black_nh_count_total,
         asian = asian_nh_count_over17 / asian_nh_count_total,
         other = other_nh_count_over17 / other_nh_count_total,
         hisp  = total_hisp_count_over17 / total_hisp_count_total) %>%
  select(-c(3:12)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_18p_race_2000")

### 2010: Calculate race % share of each county and then the share of 18+ for each race by county

redist_wide_2010 <- redist_2010 %>%
  mutate(table = str_sub(variable, -4, -4),
         varnum = str_sub(variable, -2, -1),
         age = ifelse(table == "2", "total", "over17")) %>%
  select(-variable, -table) %>%
  pivot_wider(names_from = varnum, values_from = value) %>%
  mutate(total_all_count = `01`,
         total_hisp_count = `02`,
         white_nh_count = `05` + `14` + `16` + `17` + `34` + `35` + `38` + `58`,
         black_nh_count = `06`+`13`+`18`+`19`+`20`+`21`+`29`+`30`+`31`+`32`+`39`+`40`+`41`+`42`+`43`+`44`+`50`+`51`+`52`+`53`+`54`+`55`+`60`+`61`+`62`+`63`+`66`+`67`+`68`+`69`+`71`+`73`, 
         asian_nh_count = `08` + `15` + `22` + `25` +`26` + `33` + `36` + `37` + `45` + `46` + `48` + `56` + `57` + `59` + `64` + `70`,
         other_nh_count = `07` + `09` + `10` + `23` + `24` + `27` + `47`)

county_race_numbers_2010 <- redist_wide_2010 %>%
  select(-(4:77)) %>%
  rename(white = white_nh_count,
         black = black_nh_count,
         asian = asian_nh_count,
         other = other_nh_count,
         hisp  = total_hisp_count) %>%
  pivot_longer(cols = c(4:8), names_to = "race", values_to = "cen_2010") %>%
  pivot_wider(names_from = age, values_from = cen_2010) %>%
  rename(cen_2010_total = total,
         cen_2010_18p = over17)

county_race_2010 <- redist_wide_2010 %>%
  select(-(4:76)) %>%
  filter(age == "total") %>%
  mutate(white = white_nh_count / total_all_count,
         black = black_nh_count / total_all_count,
         asian = asian_nh_count / total_all_count,
         other = other_nh_count / total_all_count,
         hisp  = total_hisp_count / total_all_count) %>%
  select(-c(3:9)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_race_2010")

county_raceage_2010 <- redist_wide_2010 %>%
  select(-(4:77)) %>%
  pivot_wider(names_from = age, values_from = c(4:8)) %>%
  mutate(white = white_nh_count_over17 / white_nh_count_total,
         black = black_nh_count_over17 / black_nh_count_total,
         asian = asian_nh_count_over17 / asian_nh_count_total,
         other = other_nh_count_over17 / other_nh_count_total,
         hisp  = total_hisp_count_over17 / total_hisp_count_total) %>%
  select(-c(3:12)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_18p_race_2010")

### 2020: Calculate race % share of each county and then the share of 18+ for each race by county

redist_wide_2020 <- redist_2020 %>%
  mutate(table = str_sub(variable, -6, -6),
         varnum = str_sub(variable, -3, -2),
         age = ifelse(table == "2", "total", "over17")) %>%
  select(-variable, -table) %>%
  pivot_wider(names_from = varnum, values_from = value) %>%
  mutate(total_all_count = `01`,
         total_hisp_count = `02`,
         white_nh_count = `05` + `14` + `16` + `17` + `34` + `35` + `38` + `58`,
         black_nh_count = `06`+`13`+`18`+`19`+`20`+`21`+`29`+`30`+`31`+`32`+`39`+`40`+`41`+`42`+`43`+`44`+`50`+`51`+`52`+`53`+`54`+`55`+`60`+`61`+`62`+`63`+`66`+`67`+`68`+`69`+`71`+`73`, 
         asian_nh_count = `08` + `15` + `22` + `25` +`26` + `33` + `36` + `37` + `45` + `46` + `48` + `56` + `57` + `59` + `64` + `70`,
         other_nh_count = `07` + `09` + `10` + `23` + `24` + `27` + `47`)

county_race_numbers_2020 <- redist_wide_2020 %>%
  select(-(4:77)) %>%
  rename(white = white_nh_count,
         black = black_nh_count,
         asian = asian_nh_count,
         other = other_nh_count,
         hisp  = total_hisp_count) %>%
  pivot_longer(cols = c(4:8), names_to = "race", values_to = "cen_2020") %>%
  pivot_wider(names_from = age, values_from = cen_2020) %>%
  rename(cen_2020_total = total,
         cen_2020_18p = over17)

county_race_2020 <- redist_wide_2020 %>%
  select(-(4:76)) %>%
  filter(age == "total") %>%
  mutate(white = white_nh_count / total_all_count,
         black = black_nh_count / total_all_count,
         asian = asian_nh_count / total_all_count,
         other = other_nh_count / total_all_count,
         hisp  = total_hisp_count / total_all_count) %>%
  select(-c(3:9)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_race_2020")

### Check for small cells
county_raceage_count_2020 <- redist_wide_2020 %>%
  select(-(4:77)) %>%
  pivot_longer(cols=c(4:8), names_to = "race", values_to = "count") %>%
  pivot_wider(names_from = age, values_from = count) %>%
  mutate(small_flag = ifelse(total <= 500, 1, 0)) %>%
  separate(race, sep = "_", into = c("race", "hisp", "type")) %>%
  mutate(race = ifelse(race == "total", "hisp", race)) %>%
  select(c(1, 2, 3, 8))



county_raceage_2020 <- redist_wide_2020 %>%
  select(-(4:77)) %>%
  pivot_wider(names_from = age, values_from = c(4:8)) %>%
  mutate(white = white_nh_count_over17 / white_nh_count_total,
         black = black_nh_count_over17 / black_nh_count_total,
         asian = asian_nh_count_over17 / asian_nh_count_total,
         other = other_nh_count_over17 / other_nh_count_total,
         hisp  = total_hisp_count_over17 / total_hisp_count_total) %>%
  select(-c(3:12)) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "prop_18p_race_2020") %>%
  full_join(county_raceage_count_2020, by = c("GEOID", "NAME", "race"))

### Create Race Share Projection


county_race_projection <- county_race_2000 %>%
  full_join(county_race_2010, by = c("GEOID", "NAME", "race")) %>%
  full_join(county_race_2020, by = c("GEOID", "NAME", "race")) %>%
  mutate(linear_2010_to_2020 = prop_race_2020 - prop_race_2010,
         linear_2000_to_2020 = (prop_race_2020-prop_race_2000) / 2,
         prop_race_2030 = prop_race_2020 + linear_2010_to_2020) %>%
  arrange(desc(prop_race_2030))

county_race_age_projection <- county_raceage_2000 %>%
  full_join(county_raceage_2010, by = c("GEOID", "NAME", "race")) %>%
  full_join(county_raceage_2020, by = c("GEOID", "NAME", "race")) %>%
  mutate(linear_2010_to_2020 = prop_18p_race_2020 - prop_18p_race_2010,
         linear_2000_to_2020 = (prop_18p_race_2020-prop_18p_race_2000) / 2,
         prop_18p_race_2030 = ifelse(small_flag == 1, prop_18p_race_2020, 
                                     prop_18p_race_2020 + linear_2010_to_2020)) %>%
  arrange(desc(prop_18p_race_2030))

county_total_proj <- county_total_proj %>%
  mutate(GEOID = as.character(GEOID))

county_race_proj_count <- county_race_projection %>%
  left_join(county_total_proj, by = c("GEOID", "NAME")) %>%
  mutate(population_2030 = round(prop_race_2030*value_2030)) %>%
  select(GEOID, NAME, race, population_2030) %>%
  pivot_wider(names_from = race, values_from = population_2030) %>%
  full_join(county_total_proj, by = c("GEOID", "NAME")) %>%
  select(1:7, 13) %>%
  mutate(total = white + black + asian + other + hisp,
         dif = total - value_2030,
         white = white + dif) %>%
  select(1:7) %>%
  pivot_longer(cols = c(3:7), names_to = "race", values_to = "pop_2030")
  
county_raceage_proj_count <- county_race_proj_count %>%
  full_join(county_race_age_projection, by = c("GEOID", "NAME", "race")) %>%
  mutate(pop_18p_2030 = round(pop_2030 * prop_18p_race_2030)) %>%
  select(GEOID, NAME, race, pop_2030, pop_18p_2030)

osbm_review <- osbm_projections %>%
  select(fips, hisp, race, sex, `2020` = tot2020, `2030` = tot2030) %>%
  filter(sex == "Total" & !(fips == "37000")) %>%
  select(-sex) %>%
  pivot_longer(cols = c(4:5), names_to = "year", values_to = "value") %>%
  pivot_wider(names_from = c("hisp", "race"), values_from = value) %>%
  select(GEOID = fips, 
         year = year,
         total = Total_Total,
         count_hisp = Hispanic_Total,
         count_white = `non-Hispanic_White`,
         count_non_white = `non-Hispanic_non-White`) %>%
  mutate(GEOID = as.character(GEOID),
         prop_hisp = count_hisp / total,
         prop_white = count_white / total,
         prop_non_white = count_non_white / total) %>%
  select(-3) %>%
  pivot_longer(cols = c(3:8), names_to = "race", values_to = "osbm") %>%
  separate(col = "race", into = c("type", "race"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = c("type", "year"), values_from = osbm, names_glue = "{.value}_{year}_{type}")

county_final <- county_raceage_proj_count %>%
  full_join(cit_county, by = c("GEOID", "NAME", "race")) %>%
  mutate(cvap_2030 = round(pop_18p_2030 * prop_cit)) %>%
  arrange(GEOID)




### Final Review File - Historical data

# Variables:
#  - GEOID: FIPS state | county
#  - NAME: County Name
#  - race: Race as we have defined it (note that ACS and OSBM projections are not quite the same)
#  - small_flag: If cell under 500 in Census 2020, hold 18 plus share constant
#  - cen_2000_total: Total Population in Census 2000
#  - cen_2000_18p: 18 plus population in Census 2000
#  - cen_2010_total: Total Population in Census 2010
#  - cen_2010_18p: 18 plus population in Census 2010
#  - cen_2020_total: Total Population in Census 2020
#  - cen_2020_18p: 18 plus population in Census 2020
#  - acs_5year_cvap: CVAP population in the 5 year ACS 2015-2019
#  - proj_2030_total: Total Population projection for 2030
#  - proj_2030_18p: 18 plus population projection for 2030
#  - proj_2030_cvap: CVAP projection 2030
#  - osbm_2030_total: Total population projection for 2030 from osbm
#  - osbm_2030_18p: 18 plus population projection for 2030 from osbm
#  - delta_2030_total: difference between our projection and osbm for total pop (osbm-our projection)
#  - delta_2030_18p: difference between our projection and osbm for 18 plus pop
#  - growth_2020_2030_total: percent population growth for total population between our projection and census 2020
#  - growth_2020_2030_18p: percent population growth for 18 plus population between our projection and census 2020
#  - prop_race_2000: Proportion share of total pop by race within a county for Census 2000
#  - prop_race_2010: Proportion share of total pop by race within a county for Census 2010
#  - prop_race_2020: Proportion share of total pop by race within a county for Census 2020
#  - prop_race_2030: Proportion share of total pop by race within a county for our 2030 projection
#  - osbm_2030_race_prop: Proportion share of total pop by race within a county for osbm 2030 projection
#  - diff_2020_2030_race_prop: Difference between the race proportion for 2020 and our projection
#  - diff_osbm_race_prop: Difference between the race proportion between OSBM and our projection
#  - prop_18p_race_2000: Proportion share of 18+ within a race within a county for Census 2000
#  - prop_18p_race_2010: Proportion share of 18+ within a race within a county for Census 2010
#  - prop_18p_race_2020: Proportion share of 18+ within a race within a county for Census 2020
#  - prop_18p_race_2030: Proportion share of 18+ within a race within a county for our 2030 projection
#  - osbm_2030_age_prop: Proportion share of 18+ within a race within a county for osbm 2030 projection
#  - diff_2020_2030_age_prop: Difference between the age proportion for 2020 and our projection
#  - diff_osbm_age_prop: Difference between the age proportion between OSBM and our projection
#  - proj_2030_cvap_prop: Proprtion of CVAP within 18+ by race by county for our projection
#  - acs_2019_cvap_prop: Proprtion of CVAP within 18+ by race by county for ACS 2015-2019


county_review_file_hist <- county_race_numbers_2000 %>%
  full_join(county_race_numbers_2010, by = c("GEOID", "NAME", "race")) %>%
  full_join(county_race_numbers_2020, by = c("GEOID", "NAME", "race")) %>%
  full_join(cit_county2, by = c("GEOID", "NAME", "race")) %>%
  rename(acs_5year_cvap = cit,
         acs_2019_cvap_prop = prop_cit) %>%
  full_join(county_final, by = c("GEOID", "NAME", "race")) %>%
  rename(proj_2030_total = pop_2030,
         proj_2030_18p = pop_18p_2030,
         proj_2030_cvap_prop = prop_cit,
         proj_2030_cvap = cvap_2030) %>%
  mutate(growth_2020_2030_total = (proj_2030_total - cen_2020_total) / cen_2020_total * 100,
         growth_2020_2030_18p   = (proj_2030_18p - cen_2020_18p) / cen_2020_18p * 100) %>%
  full_join(county_race_projection, by = c("GEOID", "NAME", "race")) %>%
  select(-(21:22)) %>%
  mutate(diff_2020_2030_race_prop = prop_race_2030 - prop_race_2020) %>%
  full_join(county_race_age_projection, by = c("GEOID", "NAME", "race")) %>%
  select(-(27:28)) %>%
  mutate(diff_2020_2030_age_prop = prop_18p_race_2030 - prop_18p_race_2020)

write_csv(county_review_file_hist, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/racecompfile_historical.csv")


### Final Review File - OSBM RACE / Hisp

# Variables:
#  - GEOID: FIPS state | county
#  - NAME: County Name
#  - race: Race / hisp as defined by OSBM (White / non-white for hisp/non-hisp)
#  - proj_2020_total: Total Population projection for 2020
#  - proj_2030_total: Total Population projection for 2030
#  - osbm_2030_total: Total population projection for 2030 from osbm
#  - delta_2030_total: difference between our projection and osbm for total pop (osbm-our projection)
#  - prop_race_2020: Proportion share of total pop by race within a county for Census 2020
#  - prop_race_2030: Proportion share of total pop by race within a county for our 2030 projection
#  - osbm_2030_race_prop: Proportion share of total pop by race within a county for osbm 2030 projection
#  - diff_2020_2030_race_prop: Difference between the race proportion for 2020 and our projection
#  - diff_osbm_race_prop: Difference between the race proportion between OSBM and our projection

proj_2020_recode <- county_race_numbers_2020 %>%
  select(-5) %>%
  pivot_wider(names_from = race, values_from = cen_2020_total) %>%
  mutate(non_white = black + asian + other) %>%
  select(-(5:7)) %>%
  pivot_longer(cols = c(3:5), names_to = "race", values_to = "proj_2020_total")

proj_race_prop_recode <- county_race_projection %>%
  select(-c(4, 5, 7, 8)) %>%
  pivot_wider(names_from = race, values_from = c("prop_race_2020", "prop_race_2030")) %>%
  mutate(prop_race_2020_non_white = prop_race_2020_black + prop_race_2020_asian + prop_race_2020_other,
         prop_race_2030_non_white = prop_race_2030_black + prop_race_2030_asian + prop_race_2030_other) %>%
  select(-c(4,5,7,9,10,12)) %>%
  pivot_longer(cols = c(3:8), names_to = "variable", values_to = "value") %>%
  separate(col = variable, sep = "_", into = c("measure", "type", "year", "race"), extra = "merge") %>%
  mutate(variable = paste(measure, type, year, sep = "_")) %>%
  select(-(3:5)) %>%
  pivot_wider(names_from = variable, values_from = value)

proj_2030_recode <- county_final %>%
  select(-(5:7)) %>%
  pivot_wider(names_from = race, values_from = pop_2030) %>%
  mutate(non_white = black + asian + other)%>%
  select(-c(4,5,7)) %>%
  pivot_longer(cols = c(3:5), names_to = "race", values_to = "proj_2030_total")

county_review_file_osbm <- proj_2020_recode %>%
  full_join(proj_2030_recode, by = c("GEOID", "NAME", "race")) %>%
  full_join(osbm_review, by = c("GEOID", "race")) %>%
  mutate(delta_2030_total = proj_2030_total - osbm_2030_count,
         delta_2020_total = proj_2020_total - osbm_2020_count,
         percentdif_2030_total = delta_2030_total / proj_2030_total,
         percentdif_2020_total = delta_2020_total / proj_2020_total) %>%
  full_join(proj_race_prop_recode, by = c("GEOID", "NAME", "race")) %>%
  mutate(diff_2020_race_props = prop_race_2020 - osbm_2020_prop,
         diff_2030_race_props = prop_race_2030 - osbm_2030_prop,
         growth_race = prop_race_2030 - prop_race_2020) %>%
  arrange(desc(growth_race))
  

write_csv(county_review_file_osbm, "/Users/chris/Documents/DA Advisors/CarolinaDemo/data/racecompfile_osbm.csv")

























