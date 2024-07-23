rm(list = ls())

# load data and packages --------------------------------------------------

# load packages
pacman::p_load(tidyverse, countrycode, RColorBrewer, pwt10, wcde)
# 
# # historic gdp per capita
# data("pwt10.01")
# d_h_gdp_raw <- pwt10.01
# d_h_pop_raw <- get_wcde(indicator = 'pop', scenario = 2, version = 'wcde-v2')
# 
# # historic life expectancy 
# d_h_le_raw <- get_wcde(indicator =  'e0', scenario = 2, version = 'wcde-v2')
# 
# # historic mean years of schooling
# d_h_mys_raw <- get_wcde(indicator =  'bmys', scenario = 2, version = 'wcde-v2')
# 
# # historic expected years of schooling
# d_h_eys_raw <- get_wcde(indicator =  'mys', scenario = 2, version = 'wcde-v2')


# projected gdp per capita
d_p_gdp_pc_raw <- readRDS('input/d_ssp.RDS')

# projected life expectancy 
d_p_le_raw <- get_wcde(indicator =  'e0', scenario = c(1, 2, 3, 4, 5), version = 'wcde-v3')

# projected mean years of schooling
d_p_mys_raw <- get_wcde(indicator =  'bmys', scenario = c(1, 2, 3, 4, 5), version = 'wcde-v3')

# projected expected years of schooling
d_p_eys_raw <- get_wcde(indicator =  'mys', scenario = c(1, 2, 3, 4, 5), version = 'wcde-v3')




# process data ------------------------------------------------------------

# GDP per capita
d_p_gdp_pc <- d_p_gdp_pc_raw %>%
  mutate(gdp_pc = iiasa_2023_gdp_pc) %>%
  select(scen, iso3c, year, gdp_pc)

# life expectancy
d_p_le <- d_p_le_raw %>%
  mutate(iso3c = countrycode(name, 'country.name', 'iso3c'),
         scen = paste('SSP', scenario, sep = ""),
         year = as.numeric(substr(period, 1, 4))) %>%
  na.omit() %>%
  group_by(scen, iso3c, year) %>%
  summarise(le = mean(e0))

# mean years of schooling
d_p_mys <- d_p_mys_raw %>%
  filter(age == '25+' & year >= 2025 & sex == 'Both') %>%
  mutate(iso3c = countrycode(name, 'country.name', 'iso3c'),
         scen = paste('SSP', scenario, sep = ""),
         mys = bmys) %>%
  na.omit() %>%
  select(scen, iso3c, year, mys)
  
# expected years of schooling := mys shifted by 20 years in age group 25-29
d_p_eys <- d_p_eys_raw %>%
  mutate(year = year - 20) %>%
  filter(age == '25--29' & year >= 2025 & year <= 2100 & sex == 'Both') %>%
  mutate(iso3c = countrycode(name, 'country.name', 'iso3c'),
         scen = paste('SSP', scenario, sep = ""),
         eys = mys) %>%
  select(scen, iso3c, year, eys)


# merge data
d_p_hdi <- left_join(d_p_gdp_pc, d_p_le, by = c('scen', 'iso3c', 'year')) %>%
  left_join(., d_p_mys, by = c('scen', 'iso3c', 'year')) %>%
  left_join(., d_p_eys, by = c('scen', 'iso3c', 'year')) %>%
  na.omit() %>%
  mutate(i_gdp_pc = (log(gdp_pc) - min(log(gdp_pc)))/(max(log(gdp_pc)) - min(log(gdp_pc))),
         i_le = (le - min(le))/(max(le) - min(le)),
         i_ed = (1/2)*(mys/max(mys) + eys/max(eys))) %>%
  mutate(hdi = (i_gdp_pc * i_le * i_ed) ^ (1/3))


# write -------------------------------------------------------------------

write.csv(d_p_hdi, 'output/d_p_hdi.csv', row.names = FALSE)
