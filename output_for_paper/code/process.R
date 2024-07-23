
# source pacakges ---------------------------------------------------------

# load packages
pacman::p_load(tidyverse, countrycode, RColorBrewer, readxl)



# read data ---------------------------------------------------------------

# ssp 2023
d_ssp_raw <- read.csv("input/ssp_input_data.csv")

# world bank income classification
wb_in_cla <- read_excel('input/CLASS.xlsx', col_names = T, sheet = 1)


# a <- read.csv('input/productivity_projections_IIAS_2023 (1).csv')
# b <- read.csv('input/productivity_projections_IIAS_2023_check.csv')
# setdiff(a$iso3c, d_ssp$iso3c)
# length(setdiff(a$iso3c, b$iso3c))
# 
# b <- na.omit(subset(d_ssp, scen == 'SSP1' & year == 2100, c(iso3c, iiasa_2023_gdp, oecd_2023_gdp)))

# process data ------------------------------------------------------------

# WB income classes
in_cla <- wb_in_cla %>%
  mutate(iso3c = Code,
         in_cla = recode(`Income group`,
                         "Low income" = "Low Income",
                         "Lower middle income" = "Lower Middle Income",
                         "Upper middle income" = "Upper Middle Income",
                         "High income" = "High Income")) %>%
  select(iso3c, in_cla)


# SSP data

# some manicure
d_ssp <- d_ssp_raw %>%
  filter(Model != "© SSP Scenario Explorer https://data.ece.iiasa.ac.at/ssp") %>%
  mutate(m = recode(Model,
                    "OECD ENV-Growth 2013" = "oecd_2013",
                    "OECD ENV-Growth 2023" = "oecd_2023",
                    "IIASA GDP 2013" = "iiasa_2013",
                    "IIASA GDP 2023" = "iiasa_2023",
                    "IIASA-WiC POP 2013" = "iiasa_wic_2013",
                    "IIASA-WiC POP 2023" = "iiasa_wic_2023"),
         scen = substring(Scenario, 1, 4),
         iso3c = countrycode(Region, "country.name", "iso3c"),
         v = recode(Variable, "GDP|PPP" = "gdp", "Population" = "pop")) %>%
  select(!c(Model, Scenario, Region, Variable, Unit)) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = 'year',
               names_prefix = "X") %>%
  pivot_wider(names_from = c(m,v),
              values_from = value) %>%
  mutate(iiasa_wic_2013_pop = iiasa_wic_2013_pop/1000,
         iiasa_wic_2023_pop = iiasa_wic_2023_pop/1000) %>%
  mutate(oecd_2013_gdp_pc = oecd_2013_gdp/iiasa_wic_2013_pop,
         oecd_2023_gdp_pc = oecd_2023_gdp/iiasa_wic_2023_pop,
         iiasa_2013_gdp_pc = iiasa_2013_gdp/iiasa_wic_2013_pop,
         iiasa_2023_gdp_pc = iiasa_2023_gdp/iiasa_wic_2023_pop,
         year = as.numeric(year)) %>%
  filter(year >= 2025) %>%
  select(scen, iso3c, year, iiasa_2023_gdp, iiasa_2023_gdp_pc, iiasa_2013_gdp, iiasa_2013_gdp_pc, oecd_2023_gdp, oecd_2023_gdp_pc, oecd_2013_gdp, oecd_2013_gdp_pc, iiasa_wic_2023_pop, iiasa_wic_2013_pop)


# tmp ---------------------------------------------------------------------


# tmp - load SSP_IIASA_GDP_2023
iiasa_2023_gdp <- readRDS(file = 'input/SSP_IIASA_GDP_2023.RDS')

# add updated data
d_ssp <- left_join(d_ssp, select(iiasa_2023_gdp, c(scenario, iso3c, year, Y)), by = c("scen" = "scenario", "iso3c" = "iso3c", 'year' = 'year')) %>%
  mutate(iiasa_2023_gdp = Y/1000,
         iiasa_2023_gdp_pc = Y/iiasa_wic_2023_pop/1000)


# tmp ---------------------------------------------------------------------





# harmonization of 2013 and 2023 GDP projections (level in 2025 is the same)
c <- d_ssp %>%
  filter(year == 2025) %>%
  mutate(oecd_c = oecd_2013_gdp/oecd_2023_gdp,
         iiasa_c = iiasa_2013_gdp/iiasa_2023_gdp) %>%
  select(scen, iso3c, oecd_c, iiasa_c)

d_ssp <- left_join(d_ssp, c, by = c('scen', 'iso3c')) %>%
  mutate(oecd_2013_gdp = oecd_2013_gdp/oecd_c,
         oecd_2013_gdp_pc = oecd_2013_gdp_pc/oecd_c,
         iiasa_2013_gdp = iiasa_2013_gdp/iiasa_c,
         iiasa_2013_gdp_pc = iiasa_2013_gdp_pc/iiasa_c,
         year = as.numeric(year)) %>%
  select(scen, iso3c, year, iiasa_2023_gdp, iiasa_2023_gdp_pc, iiasa_2013_gdp, iiasa_2013_gdp_pc, oecd_2023_gdp, oecd_2023_gdp_pc, oecd_2013_gdp, oecd_2013_gdp_pc, iiasa_wic_2023_pop, iiasa_wic_2013_pop) %>%
  left_join(., in_cla, "iso3c")








# write -------------------------------------------------------------------

saveRDS(d_ssp, 'output/d_ssp.RDS')
saveRDS(in_cla, 'output/in_cla.RDS')




