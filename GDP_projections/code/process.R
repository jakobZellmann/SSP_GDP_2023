
# source functions --------------------------------------------------------
# remotes::install_github("GAMS-dev/gdxrrw-miro")
install.packages("pacman")
pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode)


# read data ---------------------------------------------------------------

# Penn World Tabel: https://www.rug.nl/ggdc/productivity/pwt/?lang=en
data("pwt10.01")

# Wittgenstein Center population and human capital projections
wdde_raw_historic <- get_wcde(indicator = 'pop', scenario = 1, pop_age = 'all', pop_sex = 'total', pop_edu = "four", version = 'wcde-v2')

# Wittgenstein Center population and human capital projections beta 2023
wdde_raw_2023 <- rbind(read.csv('input/PROJresult_AGE_SSP1_V13.csv'),
                       read.csv('input/PROJresult_AGE_SSP2_V13.csv'),
                       read.csv('input/PROJresult_AGE_SSP3_V13.csv'),
                       read.csv('input/PROJresult_AGE_SSP4_V13.csv'),
                       read.csv('input/PROJresult_AGE_SSP5_V13.csv'))

# IWF income forecasts
p_iwf <- readRDS('input/IWF_gdp.rds')
p_iwf_2024_raw <- read_excel('input/WEO_Data.xlsx', na = 'n/a')


# process data -------------------------------------------------------------

# PWT

## select relevant variables
pwt <- data.frame(iso3c = pwt10.01$isocode, year = pwt10.01$year, Y = pwt10.01$rgdpo, K =  pwt10.01$cn)

## exclude VEN as recent instability leads to distortions in growth path => extrapolation is more adequate
pwt <- subset(pwt, iso3c != 'VEN')

## assume that 2020 behaves like 2019 (ignore COVID distortions)
tmp <- subset(pwt, year == 2019)
tmp$year <- 2020
pwt <- rbind(pwt, tmp)


# WDDE

## align structure of wdde_raw_historic and wdde_raw_2023
wdde_2023 <- wdde_raw_2023 %>% 
  
  # only age groups unquela to '-5'
  filter(agest != -5) %>%
  
  # adjust notation: country_code, age, education 
  mutate(country_code = as.numeric(substr(.$region, 4, 6)),
         age = factor(.$agest, as.character(unique(.$agest)), labels = c(levels(wdde_raw_historic$age), rep('100+', 4))),
         education = fct_collapse(as.factor(.$edu),
                                  'No Education' = 'e1',
                                  'Primary' = c('e2', 'e3'),
                                  'Secondary' = c('e4', 'e5'),
                                  'Post Secondary' = 'e6'),
         year = .$Time,
         scenario = scen) %>%
  
  # aggregate gender
  group_by(scenario, country_code, year, age, education) %>%
  summarise(pop = sum(pop))

  
## drop country names in wdde_raw_historic
wdde_raw_historic$name <- NULL
wdde_raw_historic$scenario <- 'SSP1'


# combine historic with data with projections
wdde_raw <- ungroup(rbind(subset(wdde_raw_historic, year <= 2015),
                  wdde_2023))

# total population
pop_tot <- wdde_raw %>% 
  
  ## add iso3c country code
  mutate(iso3c = countrycode(wdde_raw$country_code, origin = 'iso3n', destination = 'iso3c')) %>%
  
  ## remove observations with unknown country code (e.g. world)
  drop_na() %>%
  
  ## aggregate
  group_by(scenario, iso3c, year) %>%
  summarise(pop_tot = sum(pop))



wdde <- wdde_raw %>% 
  
  ## add iso3c country code and working age
  mutate(iso3c = countrycode(wdde_raw$country_code, origin = 'iso3n', destination = 'iso3c'),
         working_age = fct_collapse(wdde_raw$age,
                                    "15--34" = c("15--19", "20--24", "25--29", "30--34"),
                                    "35--64" = c("35--39", "40--44", "50--54", "55--59", "60--64"))) %>%
  
  ## remove observations with unknown country code (e.g. world)
  drop_na() %>%

  ## only include working age population and countries that are included in pwt (and exclude inconsistency in the data)
  filter(working_age %in% c("15--34", "35--64") & education != 'Under 15') %>%

  ## aggregate working age groups
  group_by(scenario, iso3c, year, education, working_age) %>%
  summarise(pop = sum(pop)) %>%
  
  ## change data structur s.t. keys correspond to i,t and pop variables to L_jk
  pivot_wider( 
    names_from = c(education, working_age),
    values_from = pop
  ) %>%
  
  ## rename
  rename(L_01 = `No Education_15--34`,
         L_02 = `No Education_35--64`,
         L_11 = `Primary_15--34`,
         L_12 = `Primary_35--64`,
         L_21 = `Secondary_15--34`,
         L_22 = `Secondary_35--64`,
         L_31 = `Post Secondary_15--34`,
         L_32 = `Post Secondary_35--64`,) %>%
  
  ## add human capital variables (shift by +1 to allow logarithm below)
  mutate(L_0_ = L_01 + L_02,
         L_1_ = L_11 + L_12,
         L_2_ = L_21 + L_22,
         L_3_ = L_31 + L_32,
         L = L_0_ + L_1_ + L_2_ + L_3_) %>%
  
  ## add total population
  left_join(., pop_tot, by = c('scenario', 'iso3c', 'year'))



# extrapolate PWT

## wdde countries
wdde_c <- wdde_raw %>% 
  
  ## add iso3c country code and working age
  mutate(iso3c = countrycode(wdde_raw$country_code, origin = 'iso3n', destination = 'iso3c')) %>%
  
  ## remove observations with unknown country code (e.g. world)
  drop_na() %>%
  
  ## select iso3c
  select(iso3c)

## pwt countries
pwt_c <- pwt$iso3c

## IWF countries (2020)
iwf_c <- p_iwf %>%
  filter(year == 2020) %>%
  select(ccode)


## countries in wdde and IWF but not pwt
rel_c <- union(intersect(setdiff(wdde_c$iso3c, pwt_c), iwf_c$ccode), 'VEN')


## find nearest neighbors for Y_o_L

### output list
near_Y_o_L <- list()

### interate over rel_c
for (c in rel_c) {
  
  # gdp cap of country
  x <- p_iwf[p_iwf$ccode == c & p_iwf$year == 2020, ]$GDP.PC
  # countries to consider
  tmp1 <- p_iwf[p_iwf$year == 2020 & p_iwf$ccode != c & p_iwf$ccode %in% pwt_c, ]
  # place holder
  tmp2 <- c()
  
  # reset counter
  i <- 1
  
  while (i <= 3) {

    index <- which(abs(tmp1$GDP.PC-x)==min(abs(tmp1$GDP.PC-x), na.rm = T))
    tmp2 <- c(tmp2, tmp1$ccode[index])
    tmp1 <- tmp1[-index,]
    i <- i + 1
    
  }
  
  near_Y_o_L[[c]] <- tmp2
  
}

## some manual adaption for selected countries
near_Y_o_L[['KIR']] <- c('KAZ', 'TKM')
near_Y_o_L[['TON']] <- c('NZL', 'PHL')
near_Y_o_L[['FSM']] <- c('NZL', 'PHL')
near_Y_o_L[['VUT']] <- c('NZL', 'PHL', 'IND')
near_Y_o_L[['VEN']] <- c('GEO', 'YEM', 'TJK')



## find nearest neighbors for Y

### output list
near_Y <- list()

### interate over rel_c
for (c in rel_c) {
  
  # gdp cap of country
  x <- p_iwf[p_iwf$ccode == c & p_iwf$year == 2020, ]$GDP
  # countries to consider
  tmp1 <- p_iwf[p_iwf$year == 2020 & p_iwf$ccode != c & p_iwf$ccode %in% pwt_c, ]
  # place holder
  tmp2 <- c()
  
  # reset counter
  i <- 1
  
  while (i <= 3) {

    index <- which(abs(tmp1$GDP-x)==min(abs(tmp1$GDP-x), na.rm = T))
    tmp2 <- c(tmp2, tmp1$ccode[index])
    tmp1 <- tmp1[-index,]
    i <- i + 1
    
  }
  
  near_Y[[c]] <- tmp2
  
}


## extend pwt by NN-means of Y and K

### store extrapolation
pwt_ext <- data.frame()

### iterate over near_Y
for (c in names(near_Y)) {
  
  # subset pwt for 2020 and NN of c
  tmp1 <- na.omit(subset(pwt, iso3c %in% near_Y[[c]] & year == 2015))
  tmp2 <- na.omit(subset(pwt, iso3c %in% near_Y[[c]] & year == 2020))
  
  # calculate mean to extrapolate
  pwt_ext <- rbind(pwt_ext, data.frame(iso3c = c(c), year = c(2015, 2020), Y = c(mean(tmp1$Y), mean(tmp2$Y)), K = c(mean(tmp1$K), mean(tmp2$K))))
  
  
}

### extend pwt
pwt <- rbind(pwt, pwt_ext)

# subset wdde for countries included in extended pwt
wdde <- subset(wdde, iso3c %in% pwt$iso3c)

  
# PWT + WDDE

## join data
d <- left_join(wdde, pwt, by = c('iso3c', 'year'))

## add d_{t+1} to d
tmp <- d
tmp$year <- tmp$year - 5
names(tmp)[4:ncol(d)] <- paste(names(tmp)[4:ncol(d)], '_tp1', sep = '')
d <- left_join(d, tmp, by = c('scenario', 'iso3c', 'year'))
  
## calculate log difference ~ 1 + growth rate =: g where the year indicates the beginning of the respective period
for (i in names(d)[4:as.integer((length(names(d))-3)/2 + 3)]){
  
  d[paste(i, '_g', sep = '')] <- log(d[ , paste(i, '_tp1', sep = '')] + 1) - log(d[ , i] + 1)
  
}


# define variables for estimation
d <- d %>%
  
  ## add new variables
  mutate(Y_o_L = Y/pop_tot,
         log_Y_o_L = log(Y_o_L + 1),
         L_1__o_L = L_1_/L,
         L_2__o_L = L_2_/L,
         L_3__o_L = L_3_/L,
         Y_o_L_t_L_1__o_L = Y_o_L * L_1__o_L,
         Y_o_L_t_L_2__o_L = Y_o_L * L_2__o_L,
         Y_o_L_t_L_3__o_L = Y_o_L * L_3__o_L,
         log_Y_o_L_t_L_1__o_L = log(Y_o_L) * (L_1__o_L),
         log_Y_o_L_t_L_2__o_L = log(Y_o_L) * (L_2__o_L),
         log_Y_o_L_t_L_3__o_L = log(Y_o_L) * (L_3__o_L)) %>%
  
  ## select relevant variables
  select(c(scenario, iso3c, year, Y, L, pop_tot, pop_tot_g, Y_g, K_g, L_g, L_11_g, L_12_g, L_21_g, L_22_g, L_31_g, L_32_g, Y_o_L, log_Y_o_L, L_1__o_L, L_2__o_L, L_3__o_L, Y_o_L_t_L_1__o_L, Y_o_L_t_L_2__o_L, Y_o_L_t_L_3__o_L, log_Y_o_L_t_L_1__o_L, log_Y_o_L_t_L_2__o_L, log_Y_o_L_t_L_3__o_L)) %>%
  
  ## split by scenario
  group_by(scenario) %>%
  group_split()



# use IMF income forecast 2024 for year 2025
p_iwf_2024 <- p_iwf_2024_raw %>%
  select(!c(Country, Units, Scale, `Estimates Start After`)) %>%
  pivot_longer(names_to = 'year',
               values_to = 'gdp_pc',
               cols = !ISO) %>%
  mutate(year = as.numeric(year),
         iso3c = ISO) %>%
  left_join(., d[[2]][,c('iso3c', 'year', 'pop_tot')], by = c('iso3c', 'year')) %>%
  filter(year == 2025) %>%
  mutate(Y = gdp_pc * pop_tot / 1000) %>%
  select(iso3c, year, Y)


## 2025
p_iwf_2025_tmp <- p_iwf %>%
  filter(year == 2025) %>%
  mutate(iso3c = ccode,
         Y = GDP / 10^6) %>%
  select(iso3c, year, Y)

# update based on 2024 forecast
tmp <- left_join(p_iwf_2025_tmp, p_iwf_2024, by = c('iso3c', 'year'))
tmp$Y <- ifelse(is.na(tmp$Y.y), tmp$Y.x, tmp$Y.y)
p_iwf_2025_tmp <- select(tmp, c(iso3c, year, Y))

# fill missing values (under assumption of 10% growth in 5 years)
p_iwf_2025_tmp <- pwt %>%
  filter(year == 2020) %>%
  left_join(., p_iwf_2025_tmp, 'iso3c')

p_iwf_2025_tmp[is.na(p_iwf_2025_tmp$Y.y), 'Y.y'] <- p_iwf_2025_tmp[is.na(p_iwf_2025_tmp$Y.y), 'Y.x'] * 1.1

p_iwf_2025 <- p_iwf_2025_tmp %>%
  mutate(year = 2025, Y= Y.y) %>%
  select(iso3c, year, Y)


# store data --------------------------------------------------------------

# estimation data
saveRDS(na.omit(subset(d[[1]], year >= 1970 & year <= 2015 & iso3c %in% setdiff(pwt$iso3c, pwt_ext$iso3c))), 'output/estimation_data.RDS')
saveRDS(na.omit(subset(d[[1]], year >= 1970 & year <= 2015)), 'output/estimation_data_ext.RDS')


# projection data
for (i in 1:length(d)) {
  
  path <- paste('output/projection_data_', unique(d[[i]]$scenario), '.RDS', sep = '')
  
  saveRDS(subset(d[[i]], year > 2015), file = path)

  
}

# IWF income
saveRDS(p_iwf_2025, 'output/IWF_forecast_2025.RDS')

# near_Y_o_L
saveRDS(near_Y_o_L, 'output/near_Y_o_L.RDS')

