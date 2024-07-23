# projection of capital stock growth under the assumption of linear convergence to 4% (yearly) by 2100 --------

project_K_g <- function(SSP) {
  
  # SSP \in {SSP1, SSP2, SSP3, SSP4, SSP5} ~ SSP scenario
  
  # read projection data
  d <- readRDS(paste('output/projection_data_', SSP, '.RDS', sep = ''))
  
  # read estimation data
  d_est <- readRDS('output/estimation_data_ext.RDS')
  
  # iterate over countries
  c <- unique(d$iso3c)
  out <- c()
  for (cc in c) {
    print(cc)
    K_g_cc_t_max <- as.numeric(d_est[d_est$iso3c == cc & d_est$year == 2015, 'K_g'])
    
    # exclude observation with unknown capital stock (not included in the estimation)
    if(!is.na(K_g_cc_t_max)) {
      
      # calculate average growth for 5 year periods
      K_g_cc_proj <- 5*(0.2 - K_g_cc_t_max)/(2100 - 2015)
      K_g_cc_proj_TS <- K_g_cc_t_max + cumsum(rep(K_g_cc_proj, 17))
      
      # projection data + K_g_cc_proj_TS
      d_cc <- subset(d, iso3c == cc)
      d_cc$K_g <- K_g_cc_proj_TS
      
      # store tmp
      out <- rbind(out, d_cc)
      
    }
    
  }
  
  # return 
  return(out)
  
}



# projection of time fixed effects under the assumption of given growth rate for technological frontier --------

project_time_FE <- function(SSP, output_project_K_g) {
  
  # SSP \in {SSP1, SSP2, SSP3, SSP4, SSP5} ~ SSP scenario
  
  # rename input data
  d <- output_project_K_g
  
  # mapping of SSP scenario and assumed growth rate for 5 - year periods
  m <- (1 + c(SSP1 = 0.0035, SSP2 = 0.003, SSP3 = 0.003, SSP4 = 0.003, SSP5 = 0.004))^5
  
  # estimate model to get time fixed effects of last period
  ## read estimation data
  d_est <- readRDS('output/estimation_data.RDS')
  ## estimate
  est <- lm(formula(Y_g ~ log_Y_o_L + L_21_g + L_12_g + L_1__o_L + L_3__o_L + log_Y_o_L_t_L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)),
            d_est)
  ## extract time fixed effects of last period
  tfe <- est$coefficients['factor(year)2015']
  
  # calculate cumulative technological frontier growth for each period
  ctf_g <- cumprod(rep(m[SSP], 17))
  
  # project technological frontier TS
  tf_proj_TS <- data.frame(year = seq(2020, 2100, 5), TF_g = tfe * ctf_g)
  
  # d + tf_proj_TS
  out <- left_join(d, tf_proj_TS, by = 'year')
  
  # return
  return(out)
  
}


# projection of country fixed effects under the assumption of linear convergence of institutional influence to FE of USA by year in table 3 --------
project_country_FE <- function(SSP, output_project_time_FE) {
  
  # SSP \in {SSP1, SSP2, SSP3, SSP4, SSP5} ~ SSP scenario
  # convergence_speed \ in {1, 2, 3, 4} (4 convergence speed scenarios: min = 2100, max = as described in tabel 3, 2 in between)
  
  # load packages
  pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode)
  
  # rename input data
  d <- output_project_time_FE
  
  # mapping of SSP scenario and year of institutional convergence
  m <- data.frame(SSP1 = 2180, SSP2 = 2250, SSP3 = Inf, SSP4 = 2250, SSP5 = 2130)
  
  # estimate model to get country fixed effects
  
  ## read estimation data
  d_est <- readRDS('output/estimation_data.RDS')
  
  ## estimate
  est <- lm(formula(Y_g ~ log_Y_o_L + L_21_g + L_12_g + L_1__o_L + L_3__o_L + log_Y_o_L_t_L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)),
            d_est)
  
  ## add country FE for observations in IWF not included in PWT
  ext_c <- readRDS('output/near_Y_o_L.RDS')
  for (c in names(ext_c)) {
    
    tmp <- setNames(c(mean(est$coefficients[paste('factor(iso3c)', ext_c[[c]], sep = '')])), paste('factor(iso3c)', c, sep = ''))
    est$coefficients <- c(est$coefficients, tmp)
    
  }
  
  ## extract maximal country effects
  cf_USA <- unname(est$coefficients[paste('factor(iso3c)', 'USA', sep = '')])
  
  # iterate over countries
  c <- unique(d$iso3c)
  out <- c()
  for (cc in c) {
    
    # extract country FE
    cfe_cc <- est$coefficients[paste('factor(iso3c)', cc, sep = '')]
  
    
    # only fixed effects for non reference category
    if(!is.na(cfe_cc)) {
      
      # calculate average change of country FE for 5 year period
      cfe_cc_g <- 5*(cf_USA - cfe_cc)/(m[SSP] - 2015)
      cfe_cc_proj_TS <- cfe_cc + cumsum(rep(cfe_cc_g, 17))
      
      # projection data + K_g_cc_proj_TS
      d_cc <- subset(d, iso3c == cc)
      d_cc$c_fe <- cfe_cc_proj_TS
      
      # store tmp
      out <- rbind(out, d_cc)
      
    } else {# for reference category - cfe_cc = 0 
      
      # set cfe_cc = 0
      cfe_cc <- 0
      
      # calculate average change of country FE for 5 year period
      cfe_cc_g <- 5*(cf_USA - cfe_cc)/(m[SSP] - 2015)
      cfe_cc_proj_TS <- cfe_cc + cumsum(rep(cfe_cc_g, 17))
      
      # projection data + K_g_cc_proj_TS
      d_cc <- subset(d, iso3c == cc)
      d_cc$c_fe <- cfe_cc_proj_TS
      
      # store tmp
      out <- rbind(out, d_cc)
      
    }
    
  }
  
  # return 
  return(out)
  
}



# projection of income growth rates -----------------------------------------------------------------
project <- function(SSP) {
  
  # read projection data
  d_proj <- readRDS(paste('output/projection_data_', SSP, '.RDS', sep = ''))
  
  # read estimation data
  d_est <- readRDS('output/estimation_data.RDS')
  
  # add capital stock growth rates to projection data
  d_proj <- project_K_g(SSP)
  
  # add time fixed effects (technology frontier projections)
  d_proj <- project_time_FE(SSP, d_proj)
  
  # add country fixed effects (institutional convergence projections)
  d_proj <- project_country_FE(SSP, d_proj)
  
  # estimate model parameter
  est <- lm(formula(Y_g ~ log_Y_o_L + L_21_g + L_12_g + L_1__o_L + L_3__o_L + log_Y_o_L_t_L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)),
            d_est)
  
 
  # get relevant coefficients
  coeff <- est$coefficients[1:9]
  
  # set maximum for log_Y_o_L_t_L_3__o_L equal to USA 2015
  log_Y_o_L_max <- as.numeric(subset(d_est, iso3c == 'USA' & year == 2015, log_Y_o_L))
  
  # iterate over time
  t <- seq(2020, 2095, 5)
  for (tt in t) {
    
    # subset data for tt and not tt
    d_proj_tt <- subset(d_proj, year == tt)
    d_proj_not_tt <- subset(d_proj, year != tt)
    
    # add intercept
    d_proj_tt['(Intercept)'] <- 1
    
    # convert to matrix
    d_proj_tt_m <- as.matrix(d_proj_tt[ , names(coeff)])
    
    # project income growth rates 
    Y_g_tt <- as.vector(d_proj_tt_m %*% coeff + as.matrix(d_proj_tt$TF_g) + as.matrix(d_proj_tt$c_fe))
    
    # delete intercept
    d_proj_tt['(Intercept)'] <- NULL
    
    # add income growth projection to d_proj
    d_proj_tt$Y_g <- Y_g_tt
    d_proj <- rbind(d_proj_tt, d_proj_not_tt)
    
    # subset data for tt + 1 and not tt + 1
    d_proj_tt_p_1 <- subset(d_proj, year == tt + 5)
    d_proj_not_tt_p_1 <- subset(d_proj, year != tt + 5)
    
    # update d_proj_tt_p_1 with projections of variables dependent on Y_tt_p_1
    ## exclude project of first period
    d_proj_tt_p_1$Y <- d_proj_tt$Y * (1 + Y_g_tt)
    d_proj_tt_p_1$Y_o_L <- d_proj_tt_p_1$Y / d_proj_tt_p_1$pop_tot
    d_proj_tt_p_1$log_Y_o_L <- log(d_proj_tt_p_1$Y_o_L)
    d_proj_tt_p_1$Y_o_L_t_L_1__o_L <- d_proj_tt_p_1$Y_o_L * d_proj_tt_p_1$L_1__o_L
    d_proj_tt_p_1$Y_o_L_t_L_2__o_L <- d_proj_tt_p_1$Y_o_L * d_proj_tt_p_1$L_2__o_L
    d_proj_tt_p_1$Y_o_L_t_L_3__o_L <- d_proj_tt_p_1$Y_o_L * d_proj_tt_p_1$L_3__o_L
    d_proj_tt_p_1$log_Y_o_L_t_L_1__o_L <- d_proj_tt_p_1$log_Y_o_L * d_proj_tt_p_1$L_1__o_L
    d_proj_tt_p_1$log_Y_o_L_t_L_2__o_L <- d_proj_tt_p_1$log_Y_o_L * d_proj_tt_p_1$L_2__o_L
  
    
    # implement cut of for the effect for technology adaption
    d_proj_tt_p_1$log_Y_o_L_t_L_3__o_L <- ifelse(d_proj_tt_p_1$log_Y_o_L > log_Y_o_L_max,
                                                 log_Y_o_L_max * d_proj_tt_p_1$L_3__o_L,
                                                 d_proj_tt_p_1$log_Y_o_L * d_proj_tt_p_1$L_3__o_L)
    
    
    # add income projection to d_proj
    d_proj <- rbind(d_proj_tt_p_1, d_proj_not_tt_p_1)
    
  }
  
  # return
  return(d_proj) 
  
}



# smooth growth rates for first period with 2 period mean  ------------------------------------

smooth_p <- function(p) {
  
  
  # calculate mean growth
  tmp <- p %>%
    filter(year %in% c(2025, 2030)) %>%
    group_by(scenario, iso3c) %>%
    summarise(Y_g = mean(Y_g))
  
  # replace 2025
  p_2025 <- p %>%
    filter(year == 2025) %>%
    mutate(Y_g = tmp$Y_g)
  
  # other periods
  p_else <- p %>%
    filter(year != 2025)
  
  # combine data
  p <- rbind(p_2025, p_else)
  
  # recalculate economic variables
  p <- base_IWF(p)
  
  # increase smoothing period, i.e. 1 <~> 4, 2 <~> 3
  h_1 <- p
  h_1$year <- h_1$year - 5
  h_2 <- merge(p, h_1, c('scenario', 'iso3c', 'year'))
  h_2$YoL_g <- log(h_2$Y_o_L.y) - log(h_2$Y_o_L.x)
  h_3 <- unique(subset(h_2, YoL_g < 0 & year == 2025)$iso3c)
  
  # calculate mean growth
  tmp <- p %>%
    filter(year %in% c(2025, 2030, 2035) & iso3c %in% h_3) %>%
    group_by(scenario, iso3c) %>%
    summarise(Y_g = mean(Y_g))
  
  # replace 2025
  p_2025 <- p %>%
    filter(year == 2025 & iso3c %in% h_3) %>%
    mutate(Y_g = tmp$Y_g)
  
  # other periods
  p_else <- p %>%
    filter(!(year == 2025 & iso3c %in% h_3))
  
  # combine data
  p <- rbind(p_2025, p_else)
  
  # recalculate economic variables
  p <- base_IWF(p)
  
  # increase smoothing period, i.e. 1 <~> 4, 2 <~> 3
  h_1 <- p
  h_1$year <- h_1$year - 5
  h_2 <- merge(p, h_1, c('scenario', 'iso3c', 'year'))
  h_2$YoL_g <- log(h_2$Y_o_L.y) - log(h_2$Y_o_L.x)
  h_3 <- unique(subset(h_2, YoL_g < 0 & year == 2025)$iso3c)
  
  # calculate mean growth
  tmp <- p %>%
    filter(year %in% c(2025, 2030, 2035, 2040) & iso3c %in% h_3) %>%
    group_by(scenario, iso3c) %>%
    summarise(Y_g = mean(Y_g))
  
  # replace 2025
  p_2025 <- p %>%
    filter(year == 2025 & iso3c %in% h_3) %>%
    mutate(Y_g = tmp$Y_g)
  
  # other periods
  p_else <- p %>%
    filter(!(year == 2025 & iso3c %in% h_3))
  
  # combine data
  p <- rbind(p_2025, p_else)
  
  # recalculate economic variables
  p <- base_IWF(p)
  
  # return
  return(p)
  
}



# function to base projection on IWF forecast for 2025 --------------------

base_IWF <- function(p) {
  
  # read data
  p_iwf_2025 <- readRDS('output/IWF_forecast_2025.RDS')
  
  ## replace projections by IWF projections for 2025 (<~> 2020)
  p_2025 <- subset(p, year == 2025)
  p_else <- subset(p, year != 2025)
  p_2025 <- left_join(p_2025, p_iwf_2025, c('iso3c', 'year')) %>%
    mutate(Y = Y.y,
           Y_o_L = Y/pop_tot) %>%
  select(!c(Y.x, Y.y))
  
  p <- rbind(p_2025[names(p_else)], p_else)
  
  # iterate over time
  for (t in seq(2030, 2100, 5)) {
    
    p_tm1 <- subset(p, year == t-5)
    p_t <- subset(p, year == t)
    p_tm1 <- left_join(p_t, p_tm1, c('scenario', 'iso3c'))
    p_else <- subset(p, year != t)
    p_t$Y <- p_tm1$Y.y * (1 + p_tm1$Y_g.y)
    p_t$Y_o_L <- p_t$Y/p_t$pop_tot
    p_t$log_Y_o_L <- log(p_t$Y_o_L)
    p_t$Y_o_L_t_L_1__o_L <- p_t$Y_o_L * p_t$L_1__o_L
    p_t$Y_o_L_t_L_2__o_L <- p_t$Y_o_L * p_t$L_2__o_L
    p_t$Y_o_L_t_L_3__o_L <- p_t$Y_o_L * p_t$L_3__o_L
    p_t$log_Y_o_L_t_L_1__o_L <- p_t$log_Y_o_L * p_t$L_1__o_L
    p_t$log_Y_o_L_t_L_2__o_L <- p_t$log_Y_o_L * p_t$L_2__o_L
    p_t$log_Y_o_L_t_L_3__o_L <- p_t$log_Y_o_L * p_t$L_3__o_L
    
    p <- rbind(p_t, p_else)
    
    
  }
  
  
  # return
  return(p)
  
}