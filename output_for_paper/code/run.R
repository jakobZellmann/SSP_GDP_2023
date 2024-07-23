rm(list = ls())

# load data and packages --------------------------------------------------

# load packages
pacman::p_load(tidyverse, countrycode, RColorBrewer, stargazer)

# source auxilliary functions
source('code/auxilliary.R')

# load ssp projection data
d_ssp <- readRDS('output/d_ssp.RDS')

# read estimation data
d <- readRDS('input/estimation_data.RDS')

# read hdi projection
d_p_hdi <- read.csv('input/d_p_hdi.csv')



# estimation --------------------------------------------------------------


# define formulas
fs <- list(formula(Y_g ~ log_Y_o_L +  K_g + pop_tot_g + factor(iso3c) + factor(year)),
           formula(Y_g ~ log_Y_o_L + L_11_g + L_12_g + L_21_g + L_22_g + L_31_g + L_32_g + K_g + pop_tot_g + factor(iso3c) + factor(year)),
           formula(Y_g ~ log_Y_o_L + L_11_g + L_12_g + L_21_g + L_22_g + L_31_g + L_32_g + L_1__o_L + L_2__o_L + L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)),
           formula(Y_g ~ log_Y_o_L + L_11_g + L_12_g + L_21_g + L_22_g + L_31_g + L_32_g + L_1__o_L + L_2__o_L + L_3__o_L + log_Y_o_L_t_L_1__o_L + log_Y_o_L_t_L_2__o_L + log_Y_o_L_t_L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)),
           formula(Y_g ~ log_Y_o_L + L_21_g + L_12_g + L_1__o_L + L_3__o_L + log_Y_o_L_t_L_3__o_L + K_g + pop_tot_g + factor(iso3c) + factor(year)))

# estimation
est <- list()
for (i in 1:length(fs)) {
  
  est[[i]] <- lm(fs[[i]], d)
  
}

stargazer(est[[1]], est[[2]], est[[3]], est[[4]], est[[5]], keep = c(1:16))

# plots -------------------------------------------------------------------

# iterate over convergence speed - currently speed = 4
gg <- c("World", "Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")
for (g in gg) {
    
    # iterate over SSP scenarios
    SSPs <- c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')
    for (SSP in SSPs) {
      
      # plot level_iiasa_2013_vs_iiasa_2023
      pp <- p_level_iiasa_2013_vs_iiasa_2023(d_ssp, g, SSP)
      print(pp)
      ggsave(paste('output/plots/level_iiasa_2013_vs_iiasa_2023/', SSP, '_', g, '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      # plots growth
      pp <- p_g_iiasa_2023(d_ssp, SSP)
      print(pp)
      ggsave(paste('output/plots/g_iiasa_2023/', SSP, '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      # plot density
      pp <- p_density_iiasa_2023(d_ssp, SSP, 2020)
      print(pp)
      ggsave(paste('output/plots/density_iiasa_2023/', SSP, '_2020', '.pdf', sep = ''), width = 5, height = 4, units = "in")
      pp <- p_density_iiasa_2023(d_ssp, SSP, 2050)
      print(pp)
      ggsave(paste('output/plots/density_iiasa_2023/', SSP, '_2050', '.pdf', sep = ''), width = 5, height = 4, units = "in")
      pp <- p_density_iiasa_2023(d_ssp, SSP, 2100)
      print(pp)
      ggsave(paste('output/plots/density_iiasa_2023/', SSP, '_2100', '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      # convergence plots
      pp <- p_con_iiasa_2023(d_ssp, g, SSP)
      print(pp)
      ggsave(paste('output/plots/con_iiasa_2023/', SSP, '_', g, '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      
      # comparison plots IIASA 2013
      pp <- p_g_iiasa_2023_vs_iiasa_2013(d_ssp, SSP)
      print(pp)
      ggsave(paste('output/plots/g_iiasa_2023_vs_iiasa_2013/', SSP, '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      # comparison plots OECD
      pp <- p_g_iiasa_2013_vs_oecd_2023(d_ssp, SSP)
      print(pp)
      ggsave(paste('output/plots/g_iiasa_2013_vs_oecd_2023/', SSP, '.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      # hdi plots
      for (i in c('hdi', 'gdp', 'le', 'ed')) {
        
        pdf(paste('output/plots/density_hdi/', i, '_', SSP, '.pdf', sep = ''), width = 5, height = 4)
        p_density_hdi(d_p_hdi, SSP, i)
        dev.off()
        
      }
      
      # plot level_iiasa_2013_vs_iiasa_2023
      pp <- p_change_hdi()
      print(pp)
      ggsave(paste('output/plots/change_hdi/', 'change_hdi.pdf', sep = ''), width = 5, height = 4, units = "in")
      
      
    }
    
}
