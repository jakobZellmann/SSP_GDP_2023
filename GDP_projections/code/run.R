rm(list = ls())

# source routines and packages --------------------------------------------------------

pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode)
source('code/auxilliary.R')



# projection --------------------------------------------------------------

# iterate over scenarios
SSPs <- c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')
p <- c()
for (SSP in SSPs) {
  
  ## project 
  tmp <- project(SSP)
  
  ## subset economic variables
  p <- rbind(p , tmp)
  
}


# base on IWF projections
p <- base_IWF(p)

# smooth growth for first periods
p <- smooth_p(p)



# write -------------------------------------------------------------------

saveRDS(p, 'output/SSP_IIASA_GDP_2023.RDS')

