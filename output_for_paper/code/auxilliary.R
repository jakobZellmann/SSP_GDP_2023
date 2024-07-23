p_level_iiasa_2013_vs_iiasa_2023 <- function(p, g, SSP) {
  
  p <- na.omit(subset(p, scen == SSP, c(scen, iso3c, year, iiasa_2023_gdp, iiasa_2013_gdp, iiasa_wic_2023_pop, iiasa_wic_2013_pop, in_cla, iiasa_2023_gdp_pc, iiasa_2013_gdp_pc)))
  
  if (g %in% c("Lower Middle Income", "Upper Middle Income", "High Income", "Low Income")) {
    
    pp <- p %>%
      group_by(in_cla, year) %>%
      summarise(iiasa_2013_gdp = sum(iiasa_2013_gdp), iiasa_2023_gdp = sum(iiasa_2023_gdp), iiasa_wic_2023_pop_tot = sum(iiasa_wic_2023_pop), iiasa_wic_2013_pop_tot = sum(iiasa_wic_2013_pop)) %>%
      mutate(iiasa_2023_gdp_pc = iiasa_2023_gdp/iiasa_wic_2023_pop_tot, iiasa_2013_gdp_pc = iiasa_2013_gdp/iiasa_wic_2013_pop_tot) %>%
      filter(in_cla == g)
    
  } else if (g == 'World') {
    
    pp <- p %>%
      group_by(year) %>%
      summarise(iiasa_2013_gdp = sum(iiasa_2013_gdp), iiasa_2023_gdp = sum(iiasa_2023_gdp), iiasa_wic_2023_pop_tot = sum(iiasa_wic_2023_pop), iiasa_wic_2013_pop_tot = sum(iiasa_wic_2013_pop)) %>%
      mutate(iiasa_2023_gdp_pc = iiasa_2023_gdp/iiasa_wic_2023_pop_tot, iiasa_2013_gdp_pc = iiasa_2013_gdp/iiasa_wic_2013_pop_tot)
  
    
  } else {
    
    pp <- p %>%
      filter(iso3c == g)
    
    
  }
  
  # plot
  ggplot(pp, aes(x=year)) + 
    geom_line(aes(y = iiasa_2023_gdp_pc, color = "IIASA (2023) GDP per capita")) + 
    geom_line(aes(y = iiasa_2013_gdp_pc, color= "IIASA (2013) GDP per capita")) +
    scale_color_manual(name = "Income per Capita \n (USD2017)", breaks = c("IIASA (2023) GDP per capita", "IIASA (2013) GDP per capita"), values = c("darkred", "steelblue")) + 
    ylab("GDP per Capita \n (in USD2017)")
  
}



p_g_iiasa_2023 <- function(p, SSP) {
  
  # g \in {High Income, Upper Middle Income, Lower Middle Income, Low Income, World, iso3c}
  # growth \in {T, F}
  
  
  # subset data till 2095 and omit NAs in current projections
  p <- na.omit(subset(p, scen == SSP, c(scen, iso3c, year, iiasa_2023_gdp, iiasa_wic_2023_pop, in_cla)))
  
  # aggregate by income group
  tmp_1_0 <- p %>%
    group_by(in_cla, year) %>%
    summarise(iiasa_2023_gdp = sum(iiasa_2023_gdp), iiasa_wic_2023_pop_tot = sum(iiasa_wic_2023_pop)) %>%
    mutate(iiasa_2023_gdp_pc = iiasa_2023_gdp/iiasa_wic_2023_pop_tot)
  
  
  # shift time
  tmp_1_1 <- tmp_1_0 %>%
    mutate(year = year - 5)
  
  # calculate growth
  tmp_1 <- tmp_1_0 %>%
    left_join(., tmp_1_1, by = c('in_cla', 'year')) %>%
    mutate(iiasa_2023_gdp_pc_g = log(iiasa_2023_gdp_pc.y) - log(iiasa_2023_gdp_pc.x)) %>%
    select(in_cla, year, iiasa_2023_gdp_pc_g) %>%
    pivot_wider(names_from = in_cla, values_from = iiasa_2023_gdp_pc_g)
  
  
  # aggregate for world
  tmp_2_0 <- p %>%
    group_by(year) %>%
    summarise(iiasa_2023_gdp = sum(iiasa_2023_gdp), iiasa_wic_2023_pop_tot = sum(iiasa_wic_2023_pop)) %>%
    mutate(iiasa_2023_gdp_pc = iiasa_2023_gdp/iiasa_wic_2023_pop_tot)
  
  # shift time
  tmp_2_1 <- tmp_2_0 %>%
    mutate(year = year - 5)
  
  # calculate growth
  tmp_2 <- tmp_2_0 %>%
    left_join(., tmp_2_1, by = 'year') %>%
    mutate(iiasa_2023_gdp_pc_g = log(iiasa_2023_gdp_pc.y) - log(iiasa_2023_gdp_pc.x)) %>%
    select(year, iiasa_2023_gdp_pc_g)
  
  # join world and income classes
  pp <- left_join(tmp_1, tmp_2, 'year') %>%
    rename(World = iiasa_2023_gdp_pc_g)
  
  # colors
  col <- brewer.pal(5, 'Set3')
  
  # plot
  ggplot(pp, aes(x=year)) + 
    geom_line(aes(y = `Low Income`, color = "Low Income")) + 
    geom_line(aes(y = `Lower Middle Income`, color="Lower Middle Income")) +
    geom_line(aes(y = `Upper Middle Income`, color="Upper Middle Income")) +
    geom_line(aes(y = `High Income`, color="High Income")) +
    geom_line(aes(y = World, color="World")) +
    scale_color_manual(name = "Income per Capita Growth \n (USD2017)", breaks = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "World"), values = col) + 
    ylab("Growth in GDP per Capita")
  
  
}


p_con_iiasa_2023 <- function(p, g, SSP) {
  
  # subset data 2025
  p_2025 <- p %>%
    filter(scen == SSP & year == 2025) %>%
    select(scen, iso3c, year, iiasa_2023_gdp_pc, in_cla) %>%
    na.omit()
  
  # subset data 2100
  p_2100 <- p %>%
    filter(scen == SSP & year == 2100) %>%
    select(scen, iso3c, year, iiasa_2023_gdp_pc, in_cla) %>%
    na.omit()
  
  
  
  # g \in {High Income, Upper Middle Income, Lower Middle Income, Low Income, World, iso3c}
  if (g %in% c("Lower Middle Income", "Upper Middle Income", "High Income", "Low Income")) {
    
    pp <- p %>%
      filter(in_cla == g)
    
  } else if (g == 'World') {
    
    pp <- p 
    
  }
  
  
  # calculate average growth
  dp <- left_join(p_2100, p_2025, 'iso3c') %>%
    mutate(ave_g = log(iiasa_2023_gdp_pc.x) - log(iiasa_2023_gdp_pc.y),
           log_initial_gdp_pc = log(iiasa_2023_gdp_pc.y)) %>%
    select(iso3c, ave_g, log_initial_gdp_pc)
  
  
  # plot
  ggplot(dp, aes(x = log_initial_gdp_pc, y = ave_g)) + 
    geom_smooth(method=lm, se=FALSE) + 
    geom_text(
      label= dp$iso3c
    ) +
    ylab("log(GDP per Capital, 2100) - log(GDP per Capital, 2025)") + 
    xlab("log(GDP per Capital, 2025)")
  
}


p_density_iiasa_2023 <- function(p, SSP, y) {
  
  
  if (y == 2020) {
    
    # read estimation data
    p <- readRDS('input/estimation_data.RDS')
    
    # world bank income classification
    in_cla <- readRDS('output/in_cla.RDS')
    
    # add income classes
    p <- left_join(p, in_cla, 'iso3c')
    
    # increase year
    p$year <- p$year + 5
    
    # subset
    p <- subset(p, year == y)
    
    # add new variables
    p$log_Y_o_L_low <- ifelse(p$in_cla == 'Low Income',
                              log(p$Y_o_L * 1000),
                              NA)
    p$log_Y_o_L_middle_high <- ifelse(p$in_cla != 'Low Income',
                                      log(p$Y_o_L * 1000),
                                      NA)
    
  } else {
    
    
    # subset
    p <- subset(p, year == y & scen == SSP)
    
    # add new variables
    p$log_Y_o_L_low <- ifelse(p$in_cla == 'Low Income',
                              log(p$iiasa_2023_gdp_pc),
                              NA)
    p$log_Y_o_L_middle_high <- ifelse(p$in_cla != 'Low Income',
                                      log(p$iiasa_2023_gdp_pc),
                                      NA)
    
  }
  
  
  plot <- ggplot(p, aes(x = x) ) +
    
    # Top
    geom_density( aes(x = log_Y_o_L_middle_high, y = after_stat(density)), fill="#69b3a2" ) +
    geom_label( aes(x=6, y=0.25, label="Middle and High Income"), color="#69b3a2") +
    xlim(4, 20)+
    
    # Bottom
    geom_density( aes(x = log_Y_o_L_low, y = -after_stat(density)), fill= "#404080") +
    geom_label( aes(x=6, y=-0.25, label="Low Income"), color="#404080") +
    xlab("log(GDP per Capita)") +
    xlim(4, 20)
  
  return(print(plot))
  
}


p_g_iiasa_2023_vs_iiasa_2013 <- function(p, SSP) {
  
  # subset for scenario
  p <- na.omit(subset(p, scen == SSP, c(iso3c, year, iiasa_2023_gdp_pc, iiasa_2013_gdp_pc)))
  
  # subset
  p_2025 <- subset(p, year == 2025)
  p_2100 <- subset(p, year == 2100)
  
  # merge
  p <- left_join(p_2100, p_2025, by = c('iso3c'))
  
  # data for plot
  d_p <- na.omit(data.frame(iso3c = p$iso3c, iiasa_2023_gdp_pc_g = log(p$iiasa_2023_gdp_pc.x) - log(p$iiasa_2023_gdp_pc.y), iiasa_2013_gdp_pc_g = log(p$iiasa_2013_gdp_pc.x) - log(p$iiasa_2013_gdp_pc.y)))
  
  # plot
  ggplot(d_p, aes(x = iiasa_2023_gdp_pc_g, y = iiasa_2013_gdp_pc_g)) + 
    # geom_smooth(method=lm, se=FALSE) + 
    geom_text(
      label= d_p$iso3c
    ) +
    geom_abline(intercept = 0, slope = 1, size = 1, col = 'blue') +
    # xlim(0, 4) +
    # ylim(0, 4) + 
    ylab("Income growth, 2025-2100, IIASA 2013") + 
    # xlab("IIASA 2023 ln(GDP per Capita, 2100) - ln(GDP per Capita, 2025)")
    xlab("Income growth, 2025-2100, IIASA 2023")
  
}




p_g_iiasa_2013_vs_oecd_2023 <- function(p, SSP) {
  
  # subset for scenario
  p <- na.omit(subset(p, scen == SSP, c(iso3c, year, iiasa_2023_gdp_pc, oecd_2023_gdp_pc)))
  
  # subset
  p_2025 <- subset(p, year == 2025)
  p_2100 <- subset(p, year == 2100)
  
  # merge
  p <- left_join(p_2100, p_2025, by = c('iso3c'))
  
  # data for plot
  d_p <- na.omit(data.frame(iso3c = p$iso3c, iiasa_2023_gdp_pc_g = log(p$iiasa_2023_gdp_pc.x) - log(p$iiasa_2023_gdp_pc.y), oecd_2023_gdp_pc_g = log(p$oecd_2023_gdp_pc.x) - log(p$oecd_2023_gdp_pc.y)))
  
  # plot
  ggplot(d_p, aes(x = iiasa_2023_gdp_pc_g, y = oecd_2023_gdp_pc_g)) + 
    # geom_smooth(method=lm, se=FALSE) + 
    geom_text(
      label= d_p$iso3c
    ) +
    geom_abline(intercept = 0, slope = 1, size = 1, col = 'blue') +
    # xlim(0, 4) +
    # ylim(0, 4) + 
    ylab("Income growth, 2025-2100, OECD 2023") + 
    # xlab("IIASA 2023 ln(GDP per Capita, 2100) - ln(GDP per Capita, 2025)")
    xlab("Income growth, 2025-2100, IIASA 2023")
  
}


p_density_hdi <- function(d_p_hdi, SSP, i) {
  
  # calc quantils 
  p <- d_p_hdi %>%
    filter(scen == SSP) %>%
    group_by(scen, year) %>%
    summarise(i_gdp_pc_q0975 = quantile(i_gdp_pc, 0.975),
              i_le_q0975 = quantile(i_le, 0.975),
              i_ed_q0975 = quantile(i_ed, 0.975),
              i_hdi_q0975 = quantile(hdi, 0.975),
              
              i_gdp_pc_q0950 = quantile(i_gdp_pc, 0.95),
              i_le_q0950 = quantile(i_le, 0.95),
              i_ed_q0950 = quantile(i_ed, 0.95),
              i_hdi_q0950 = quantile(hdi, 0.95),
              
              i_gdp_pc_q0800 = quantile(i_gdp_pc, 0.8),
              i_le_q0800 = quantile(i_le, 0.8),
              i_ed_q0800 = quantile(i_ed, 0.8),
              i_hdi_q0800 = quantile(hdi, 0.8),
              
              i_gdp_pc_q0500 = quantile(i_gdp_pc, 0.5),
              i_le_q0500 = quantile(i_le, 0.5),
              i_ed_q0500 = quantile(i_ed, 0.5),
              i_hdi_q0500 = quantile(hdi, 0.5),
              
              i_gdp_pc_q0200 = quantile(i_gdp_pc, 0.2),
              i_le_q0200 = quantile(i_le, 0.2),
              i_ed_q0200 = quantile(i_ed, 0.2),
              i_hdi_q0200 = quantile(hdi, 0.2),
              
              i_gdp_pc_q0050 = quantile(i_gdp_pc, 0.05),
              i_le_q0050 = quantile(i_le, 0.05),
              i_ed_q0050 = quantile(i_ed, 0.05),
              i_hdi_q0050 = quantile(hdi, 0.05),
              
              i_gdp_pc_q0025 = quantile(i_gdp_pc, 0.025),
              i_le_q0025 = quantile(i_le, 0.025),
              i_ed_q0025 = quantile(i_ed, 0.025),
              i_hdi_q0025 = quantile(hdi, 0.025))
  
 # years expectancy
 year <- unique(p$year)
 # labels
 ylabs <- c(hdi = "Human Development Index",
           gdp = "GDP per Capita Index",
           le = "Life Expectancy Index",
           ed = "Education Index")
 
 # data for plots
 p <- p[,str_detect(names(p), i)]
 # get colors  
 cols = brewer.pal(5, 'Blues')
 # plot env
 plot(year, unlist(p[,4]), ylim=c(0,1), ylab=ylabs[i], xlab="Year", type = 'n')
 # grid
 grid(nx = NULL, ny = NULL,
      lty = 2,      # Grid line type
      col = "black", # Grid line color
      lwd = 1) 
 # 95%
 polygon(c(year, rev(year)),c(unlist(p[,1]), rev(unlist(p[,7]))), col = cols[2], border='navyblue')  #This function creates the CI bands
 # 90%
 polygon(c(year, rev(year)),c(unlist(p[,2]), rev(unlist(p[,6]))), col = cols[4], border='navyblue')  #This function creates the CI bands
 # 80%
 polygon(c(year, rev(year)),c(unlist(p[,3]), rev(unlist(p[,5]))), col = cols[5], border='navyblue')  #This function creates the CI bands
 # median
 lines(year, unlist(p[4]), ylim=c(0,1), col = 'darkred', lty=5, lwd = 1.5, ylab=ylabs[i], xlab="Year")
 # store
 pl <- recordPlot()
 #return
 return(pl)
 
}


p_change_hdi <- function(){
  
  d_p_hdi_2025 <- subset(d_p_hdi, year == 2025)
  d_p_hdi_2080 <- subset(d_p_hdi, year == 2080)
  d_p_hdi_c <- left_join(d_p_hdi_2080, d_p_hdi_2025, by = c('scen', 'iso3c'))
  d_p_hdi_c$hdi_c <- d_p_hdi_c$hdi.x - d_p_hdi_c$hdi.y
  d_p_hdi_c$Scenario <- d_p_hdi_c$scen
  ggplot(d_p_hdi_c, aes(y = hdi_c, x = hdi.y, colour = Scenario, shape = Scenario)) +
    geom_point() +
    geom_smooth(method = "lm", fill = NA) +
    xlab("Human Development Index, 2025") +
    ylab("HDI, 2080 - HDI, 2025")

  }  
