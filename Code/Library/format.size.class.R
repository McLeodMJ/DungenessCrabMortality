# fxn for setting up crab df to be in standarzied format of mu-juv, mu-subs, mu-legals


format.size.class <- function(dat_total, shell){
  # NO shell-condition data avail.
  if(is.na(shell)){ 
    Pot_data <- dat_total %>% 
      group_by(Date, Month, Year, Pot) %>% 
      summarise()
  }
  # for shell-condition based analysis
  if(!is.na(shell)){ 
    Pot_data <- dat_total %>% 
      group_by(Date, Month, Year, ShellCondition, Pot) %>% 
      summarise()
  }
  
  
 # Pot_data <- left_join(Pot_data, Delta.T, by=c('Date','Month', 'Year'))
  ##counts the number of juveniles per pot & takes mean 
  # juvs <- Molt_szn_data %>% 
  #     group_by(Date, Month, Year, Pot) %>% 
  #   summarise(Mu_juv = count(CW < 140))
  
  juvs <- dat_total %>% 
    filter( CW < 140) %>% 
    group_by(Date, Month, Pot, Year) %>% 
    summarise(Molt.prb_juv = mean(Molt.prob),
              Mu_juv = n())
  
  #juvs <- left_join(juvs, juvs2, by=c('Date','Month', 'Year', 'Pot'))
  #juvs$Molt.prb_juv <- ifelse(is.na(juvs$Molt.prb_juv), 0, juvs$Molt.prb_juv)
  
  
  ##counts the number of sub-legals [btw 140:149] per pot & takes mean 
  subs1 <- dat_total %>% 
    filter( CW %in% 140:149) %>% 
    group_by(Date, Month, Year, Pot) %>% 
    summarise(Molt.prb_sub1= mean(Molt.prob),
              Mu_sub1 = n())
  
  
  ##counts the number of sub-legals [btw 140:149] per pot & takes mean 
  subs2 <- dat_total %>% 
    filter( CW %in% 150:159) %>% 
    group_by(Date, Month, Year, Pot) %>% 
    summarise(Molt.prb_sub2 = mean(Molt.prob),
              Mu_sub2 = n())
  
  
  ##counts the number of sub-legals per pot & takes mean 
  legals <- dat_total %>% 
    filter( CW > 159) %>% 
    group_by(Date, Month, Year, Pot) %>% 
    summarise(Molt.prb_legal = mean(Molt.prob),
              Mu_legal = n())
  
  
  #merge data
  Pot_data <- left_join(Pot_data, juvs, by=c('Date','Month', 'Year', 'Pot'))
  Pot_data <- left_join(Pot_data, subs1, by=c('Date','Month', 'Year', 'Pot'))
  Pot_data <- left_join(Pot_data, subs2, by=c('Date','Month', 'Year', 'Pot'))
  Pot_data <- left_join(Pot_data, legals, by=c('Date','Month', 'Year', 'Pot'))
  #Pot_data[is.na(Pot_data)] <- 0 
  # if we do not want the molt.prob to be zero & instead excluded, keep as na and just change sum to include na.rm = 0 for molt.in/out
  
  return(Pot_data)
}