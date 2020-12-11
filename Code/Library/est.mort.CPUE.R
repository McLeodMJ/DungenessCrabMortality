########################################################
# Estimating non-molting Natural Mortality using CPUE
## M = -1 / Tb - Ta * ln(CPUE(Tb) / CPUE(Ta))
###' limit: if the aggregated data should be for all sample dates (N - no dont limit)
###' y - Yes limit to just april and september as molt months
########################################################

est.mort.CPUE <- function(data, limit){
  # develop a calc across all data 
  if(limit == "N"){  
  data <- data %>% 
    group_by(Date, Month, Year, J.date) %>% 
    summarise(across(Soak.Time.hr : CPUE, mean))
  }
  
  # develop a calc. for dates of specific interst [molt season]
  if(limit == "Y"){
  data <- data %>% group_by(Date, Month, Year, J.date) %>% 
    summarise(across(Soak.Time.hr : CPUE, mean)) %>% 
    filter(month(Date) %in% c(4,9)) %>% 
    filter(!Date =="2013-09-13" & !Date == "2020-09-01")
  }
  
  # estimating mortality for samples in same year but different months
  data$N.Mort <- rep(NA, nrow(data)) #NULL column
  
  # I actually can't think of a tidyverse way to do this bit... 
  for(i in 2:nrow(data)){
    if(data$Year[i] == data$Year[i-1] & data$Month[i] != data$Month[i-1]){
      data$N.Mort[i] <- (-1/( (data$J.date[i]) - (data$J.date[i-1]) ) )* log( data$CPUE[i] / data$CPUE[i-1] )
    }
  }
  return(data)
}
