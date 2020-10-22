########################################################
# Estimating non-molting Natural Mortality using CPUE
## M = -1 / Tb - Ta * ln(CPUE(Tb) / CPUE(Ta))
###' limit: if the aggregated data should be for all sample dates (N - no dont limit)
###' y - Yes limit to just april and september as molt months
########################################################

est.mort.CPUE <- function(source, data, limit){
  #develop a calc across all data 
  if(limit == "N"){
    data <- aggregate(cbind(Month, Year, J.date, Soak.Time.hr, Total.pots, Total.crabs, Effort, CPUE) ~ Date, data = source, mean) #reports 
    data$N.Mort <- rep(NA, nrow(data)) #NULL column
  }
  # develop a calc. for dates of specific interst [molt season]
  if(limit == "Y"){
    data <- aggregate(cbind(Month, Year, J.date, Soak.Time.hr, Total.pots, Total.crabs, Effort, CPUE) ~ Date, data = source, mean) #reports 
    data$N.Mort <- rep(NA, nrow(data))
    data <- subset(data, Month == "4" | Month == "9")
  }
  
  #estimating mortality for samples in same year but different months
    for(i in 2:nrow(data)){
      if(data$Year[i] == data$Year[i-1] & data$Month[i] != data$Month[i-1]){
       data$N.Mort[i] <- (-1/( (data$J.date[i]) - (data$J.date[i-1]) ) )* log( data$CPUE[i] / data$CPUE[i-1] )
      }
    }
    return(data)
  } 



  
## my attempt with tidyverse
#estimating mortality for samples in same year but different months
#for(i in 2:nrow(data)){
 # if(data$Year[i] == data$Year[i-1] & data$Month[i] != data$Month[i-1]){
  #  data <- data %>%
    #  mutate(N.Mort[i] <- (-1 / (J.date[i] - J.date[i-1])) * logCPUE[i]/ CPUE[i-1])
 # }

