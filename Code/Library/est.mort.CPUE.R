########################################################
# Estimating non-molting Natural Mortality using CPUE
## M = -1 / Tb - Ta * ln(CPUE(Tb) / CPUE(Ta))
###' limit: "N" -if the aggregated data should be for all sample dates (N - no dont limit)
###' limit: "y" - Yes limit to just april and september as molt months
###' limit : "shell" - then should process data based on shell condition months 
###' pre_molt: vector of months that are pre-molt time frame (NA if limit is not "shell")
###' post_molt: vector of months for post-molt (NA if limit is not "shell")
########################################################

est.mort.CPUE <- function(data, limit, pre_molt, Post_molt){
  # develop a calc across all data 
  if(limit == "N"){  
  data <- data %>% 
    group_by(Date, Month, Year, J.date) %>% 
    summarise(across(Soak.Time.hr : CPUE, mean))
  
  # estimating mortality for samples in same year but different MONTHS
  data$N.Mort <- rep(NA, nrow(data)) 
  # I actually can't think of a tidyverse way to do this bit... 
    for(i in 2:nrow(data)){
      if(data$Year[i] == data$Year[i-1] & data$Month[i] != data$Month[i-1]){
        data$N.Mort[i] <- (-1/( (data$J.date[i]) - (data$J.date[i-1]) ) )* log( data$CPUE[i] / data$CPUE[i-1] )
      }
    }
  }#end of limit = No
  
  
  # develop a calc. for dates of specific interst [molt season]
  if(limit == "Y"){
  data <- data %>% group_by(Date, Month, Year, J.date) %>% 
    summarise(across(Soak.Time.hr : CPUE, mean)) %>% 
    filter(month(Date) %in% c(4,9)) %>% 
    filter(!Date =="2013-09-13" & !Date == "2020-09-01")
  
  # estimating mortality for samples in same year but different MONTHS
  data$N.Mort <- rep(NA, nrow(data)) 
    for(i in 2:nrow(data)){
      if(data$Year[i] == data$Year[i-1] & data$Month[i] != data$Month[i-1]){
        data$N.Mort[i] <- (-1/( (data$J.date[i]) - (data$J.date[i-1]) ) )* log( data$CPUE[i] / data$CPUE[i-1] )
      }
    }
  } #end of limit = yes
  
  
  if(limit == "shell"){
    data <- data %>% 
      relocate(Site, Date, Month, Year, J.date, Std.time, Effort, CPUE)
    
    # pre-molt df
    prior <- data %>% group_by(Site, Year) %>% 
      filter(Month %in% c(pre_molt)) %>%
      summarise(across(J.date: CPUE, mean))  %>% 
      mutate(Stage = "prior")
      
    #post-molt df
    post <- data %>% group_by(Site, Year) %>% 
      filter(Month %in% c(post_molt))  %>% 
      summarise(across(J.date: CPUE, mean)) %>% 
      mutate(Stage = "post")
    
    zata <- data %>% group_by(Site, Year)  %>% 
      summarise() 
      
    zata <- left_join(zata, prior, by=c('Site', 'Year'))
    zata <- rbind(zata, post)
    data <- na.omit(zata) # remove years when nothing present
    data <- data[order(data$Year),] #order df by year
    
    # estimating mortality for samples in same year but different SHELL CONDITION
    data$N.Mort <- rep(NA, nrow(data)) 
    
    for(i in 2:nrow(data)){
      if(data$Year[i] == data$Year[i-1] & data$Stage[i] != data$Stage[i-1]){
        data$N.Mort[i] <- (-1/( (data$J.date[i]) - (data$J.date[i-1]) ) )* log( data$CPUE[i] / data$CPUE[i-1] )
      }
    }
  }# end of limit = shell
  
  return(data)
}
