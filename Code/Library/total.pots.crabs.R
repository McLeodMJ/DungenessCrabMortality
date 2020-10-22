##################################################
# Total.Pots.Crabs Function 
### takes the dataframe in place and calculates:
# 1) total number of pots per sample
# 2) total number of crabs per sample 
##################################################

total.pots.crabs <- function(data){
  #When there is a different sample date (i.e. sample), records total pots used and number of crabs collected 
  data$Total.pots <- rep(NA,nrow(data))
  data$Total.crabs <- rep(NA,nrow(data))
  
  j=1
  for(i in 2:nrow(data)){
    if(data$Date[i] !=  data$Date[i-1])
    { data$Total.pots[j:i] <- data$Pot[i-1];
    data$Total.crabs[j:i] <- (i-j);
    j=i}
    
    if(i == nrow(data)) 
    {data$Total.pots[j:i] <- data$Pot[i];
    data$Total.crabs[j:i] <- i-j +1}
  }
  return(data)
}

 
 ## attempt at tidyverse 
 ### need to fix it so that all the columns in select are showing
 #data <- data %>% 
  # select(Date, Month, Year, Sex, CW, Pot, Soak.Time.hr) %>% 
   #group_by(Date) %>% 
   #summarise(Total.Pots = max(Pot)) %>%
   #arrange(Date, Month, Year, Sex, CW, Pot, Soak.Time.hr, Total.Pots)
