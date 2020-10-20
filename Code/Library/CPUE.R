##################################################
# CPUE Function
##' 1) Calculate the Julian date for each year
##' 2) Calculate Effort [soak time * no. of pots]
##' 3) Calculate CPUE [Total crabs / effort]
##################################################

CPUE <- function(data){
  
  # Making a Julian date by the year 
  data$J.date <- julian.Date(data$Date, as.Date("2013-01-01"), by= 'Year' )
  for (i in 1:nrow(data)){
    if(data$Year[i] != 2013) {
      n = data$Year[i] - 2013;
      data$J.date[i] <- data$J.date[i] - (365 * n) 
    }
  }
  # estimating CPUE
   data <- data %>% 
     mutate(Effort = Soak.Time.hr * Total.pots) %>%
     mutate(CPUE = Total.crabs / Effort)
}

