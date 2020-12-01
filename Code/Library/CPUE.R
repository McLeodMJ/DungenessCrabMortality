##################################################
# CPUE Function
##' 1) Calculate the Julian date for each year
##' 2) Calculate Effort [soak time * no. of pots]
##' 3) Calculate CPUE [Total crabs / effort]
##################################################

CPUE <- function(data){
  
  data <- data %>% mutate(J.date = yday(as_date(Date)))
  
  
  # estimating CPUE
  data <- data %>% 
    mutate(Effort = Soak.Time.hr * Total.pots) %>%
    mutate(CPUE = Total.crabs / Effort)
}
