##################################################
# CPUE Function
##' 1) Calculate the Julian date for each year
##' 2) Calculate standars soak time [ 1-(exp(-k * soak.time))/ (1 - exp(-k))]
#### 'k' was estimated in the zhang et al 2004 paper as a crab catch rate.. if every hour was as effective as the first.
##### equation 2 - zhang et al. 2004
##' 3) Calculate Effort [soak time * no. of pots]
##' 4) Calculate CPUE [Total crabs / effort]
##################################################

CPUE <- function(data){
  
  data <- data %>% mutate(J.date = yday(as_date(Date)))
  
  # estimating CPUE
  data <- data %>% 
    mutate(Std.time = (1 - exp(-0.19 * Soak.Time.hr) / (1-exp(-0.19)) )) %>% 
    mutate(Effort = Std.time * Total.pots) %>%
    mutate(CPUE = Total.crabs / Effort)
  
  return(data)
 }
