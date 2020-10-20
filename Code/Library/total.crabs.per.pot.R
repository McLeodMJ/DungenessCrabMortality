##################################################
# Total.Crabs.Per.Pot Function 
###  total number of crabs per pot
##################################################


total.crabs.per.pot <- function (data){
  Totals <- data %>% 
    select(Date, Month, Year, Pot, Total.pots, Total.crabs) %>% 
    group_by(Date, Month, Year, Pot, Total.pots, Total.crabs) %>% 
    count(Pot)
  colnames(Totals)[colnames( Totals) == "n"]  <- "Crabs.per.pot"
  Totals <- as.data.frame(Totals)
  return(Totals)
}