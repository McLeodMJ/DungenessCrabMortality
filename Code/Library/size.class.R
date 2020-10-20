##################################################################
# Size Class function 
### function to subset from main dataframe based on size and sex
##################################################################

size.class <-function(data, sex, size){
  if(size == "juv"){
    sized_data <- subset(data, CW <= 139 & Sex == sex)
  }
  if(size == "sub"){
    sized_data <- subset(data, CW  %in% c(140:159) & Sex == sex)
  }
  if(size == "legal"){
    sized_data <- subset(data, CW  >= 160 & Sex == sex)
  }
  return(sized_data)
}
 