#########################################################################################
# Size Class function 
### function to subset from main dataframe based on size and sex

#' data = datset to be filtered for size and sex 
#' sex = 1: males // 2: females
#' size = #juv: juveniles, sub: sub-adults, legal: legal-sized crabs, total: all crabs
##########################################################################################
 
size.class <-function(data, sex, size){
  # No sex characteristics 
  if (is.na(sex)){
    if(size == "juv"){
      sized_data <- subset(data, CW <= 139)
    }
    if(size == "sub"){
      sized_data <- subset(data, CW  %in% c(140:159))
    }
    if(size == "legal"){
      sized_data <- subset(data, CW  >= 160 )
    }
  }else{
  
    if(size == "juv"){
      sized_data <- subset(data, CW <= 139 & Sex == sex)
    }
    if(size == "sub"){
      sized_data <- subset(data, CW  %in% c(140:159) & Sex == sex)
    }
    if(size == "legal"){
      sized_data <- subset(data, CW  >= 160 & Sex == sex)
    }
    if(size == "total"){
      sized_data <- subset(data, Sex == sex)
    }
  }
 
  
  return(sized_data)
}
