##################################################
# Total.Pots.Crabs Function 
### takes the dataframe in place and calculates:
# 1) total number of pots per sample
# 2) total number of crabs per sample 

#' data = full dataset
#' size = sub: only sub-adult males // total: all males 
#' sex = 1: male 2: female NA: when either
##################################################

total.pots.crabs <- function(data, sex, size){

# for no size limitations
  if(size == "total"){
    data.summary <- data %>% group_by(Date) %>% 
    summarise(Total.crabs = n(), Total.pots = max(Pot))
    # append the table to the main data table
    data <- inner_join(data, data.summary, by = 'Date')
    
    #now account for JUST the sub-adult males
    data <- size.class(data, sex, "total")
  }
  
  if(size == "sub"){
    data.summary <- data %>% group_by(Date) %>% 
      summarise(Total.pots = max(Pot))
    # append the table to the main data table
    data <- inner_join(data, data.summary, by = 'Date')
    
    #now account for JUST the sub-adult males
    sized_data <- size.class(data, sex, "sub")
  
  data.sub <- sized_data %>% group_by(Date) %>% 
    summarise(Total.crabs = n())
    # append the table to the main data table
    data <- inner_join(data, data.sub, by = 'Date')
  }
  
  return(data)
}
