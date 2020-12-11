##################################################
# Total.Pots.Crabs Function 
### takes the dataframe in place and calculates:
# 1) total number of pots per sample
# 2) total number of crabs per sample 

#' data = full dataset
#' size = sub: only sub-adult males // total: all males 
##################################################

total.pots.crabs <- function(data, size){

# for no size limitations
  if(size == "total"){
    data.summary <- data %>% group_by(Date) %>% 
    summarise(Total.crabs = n(), Total.pots = max(Pot))
    # append the table to the main data table
    data <- inner_join(data, data.summary, by = 'Date')
    
    #now account for JUST the sub-adult males
    data <- size.class(data, 1, "total")
  }
  
  if(size == "sub"){
    data.summary <- data %>% group_by(Date) %>% 
      summarise(Total.pots = max(Pot))
    # append the table to the main data table
    data <- inner_join(data, data.summary, by = 'Date')
    
    #now account for JUST the sub-adult males
    Sub_males <- size.class(data, 1, "sub")
  
  data.sub <- Sub_males %>% group_by(Date) %>% 
    summarise(Total.crabs = n())
    # append the table to the main data table
    data <- inner_join(Sub_males, data.sub, by = 'Date')
  }
  
  return(data)
}
