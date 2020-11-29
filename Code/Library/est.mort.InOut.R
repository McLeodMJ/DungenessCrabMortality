#########################################################################################################
# Survival Molt function
## Data contains only years where there is both April and September data for male D.crab
## Proportion of cohort that goes into next size class is 100% for juvenile and subadult males
#### [did NOT included in calc given prop.=1]
# juv <140mm
# subadult 140-159mm
# legal >=160
### different molt probabilities for sub-adult class: 100-150mm AND  150-180mm 
#########################################################################################################

# Function for proportion of pop that molts from April --> September
surv.molt <- function(data, y, a){
  Ap_Sept_Data <- subset(data, Date == "2014-04-07" | Date == "2014-09-17"| Date == "2020-04-03" |Date == "2020-09-01" |Date == "2020-09-15")
  
  #create pre and post df for each size class 
  Pre_molt_juv <- subset(Ap_Sept_Data, CW <= 139 & Month == "April" & Year == y) #juveniles moving into sub-adults
  
  # sub-adults have to be spilt into two b/c of zhang LR calc
  Pre_molt_sub1 <- subset(Ap_Sept_Data, CW %in% c(140:149) & Month == "April"& Year == y)
  Pre_molt_sub2 <- subset(Ap_Sept_Data, CW %in% c(150:159) & Month == "April"& Year == y)
  
  Post_molt_sub <- subset(Ap_Sept_Data, CW %in% c(140:159) & Month == "September" & Year == y)
  
  molt.in = nrow(Pre_molt_juv) * mean(Pre_molt_juv$Molt.prob) 
  molt.out = (nrow(Pre_molt_sub1) * mean(Pre_molt_sub1$Molt.prob)) + (nrow(Pre_molt_sub2)* mean(Pre_molt_sub2$Molt.prob)) 
  survived <- (((nrow(Post_molt_sub) + molt.in - molt.out) * mean(Post_molt_sub$Effort))  / ((nrow(Pre_molt_sub1) + nrow(Pre_molt_sub2)) * mean(Pre_molt_sub1$Effort))/a)  # N(1)/N(0)
  return(survived)
}

# We have a lot more datapoints for september of 2020 which is why we have such a large 2020 value
#table(Ap_Sept_Data$Date) 


