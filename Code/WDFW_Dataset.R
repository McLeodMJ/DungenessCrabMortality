# Dungeness Crab data WDFW
setwd("~/Documents/THESIS/Dungeness_crab/DungenessCrabMortality/Code")
source("./Library/Total.Pots.Crabs.R")
source("./Library/Total.Crabs.Per.Pot.R")
source("./Library/CPUE.R")
source("./Library/est.mort.InOut.R")
source("./Library/molt.prob.R")
source("./Library/size.class.R")

require(ggplot2)
library(lubridate)
library(tidyverse)

WDFW_Crabs <-readRDS(file= "/Users/montanamcleod/Downloads/West PS WDFW Crab Test Fishery - 1999-2019.rds")

WDFW_Crabs$Date <- as.Date(WDFW_Crabs$SetDateTime,format= "%Y-%m-&d")
# adjust date
WDFW_Crabs <- WDFW_Crabs %>% 
  mutate(Month = factor(
    month(Date, label = FALSE),   # thing you're converting
    1:12,                                  # values it could take
    labels =                               # how they should appear
      c("January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"),
    ordered = TRUE)) #Sets the Month to automatically order via month order [lubridate package]
WDFW_Crabs$Year <- as.numeric( format(WDFW_Crabs$Date, format="%Y") )

# No crabs collected in pot
 WDFW_Crabs<- WDFW_Crabs[complete.cases(WDFW_Crabs[ ,c(1:7, 13:18)]), ] # remove rows that have NA except for rows that have na in sex, shell cond, & width
# 120 removed 
 

WDFW_pots <- WDFW_Crabs[, c(1, 16:18, 5:6, 15, 8:10, 12)]
colnames(WDFW_pots)[colnames(WDFW_pots) == "Width_mm"] <- "CW"


#Reorder data 
levels(WDFW_pots$ShellCondition) = 1:8 # 1 = 1.1m premolt - 8 newly molted
index <- with(WDFW_pots, order(Site, Date, PotSetNumber)) #organizes by month then year
WDFW_pots <- WDFW_pots[index,]

# remove rows that have NA except for rows that have NA in shell cond & CW
WDFW_pots <- WDFW_pots[complete.cases(WDFW_pots[ ,1:8]), ] #317 pots do NOT have crabs present (967 do not have )

### these following have false zeros - currupted data 
  wdfw_test <- which(WDFW_pots$Date == "2020-06-01" & WDFW_pots$Site == "Tahuya")
# removed them in a weird way b.c brain doesnt work 
  WDFW_pots[wdfw_test, ] <- rep(NA, ncol(WDFW_pots))
  WDFW_pots <- WDFW_pots[complete.cases(WDFW_pots[ ,1:8]), ] 
    #in 1999 some years they just measured one male crab per pot and counted/sexed remainder in pot - over half of them so removed alll of 1999
    WDFW_pots <- WDFW_pots[WDFW_pots$Year != 1999, ] 


# trying to sus out pot numbers
WDFW_pots$Pot <- rep(NA, nrow(WDFW_pots))
#WDFW_pots <- WDFW_pots[, c(1:11, 13, 12)]
sites <- na.omit(unique(WDFW_pots$Site))

### cycle through data to get a pot number for each site - per sample date 
for(s in sites){
   dat <- WDFW_pots[WDFW_pots$Site == s, ] # calls the site
   dates <- unique(dat$Date)
   
   k = 1
   for (d in 2:nrow(dat)){
    
     # maintain pot for Same sample date
     if(dat$Date[d-1] == dat$Date[d] & dat$PotSetNumber[d-1] == dat$PotSetNumber[d]){
       dat$Pot[d-1] <- k 
     }
     # when same sample date BUT pot number changes add one 
     if (dat$Date[d-1] == dat$Date[d] & dat$PotSetNumber[d-1] != dat$PotSetNumber[d]){
       dat$Pot[d-1] <- k
       k = k + 1 
     }
     # we have one weirdo that has same date and potsetnumber
     if (dat$Date[d-1] != dat$Date[d] & dat$PotSetNumber[d-1] == dat$PotSetNumber[d]){
       dat$Pot[d-1] <- k
       k = 1 
     }
     # When new sample date
     if(dat$Date[d-1] != dat$Date[d] & dat$PotSetNumber[d-1] != dat$PotSetNumber[d]){
       dat$Pot[d-1] <- k 
       k=1
     }
     # break at end of site's dataset
     if(d == nrow(dat)){
       dat$Pot[d] <- k 
       break
     }
   }
   WDFW_pots[WDFW_pots$Site == s, 12 ] <- dat$Pot
}
## JUNE 2017 THEY PUT OUT AS MUCH AS 18 pots AT EACH SITE

# give a zero when no crab presnt
WDFW_pots$Crabs.per.Pot <- rep(NA, nrow(WDFW_pots))
for(p in 2: nrow(WDFW_pots)){
  # all NAs where they need to be but different pot before and after datapt
 if(WDFW_pots$Pot[p] != WDFW_pots$Pot[p-1] & WDFW_pots$Pot[p] != WDFW_pots$Pot[p+1] & is.na(WDFW_pots$CW[p]) & is.na(WDFW_pots$ShellCondition[p]) & is.na(WDFW_pots$Sex[p]) ){
   WDFW_pots$Crabs.per.Pot[p] <- 0
 } 
  if(is.na(WDFW_pots$CW[p]) & is.na(WDFW_pots$ShellCondition[p]) & is.na(WDFW_pots$Sex[p]) & !is.na(WDFW_pots$CrabComments[p]) & WDFW_pots$Pot[p] == WDFW_pots$Pot[p-1] & WDFW_pots$Pot[p] != WDFW_pots$Pot[p+1] ){
    WDFW_pots$Crabs.per.Pot[p] <- 0
  }
  #need to separate but may be able to fix
  if(is.na(WDFW_pots$CW[p]) & is.na(WDFW_pots$ShellCondition[p]) & is.na(WDFW_pots$Sex[p]) & !is.na(WDFW_pots$CrabComments[p]) & WDFW_pots$Pot[p] != WDFW_pots$Pot[p-1] & WDFW_pots$Pot[p] == WDFW_pots$Pot[p+1] ){
    WDFW_pots$Crabs.per.Pot[p] <- 0
  }
  if(p == nrow(WDFW_pots)){
    break }
}
# count max pots per site/sample
WDFW_pots1 <- WDFW_pots
# total pots per sample date & total crabs per sample date
data.summary <- WDFW_pots1 %>% 
      group_by(Date, Site) %>% 
      summarise( Total.pots = max(Pot))
  # append the table to the main data table
  WDFW_pots1 <- inner_join(WDFW_pots1, data.summary, by = c('Date', 'Site' ))
  
  #extract only males
  WDFW_Males <- WDFW_pots1[WDFW_pots1$Sex == "MALE", ]
  
  # molting probabilty
 WDFW_Males$Molt.prob <- rep(NA, nrow(WDFW_Males)) #null column
    
    for(i in 1:nrow(WDFW_Males)){
      if(is.na(WDFW_Males$CW[i]))
      { WDFW_Males$Molt.prob[i] <- NA }
      
      if(WDFW_Males$CW[i] <= 150 & !is.na(WDFW_Males$CW[i]))
      { WDFW_Males$Molt.prob[i] <- (-0.0014* WDFW_Males$CW[i]) + 1.14 }
      
      if(WDFW_Males$CW[i] > 150 & !is.na(WDFW_Males$CW[i]))
      { WDFW_Males$Molt.prob[i] <- (-0.014* WDFW_Males$CW[i]) + 2.71 }
      
      # >193 produces negatives
      if(WDFW_Males$CW[i] > 193 & !is.na(WDFW_Males$CW[i]))
      { WDFW_Males$Molt.prob[i] <- 0 }
    }
  
 # WDFW_Males <- molt.prob(WDFW_Males)
 
  # count number of male crabs per pot
  data.summary2 <- WDFW_Males%>% 
    group_by(Date, Site) %>% 
    summarise( Total.crabs = n())
  # append the table to the main data table
  WDFW_Males <- inner_join(WDFW_Males, data.summary2, by = c('Date', 'Site' ))
  ## ZERO MALES FOUND AT TAHUYA ON 2006-08-21
  
  WDFW_Males<- WDFW_Males[, c(1:12,14:15)]
  
  # crabs in each pot
  total.crabs.per.pot <- function (data){
    Totals <- data %>% 
      select(Site, Date, Month, Year, SoakTime_hr, Pot, Total.pots, Total.crabs) %>% 
      group_by(Site, Date, Month, Year, Pot,  Total.pots, Total.crabs, SoakTime_hr) %>% 
      count(Pot)
    colnames(Totals)[colnames( Totals) == "n"]  <- "Crabs.per.pot"
    Totals <- as.data.frame(Totals)
    return(Totals)
  }

  WDFW_pots_total <- total.crabs.per.pot(WDFW_Males)
  
  CPUE <- function(data){
    data <- data %>% mutate(J.date = yday(as_date(Date)))
    # estimating CPUE
    data <- data %>% 
       mutate(Effort = SoakTime_hr * Total.pots) %>%
      mutate(CPUE = Total.crabs / Effort)
  }
  WDFW_pots_total<- CPUE(WDFW_pots_total)
  
  #now account for JUST the sub-adult males
  #sized_data <- size.class(data, sex, "total")
  
  
  
  ###  ###################  ###################  ###################  ###################
  #CHECK THAT ALL THE WDFW MALES ARE READY TO JOIN WITH POTS_TOTAL THEN RUN ESTIMATE
  ###################  ###################  ###################  ###################
  
  join(WDFW_Males, WDFW_pots_total, by= c("Site", "Date", "Month", "Year", "Pot", "Total.pots", "Total.crabs", "SoakTime_hr")
  
       
       
       
       
       
  est.mort.InOut <- function(data, y, a){
    data <- subset(data, Date == "2014-04-07" | Date == "2014-09-17"| Date == "2020-04-03" |Date == "2020-09-01" |Date == "2020-09-15" | Date == "2021-04-16" |Date == "2021-08-31")
    
    #create pre and post df for each size class 
    Pre_molt_juv <- subset(data, CW <= 139 & Month == "April" & Year == y) #juveniles moving into sub-adults
    
    # sub-adults have to be spilt into two b/c of zhang LR calc
    Pre_molt_sub1 <- subset(data, CW %in% c(140:149) & Month == "April"& Year == y)
    Pre_molt_sub2 <- subset(data, CW %in% c(150:159) & Month == "April"& Year == y)
    
    Post_molt_sub <- subset(data, CW %in% c(140:159) & Month == "September" |Month == "August" & Year == y)
    
    molt.in = nrow(Pre_molt_juv) * mean(Pre_molt_juv$Molt.prob) 
    molt.out = (nrow(Pre_molt_sub1) * mean(Pre_molt_sub1$Molt.prob)) + (nrow(Pre_molt_sub2)* mean(Pre_molt_sub2$Molt.prob)) 
    survived <- (((nrow(Post_molt_sub) + molt.in - molt.out) * mean(Post_molt_sub$Effort))  / ((nrow(Pre_molt_sub1) + nrow(Pre_molt_sub2)) * mean(Pre_molt_sub1$Effort))/a)  # N(1)/N(0)
    return(survived)
  }
  
 
  ####################################################################################################
  # ALL ABOVE WORKS AND IS BETTER BUT NOT CATCHING ZEROS IN TOTAL CRABS PER POT
  #CHECK THE FUNCTIONS/ TIDYVERSES ABOVE
  ####################################################################################################
  
  
  
  # account for zero males in a pot
  WDFW_pots1$Pot <- as.ordered(WDFW_pots1$Pot)
  # set to max number of levels
  levels(WDFW_pots1$Pot) = 1:data.summary[l]
  #expand out the table and fill in the crabs per pot values with 0's
  Apr_Sept_CPUEtemp <- WDFW_pots %>% 
    complete(expand(WDFW_pots, Pot), fill = list(Crabs.per.Pot = 0))
  
  WDFW_pots_total <- NULL
  for(l in 1:nrow(data.summary)){
    dat <- WDFW_Males[WDFW_Males$Site == data.summary$Site[l] & WDFW_Males$Date == data.summary$Date[l], ] # calls the site
    dat$Pot <- as.ordered(dat$Pot)
    # set to max number of levels
    levels(dat$Pot) = 1:data.summary$Total.pots[l]
    #expand out the table and fill in the crabs per pot values with 0's
    Apr_Sept_CPUEtemp <- dat %>% 
      complete(expand(dat, Pot), fill = list(Crabs.per.Pot = 0))
    
    WDFW_pots[WDFW_pots$Site == s, 12 ] <- dat$Pot
  }
  
  
  # start  crab count per pot by accounting for comments that say no dungeness
  WDFW_Crabs$Crabs.per.Pot <- rep(NA, nrow(WDFW_Crabs))
  index_abs <- c(grep("no dungies", WDFW_Crabs$CrabComments, ignore.case = T),  grep("no dungys", WDFW_Crabs$CrabComments, ignore.case = T),  grep("no dungeness", WDFW_Crabs$CrabComments, ignore.case = T))
  WDFW_Crabs[index_abs, 19] <- 0
  
  
  
  
  
  
  
##########NEED TO ORGANIZE BY POT AND GET POTS TO BE MAX 15 PER SAMPLE DATE/SITE#############
#### USE FUNCTIONS MADE UP BUT NEED TO ADJUST THEM FOR THIS DATASET
# first set as ordered factor
#colnames(WDFW_pots)[colnames( WDFW_pots) == "PotID"]  <- "Pot"
WDFW_pots1$Pot <- as.ordered(WDFW_pots1$Pot)
# set to max number of levels
levels(WDFW_pots$Pot) = 1:15
 #expand out the table and fill in the crabs per pot values with 0's
Apr_Sept_CPUEtemp <- WDFW_pots %>% 
  complete(expand(WDFW_pots, Pot), fill = list(Crabs.per.Pot = 0))

years <- na.omit(unique(WDFW_pots$Year))

for(s in 1:length(sites)){
  for(y in 1: length(years)){
    Males_site_yr <- WDFW_pots[WDFW_pots$Site == sites[1], ] %>% 
      select(Year, ShellCondition) %>% 
      group_by(Year, ShellCondition) %>% 
      count(ShellCondition)
  }
}


# only Males
#WDFW_Males <- WDFW_Crabs[WDFW_Crabs$Sex == "MALE" | is.na(WDFW_Crabs$Sex), ] #keep NA for NO crabs in pot


######################################### Plots ###########################################################
# Males by month 
ggplot(WDFW_Males, aes(Month, CW))+
  geom_boxplot(fill = "green")

# shell condition by month 
ggplot(na.omit(WDFW_Males), aes(x=ShellCondition, fill=ShellCondition)) +
  geom_bar(binwidth= 10)+ 
  facet_wrap(~Month, nrow=2)+
  ggtitle("WDFW Hood Canal D.crabs")


# shel condition by site
ggplot(na.omit(WDFW_Males), aes(x=ShellCondition, fill=ShellCondition)) +
  geom_bar(binwidth= 10)+ 
  facet_wrap(~Site, nrow=2) +
  ggtitle("WDFW Hood Canal D.crabs by site ")


# shell condition = newly molted by month 
ggplot(na.omit(WDFW_Males[WDFW_Males$ShellCondition == 7,]), aes(x=ShellCondition, fill=ShellCondition)) +
  geom_bar(binwidth= 10)+ 
  facet_wrap(~Month, nrow=2)+
  ggtitle("WDFW Hood Canal D.crabs")

# shell condition = newly molted by site 
ggplot(na.omit(WDFW_Males[WDFW_Males$ShellCondition == "3-1",]), aes(x=ShellCondition, fill=ShellCondition)) +
  geom_bar(binwidth= 10)+ 
  facet_wrap(~Site, nrow=2)+
  ggtitle("WDFW Hood Canal D.crabs")


sites <- na.omit(unique(WDFW_Males$Site))
# shell condition = newly molted at each site separated by month and time  
ggplot(na.omit(WDFW_Males[WDFW_Males$ShellCondition == 8 |WDFW_Males$ShellCondition == 8  & WDFW_Males$Site == sites[1], ]), aes(x=Month, fill=Month)) +
  geom_bar(binwidth= 10)+ 
  facet_wrap(~Year, nrow=2)+
  ggtitle("WDFW Hood Canal D.crabs")

# determine month with most pre-molt stage
z<- WDFW_Males[WDFW_Males$ShellCondition == 8 |WDFW_Males$ShellCondition == 8  & WDFW_Males$Site == sites[3], ]
which.max(table(z$Month))


### Notes per site 
#' site 1: 2003-2004, 2006, 2010,2018- 2019 . newly molted = may- september ?
#' pre-molt = complete yrs . Apr-June maybe sept , december 
#' site 2: 2000-2004, 2018 . newly molted= sept - Feb ? 
#' pre-molt = complete yrs = April - june. 2002: big spike in september
#' site 3: 2001-2004, 2010, 2017 . newly molted = May - sept ? big spike 2002 in april 
#' pre-molt = complete yrs = April - June // bike spike in september 2002 
#' site 4: 2003, 2006, 2009, 2012, 2020 . newly molted = May - june (april ?)
#' site 5: 2002-2004 . newly molted = April - may
#' site 6: 2002, 2015 . newly molted = April or october..?
#' site 7: 2003-2004, 2013, 2015 . newly molted = April - June
#' site 8: 2017, 2019 . newly molted = May - June
#' site 9: 2002-2004 . newly molted = March - april 
#' site 10: 2001, 2003-2005, 2017 . newly molted = April - may // NEWSET molted = June
#' site 11: 2003-2004, 2019 . newly molted = May


######################################### Plots ###########################################################


