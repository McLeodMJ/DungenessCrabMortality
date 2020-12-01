Check the total pots used in sample in order to account for any missing pots in sub-male df

# counts the total number of crabs per pot  
Apr_Sept_CPUE<- Apr_Sept_Sub %>% 
  select(Date, Month, Year, Pot, Total.pots, Total.crabs, Effort) %>% 
  group_by(Date, Month, Year, Pot, Total.pots, Total.crabs, Effort) %>% 
  count(Pot)
colnames(Apr_Sept_CPUE)[colnames( Apr_Sept_CPUE) == "n"]  <- "Crabs.per.Pot"
Apr_Sept_CPUE <- as.data.frame(Apr_Sept_CPUE)

#Reorder by date
index <- with(Apr_Sept_CPUE, order(Date, Pot)) #organizes by month then year
Apr_Sept_CPUE <- Apr_Sept_CPUE[index,]

Apr_Sept_CPUE <- Apr_Sept_CPUE[Apr_Sept_CPUE$Date != "2020-09-01", ] #save only the last date in Sept2020 for CPUE calc - needs one end pt


# Set up DF of Pre-Post Molt dates from 2014 & 2020 w/ unaccounted pots 
###Trying to reformat df so that it includes pots that crabs were not observed in even though the pots were deployed. Total number of pots deployed is different for each sample date. 

# Aggregate across Date & Pot
Sample_Dates <- aggregate(cbind(Year, Month, Total.pots, Total.crabs, Effort) ~ Date, data = Apr_Sept_CPUE, mean)
Sample_Dates$Date <- as.factor(Sample_Dates$Date)

#saves the total pots for use in for loop
total.pots <- Sample_Dates$Total.pots

# Account for pots that were sampled but no sub-legal males were present in that pot
df <- Apr_Sept_CPUE[ ,c(1,4,8)] #just values per pot
df$Pot <- factor(df$Pot, levels=1:max(total.pots)) #make factor so cast each sample date across  pots (1-12)
Resample_CPUE <-dcast(df, Date~Pot, add.missing=TRUE, fill=0) #reformat the df to include Pots not present in wide format

###MISSING POT 4 B/C APPARENTLY CRABS WERE NEVER CAUGHT IN 4TH POT (EVEN THOSE THAT INCLUDED MORE THAN 4 POTS IN A SAMPLE DATE)
Resample_CPUE$v4 <- rep(0,4)
Resample_CPUE[,c(1:4,13,5:12)] #doesnt work

#Want to make the pots outside the max pot range for that specific sample NA so that I can then move forward with removing NAs after melt()  
for(i in 1:nrow(Resample_CPUE)){
  Resample_CPUE[i, c(total.pots[i]+2) : length(Resample_CPUE)] <- NA
}  

Resample_CPUE <- melt(Resample_CPUE) #back to long format
Resample_CPUE <- na.omit(Resample_CPUE[, c(1,3,2)]) # reorganize
colnames(Resample_CPUE)[colnames(Resample_CPUE) == "value"] <- "Crabs.per.Pot"

#Reorder by date
index <- with(Resample_CPUE, order(Date, Pot)) #organizes by month then year
Resample_CPUE <- Resample_CPUE[index,]

Resample_CPUE <-merge(Resample_CPUE, Sample_Dates, by="Date", all=TRUE)
Resample_CPUE <- Resample_CPUE[, c(1,4,5,2,3,6:8)]
