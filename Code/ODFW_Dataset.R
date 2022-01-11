# ODFW_Dataset

setwd("~/Box Sync/Eder_Dungeness_2020/Model_code")
source("./Library/Total.Pots.Crabs.R")
source("./Library/Total.Crabs.Per.Pot.R")
source("./Library/CPUE.R")
source("./Library/est.mort.InOut.R")
source("./Library/est.mort_Shell.cond.R")
source("./Library/molt.delta.R")
source("./Library/molt.prob.R")
source("./Library/size.class.R")
source("./Library/format.size.class.R")
source("./Library/format.dat.mll.R")
source("./Library/freq.dat.R")
source("./Library/molt.LL.shell.R")

require(ggplot2)
library(lubridate)
library(tidyverse)
library(viridis)


ODFW_Data_all <- read.csv(file= "../Data/ODFW_Dcrab_sampling_2007-2021_McLeod.csv")

# set up dates
ODFW_Data_all$Date <- as.Date(ODFW_Data_all$Sample.Date,format= "%m/%d/%y")
# adjust date
ODFW_Data_all <- ODFW_Data_all %>% 
  mutate(Month = factor(
    month(Date, label = FALSE),   # thing you're converting
    1:12,                                  # values it could take
    labels =                               # how they should appear
      c("January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"),
    ordered = TRUE)) #Sets the Month to automatically order via month order [lubridate package]
ODFW_Data_all$Year <- as.numeric( format(ODFW_Data_all$Date, format="%Y") )


#cohesive names for lengths
colnames(ODFW_Data_all)[colnames(ODFW_Data_all) == "Length"] <- "CW"
colnames(ODFW_Data_all)[colnames(ODFW_Data_all) == "Shell.Grade"] <- "ShellCondition"
colnames(ODFW_Data_all)[colnames(ODFW_Data_all) == "Port"] <- "Site"
colnames(ODFW_Data_all)[colnames(ODFW_Data_all) == "Pot_ID"] <- "Pot"
colnames(ODFW_Data_all)[colnames(ODFW_Data_all) == "Pots.Total"] <- "Total.pots"


#Reorder data 
levels(ODFW_Data_all$ShellCondition) = 1:3 #1 = hard; 2 = some "give" in shell; 3 = soft shell
index <- with(ODFW_Data_all, order(Site, Date, Pot)) #organizes by month then year
ODFW_Data_all <- ODFW_Data_all[index,]
ODFW_Data_all <- ODFW_Data_all[c(4,17,19,18,14,15,12:13,10,9,16)]


# total pots per sample date & total crabs per sample date
data.summary <- ODFW_Data_all %>% 
  group_by(Site, Date) %>% 
  summarise(Total.crabs = n())
ODFW_Data_males <- inner_join(ODFW_Data_males, data.summary, by = c('Date', 'Site' ))


#extract only males
ODFW_Data_males <- ODFW_Data_all[ODFW_Data_all$Sex == "M", ]

#size-specific molting probabilites 
ODFW_Data_males <- molt.prob(ODFW_Data_males)

#calculate soaktimes
ODFW_Data_males$Drop.Time <- strptime(ODFW_Data_males$Drop.Time , "%H:%M:%S")
ODFW_Data_males$Pull.Time <- strptime(ODFW_Data_males$Pull.Time , "%H:%M:%S")
ODFW_Data_males$Soak.Time.hr <- as.numeric(ODFW_Data_males$Pull.Time - ODFW_Data_males$Drop.Time, units="hours")

#CPUE
ODFW_Data_males <- CPUE(ODFW_Data_males)


shell <- c(1,3) # shell conditions we want to reference
ODFW_Data_males <- ODFW_Data_males[ODFW_Data_males$ShellCondition %in% shell,]



# port 24 = Yaquina Bay
 ODFW_Ya <- ODFW_Data_males[ODFW_Data_males$Site == 24, ]

# port 26 = ALsea Bay
ODFW_Als <- ODFW_Data_males[ODFW_Data_males$Site == 26, ]

#organize by site
ODFW_list_sites <- vector("list", 2)
names(ODFW_list_sites) <- c("Yaquina", "Alsea")


# format the dat for calc.
ODFW_list_sites[[1]] <- format.size.class(ODFW_Ya, T)
ODFW_list_sites[[2]] <- format.size.class(ODFW_Als, T)

# format the dat for calc.
ODFW_list_Mll <- vector("list", 2)
names(ODFW_list_Mll) <- c("Yaquina", "Alsea")

#create list by site of means 
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
freq.dat(ODFW_Ya, 3)
ODFW_list_Mll[[1]] <- format.dat.mll(ODFW_list_sites[[1]], months[4:6], months[7:9], 1, 3, T)
freq.dat(ODFW_Als, 3)
ODFW_list_Mll[[2]] <- format.dat.mll(ODFW_list_sites[[2]], months[2:4], months[5:7], 1, 3, T)


#mll results
sites <- c("Yaquina", "Alsea")
ODFW_Results <- data.frame(Site = sites,
                           M = rep(-999, length(sites)),
                           Std_error = rep(-999, length(sites)))

for(d in 1:length(sites)){
  
  Molt.LL2 <- mle2(minuslogl =  molt.LL.shell, 
                   start = list(k1 = log(10),
                               k2 = log(10),
                                k3 = log(10),
                                M = log(0.3),
                               Mu1 = log(mean(ODFW_list_sites[[d]]$Mu_sub1, na.rm = T)),
                                Mu2 = log(mean(ODFW_list_sites[[d]]$Mu_sub2, na.rm = T))), 
                   data = list(dat = ODFW_list_Mll[[d]], dat_total = ODFW_list_sites[[d]]))
  #method = "L-BFGS-B", 
  #lower = rep(-1e10, 6), upper = rep(1e10, 6),
  #control = list(trace = 5, fnscale = -1) )
  #method = "Nelder-Mead") 
  print(summary(Molt.LL2)) #watch updates
  # EXP the mortality estimate 
  
  ODFW_Results[d, 2] <- coef(summary(Molt.LL2))[6,1]
  ODFW_Results[d, 3] <- coef(summary(Molt.LL2))[6,2]
}
ODFW_Results$Upper <- exp(ODFW_Results$M  + 1.96*ODFW_Results$Std_error)
ODFW_Results$Lower <- exp(ODFW_Results$M  - 1.96*ODFW_Results$Std_error)
ODFW_Results$M <- exp(ODFW_Results$M)



# plot of each site with estimate of mortality 
ggplot(ODFW_Results, aes(x=Site, y= M, color= Site)) +
  geom_errorbar(aes(ymin= Lower, ymax=Upper), width=.1) +
  ylab("Natural Mortality Rate")+
  ggtitle("ODFW Datasets")+
  ylim(0,4)+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_color_viridis(discrete = T) +
  guides(color=FALSE)






