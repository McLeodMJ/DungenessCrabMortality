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

MCR <- read.csv("../Data/MCR.csv")
MCR$Date <- as.Date(MCR$Date,format= "%d-%b-%y")
# adjust date
MCR <- MCR %>% 
  mutate(Month = factor(
    month(Date, label = FALSE),   # thing you're converting
    1:12,                                  # values it could take
    labels =                               # how they should appear
      c("January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"),
    ordered = TRUE)) #Sets the Month to automatically order via month order [lubridate package]
colnames(MCR)[colnames(MCR) == "sz"] <- "CW"
colnames(MCR)[colnames(MCR) == "Statio"] <- "Site"
colnames(MCR)[colnames(MCR) == "Rep"] <- "Pot"
colnames(MCR)[colnames(MCR) == "sex..F.0."] <- "Sex"
colnames(MCR)[colnames(MCR) == "IFYEAR"] <- "Year"
MCR$Sex <- ifelse(MCR$Sex == 0, "F", "M")

#extract only males
MCR_males <- MCR[MCR$Sex == "M", ]
MCR_males <- MCR_males[complete.cases(MCR_males),]

#size-specific molting probabilites 
MCR_males <- molt.prob(MCR_males)


#separate by sites
MCR_males$Site <- ifelse(MCR_males$Site == 'wb', 'WB', MCR_males$Site)
sites <-unique(MCR_males$Site)

MCR_Esi <- MCR_males[MCR_males$Site == sites[1], ]

MCR_Wb <- MCR_males[MCR_males$Site == sites[2], ]




#organize by site
MCR_list_sites <- vector("list", 2)
names(MCR_list_sites) <- sites


# format the dat for calc.
MCR_list_sites[[1]] <- format.size.class(MCR_Esi, NA)
MCR_list_sites[[2]] <- format.size.class(MCR_Wb, NA)

# format the dat for calc.
MCR_List_Mll <- vector("list", 2)
names(MCR_List_Mll) <- sites

#create list by site of means 
#months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#freq.dat(MCR_Esi, 3)
MCR_List_Mll[[1]] <- format.dat.mll(MCR_list_sites[[1]], NA, NA, NA, NA, F)
#freq.dat(ODFW_Als, 3)
MCR_List_Mll[[2]] <- format.dat.mll(MCR_list_sites[[2]], NA, NA, NA, NA, F)


#mll results
MCR_Results <- data.frame(Site = "ESI",
                           M = rep(-999, 1),
                           Std_error = rep(-999, 1))

d=1 
  Molt.LL2 <- mle2(minuslogl =  molt.LL.shell, 
                   start = list(k1 = log(10),
                                k2 = log(10),
                                k3 = log(10),
                                M = log(0.3),
                                Mu1 = log(mean(MCR_list_sites[[d]]$Mu_sub1, na.rm = T)),
                                Mu2 = log(mean(MCR_list_sites[[d]]$Mu_sub2, na.rm = T))), 
                   data = list(dat = MCR_List_Mll[[d]], dat_total = MCR_list_sites[[d]]))
  #method = "L-BFGS-B", 
  #lower = rep(-1e10, 6), upper = rep(1e10, 6),
  #control = list(trace = 5, fnscale = -1) )
  #method = "Nelder-Mead") 
  print(summary(Molt.LL2)) #watch updates
  # EXP the mortality estimate 
  
  MCR_Results[d, 2] <- coef(summary(Molt.LL2))[6,1]
  MCR_Results[d, 3] <- coef(summary(Molt.LL2))[6,2]

MCR_Results$Upper <- exp(MCR_Results$M  + 1.96*MCR_Results$Std_error)
MCR_Results$Lower <- exp(MCR_Results$M  - 1.96*MCR_Results$Std_error)
MCR_Results$M <- exp(MCR_Results$M)



# plot of each site with estimate of mortality 
ggplot(MCR_Results, aes(x=Site, y= M, color= Site)) +
  geom_errorbar(aes(ymin= Lower, ymax=Upper), width=.1) +
  ylab("Natural Mortality Rate")+
  ylim(0,1)+
  geom_point() +
  theme_minimal() +
  ggtitle("MCR Dataset")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_color_viridis(discrete = T) +
  guides(color=FALSE)

