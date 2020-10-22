## NEED TO INCORPORATE LINES 6- 17 INTO A FUNCTION THEN MOLT.INC FXN
## molt increments [Jamieson 1989]
### range 80 ≤ MW ≤ 174 mm


Pots_N$Month.N <- as.numeric( format(Pots_N$Date, format="%m") ) #month

max.molt = 4
# molt expected around July - September
Pre_molt_juv <- subset(Pots_N, CW <= 139 & Month.N %in% c(1:max.molt) ) #juveniles moving into sub-adults
Pre_molt_sub <- subset(Pots_N, CW %in% c(140:160) & Month.N %in% c(1:max.molt))
# error term 
Pre_molt_leg <- subset(Pots_N, CW >= 161 & Month.N %in% c(1:max.molt))
Data_2020 <- subset(Pots_N, CW >= 161 & Month.N %in% c(1:max.molt) & Year ==2020)

sim<- NULL #creates empty lists to store the sim for both sub & juvs
n= 1000




molt.inc <- function(data, sex){
  post_molt <- subset(data, Sex == sex, Date : Sex) #ability ot change sex in fxn
  sim <- as.data.frame(post_molt$CW) # sets up data frame w/ initial CW
  for(i in 1:n){
    e <- rnorm(1, mean= 0, sd= 3.29) #randomizing agent for vv 
    
    #equation 9 from Zhang et al 2004 & Smith and Jamieson 1989 (BC)
    ifelse(post_molt$Sex == 1,
           sim[,i+1] <- (1.069* sim[1]) + 18.07 + e, 
           ifelse(post_molt$Sex == 2,
                  sim[,i+1] <- (0.864* sim[1]) + 32.35 + e, NA))
  }
  
  for(j in 1:nrow(sim)){
    x<- unlist(sim[j, c(2:(n+1))])
    post_molt$POST.carapace[j] <- mean(x)
    post_molt$SD[j] <- sqrt(var(x))
    
    post_molt$diff <- post_molt$POST.carapace - post_molt$CW    
    post_molt$perc <- post_molt$diff / post_molt$POST.carapace
  }
  return(post_molt)
}

post_molt_juv <- molt.inc(Pre_molt_juv, 1)
post_molt_sub <- molt.inc(Pre_molt_sub, 1)
post_molt_leg <- molt.inc(Pre_molt_leg, 1)
post_2020 <- molt.inc(Data_2020, 1) #Use to show direct relationship for 2020 
