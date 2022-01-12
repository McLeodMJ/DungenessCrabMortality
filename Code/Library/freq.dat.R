# fxn for finding 3-consecutive mths with highest frequency of shoft-shell cond. across yrs


freq.dat <-function(x, post_shell){
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  # create df for the combo of months that have the most legals w/ soft shell count 
  Freq_df <- data.frame(Trial = rep(NA, 12), 
                        Count = rep(NA, 12))
  Freq_df$Trial[1] <- c("January, February, March")
  Freq_df$Trial[2] <- c("February, March, April")
  Freq_df$Trial[3] <- c("March, April, May")
  Freq_df$Trial[4] <- c("April, May, June")
  Freq_df$Trial[5] <- c("May, June, July")
  Freq_df$Trial[6] <- c("June, July, August")
  Freq_df$Trial[7] <- c("July, August, September")
  Freq_df$Trial[8] <- c("August, September, October")
  Freq_df$Trial[9] <- c("September, October, November")
  Freq_df$Trial[10] <- c("October, November, December")
  Freq_df$Trial[11] <- c("November, December, January")
  Freq_df$Trial[12] <- c("December, January, February")
  
  # df of soft shell legals
  x <- x[x$CW >= 160 & x$ShellCondition %in% post_shell, ]
  
  # finding the maximum focal pt count
  for(m in 1:12){ 
    if(m == 11){
      xx  <- nrow(x[x$Month %in% months[c(m:c(m+1), 1)], ])
    }
    if(m ==12){
      xx  <- nrow(x[x$Month %in% months[c(m,1:2)], ])
    }
    xx  <- nrow(x[x$Month %in% months[m:c(m+2)], ])
    Freq_df$Count[m] <- xx
  }
  return( Freq_df[which.max(Freq_df$Count), 1])
}
