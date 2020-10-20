######################################################################
#  Estimating molting probabilty using (Zhang et al 2004) values
##' equations in figure 3~
##' LReg. showed a distinct breaking point at 150mm -> TWO equations
######################################################################

molt.prob <- function(data){
  data <- na.omit(data)
  data$Molt.prob <- rep(NA, nrow(data)) #null column
  
  for(i in 1:nrow(data)){
    if(data$CW[i] <= 150)
    { data$Molt.prob[i] <- (-0.0014* data$CW[i]) + 1.14 }
    
    if(data$CW[i] > 150)
    { data$Molt.prob[i] <- (-0.014* data$CW[i]) + 2.71 }
  }
  return(data)
}
