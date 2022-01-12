# Estimated post-molt carapace width based on Smith and Jamieson 1989
#' data = datasource with CW
#' mu = mean of error term [EX. 0]
#' sd = standard eviation of error term - Zhang et al. suggest [3.29mm]
#' 

molt.delta <- function(data, mu, sd){
  # give value for juv, sub-legal and legal size classes
  size <- ifelse(data$CW < 140, 1, 
                 ifelse(data$CW %in% 140:159, 2, NA) ) # wnat legal males to be NA so we can exclude

  
  # equatuon: PW = 1.069mw + 18.07 + Error {eqn 9 in Zhang et al. 2017}
    est <- (1.069 * data$CW) + 18.07 + rnorm(nrow(data), mu, sd)
    
   # give value for ESTIAMTE juv, sub-legal and legal size classes
    est_size <- ifelse(est < 140, 1, 
                   ifelse(est %in% 140:159, 2, 3) ) 
    change <- est_size - size
  
}
