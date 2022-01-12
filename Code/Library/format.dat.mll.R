# function for setting up datasets to have prior/post for each year 


format.dat.mll <- function(dat_total, pre_time, post_time, pre_shell, post_shell, shell_mth){
  
  years <- unique(dat_total$Year)
  dat <- as.list(years)
  names(dat) <- c(years)
  
  for(y in 1:length(years)){
    dat[[y]] <- list(prior = NULL, 
                     post = NULL)
    
    # make dfs of pre & post molt periods
    if(shell_mth == TRUE){
      prior <- dat_total[dat_total$Year == years[y] & dat_total$ShellCondition %in% pre_shell & dat_total$Month %in% pre_time, ] # pre-molt size
      prior  <- prior[!is.na(prior$Mu_sub1) | !is.na(prior$Mu_sub2), ] # must have at least one sub-leagal szied male from either sub1 or sub2 class
      post <- dat_total[dat_total$Year == years[y] & dat_total$ShellCondition %in% post_shell & dat_total$Month %in% post_time & !is.na(dat_total$Mu_legal), ] # post-molt size
    }
    
    # set months instead of frequency derived months
    if(shell_mth == FALSE){
      prior <- dat_total[dat_total$Year == years[y]  & dat_total$Month %in% months[4:7], ] # pre-molt ize
      prior <- prior[!is.na(prior$Mu_sub1) | !is.na(prior$Mu_sub2), ] # must have at least one sub-leagal szied male from either sub1 or sub2 class
      post <- dat_total[dat_total$Year == years[y] & dat_total$Month %in% months[8:9] & !is.na(dat_total$Mu_legal), ] # post-molt size
    }
    
    #exclude years that do not have enough data for the calculation 
    if(nrow(prior) > 0 & nrow(post) > 0){
      dat[[y]]$prior <- prior
      dat[[y]]$post <- post
    }
  }
  return(dat)
}
