## fxn for LL & prior/post molt eqns


molt.LL.shell <- function(dat, dat_total, k1, k2, k3, Mu1, Mu2, M){
  years <- unique(dat_total$Year)
  
  LL1 <- rep(NA, length(years))
  LL2 <- rep(NA, length(years))
  LL3 <- rep(NA, length(years))
  # prep data for the specific year 
  
  for(y in 1:length(years)){
    # create while loop to only work for years that has data for prior AND post
    if(!is.null(dat[[y]]$prior) & !is.null(dat[[y]]$post)){
      
      # make dfs of pre & post molt periods
      prior <- dat[[y]][1]$prior
      #dat[dat$Year == years[y] & dat$ShellCondition %in% 1:2 & dat$Month %in% c("March", "April", "May", "June"), ] # pre-molt size
      # prior <- #prior[!is.na(prior$Mu_sub1) | !is.na(prior$Mu_sub2), ] # must have at least one sub-leagal szied male from either sub1 or sub2 class
      post <- dat[[y]][2]$post
      #dat[dat$Year == years[y] & dat$ShellCondition %in% 6:8 & dat$Month %in% c("September", "October", "November", "December") & !is.na(dat$Mu_legal), ] # post-molt size
      
      # calcuate the avg time difference btw post & pre molt periods
      DeltaT =  as.numeric(abs(difftime(median(post$Date), median(prior$Date), units = "weeks" ))) / 52
      
      # BOTH sub-legal classes as Log likelihoods at time 1 [ pre-molt]
      LL1[y] <- -mean(dnbinom(x = na.omit(prior$Mu_sub1), mu = exp(Mu1), size = exp(k1), log = T))
      LL2[y] <- -mean(dnbinom(x = na.omit(prior$Mu_sub2), mu = exp(Mu2), size = exp(k2), log = T))
      
      # account for crabs molting in & out 
      lin_mu_1 = exp(Mu1) * mean(prior$Molt.prb_sub1, na.rm = T) * exp(-exp(M) * DeltaT) 
      lin_mu_2 = exp(Mu2) * mean(prior$Molt.prb_sub2, na.rm = T) * exp(-exp(M) * DeltaT)
      if(is.nan(lin_mu_1)){lin_mu_1 = 0}
      if(is.nan(lin_mu_2)){lin_mu_2 = 0}
      mu_post = lin_mu_1 + lin_mu_2
      
      # sub legal males as log likelihood at time 2 [post-molt]
      LL3[y] <- -mean(dnbinom(x = na.omit(post$Mu_legal), mu = mu_post, size = exp(k3), log = T))
    } #end of if statement - skip null years
  } #end of for loop over years
  
  #if sub-legals1 or 2 have only NAs make 0
  if(is.nan(mean(LL1))){LL1 = 0}
  if(is.nan(mean(LL2))){LL2 = 0}
  
  #sum_log_lik <- sum(LL1,LL2,LL3, na.rm = T)
  sum_log_lik <- sum(mean(LL1,na.rm = T), mean(LL2, na.rm = T), mean(LL3, na.rm = T))
  sum_log_lik[which(!is.finite(sum_log_lik))] <- log(1e-320) 
  return(sum_log_lik)
}