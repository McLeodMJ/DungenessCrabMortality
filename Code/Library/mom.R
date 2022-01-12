# Method of moments 

#mu = mean(WDFW_list[[d]]$Mu_sub1, na.rm = T)
#vr = var(WDFW_list[[d]]$Mu_sub1, na.rm = T)

mom <- function(mu, vr){
  r = mu^2 / (vr - mu)
  p = 1 - (mu / vr)
  
  k = (r/p) - r
  return(k)
}
