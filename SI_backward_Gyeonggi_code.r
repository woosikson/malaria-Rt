library(tidyverse)
library(data.table)
library(foreach)
library(doMC)
library(parallel)
library(armspp) # adaptive rejection Metropolis sampling

registerDoMC(cores = detectCores())
set.seed(1)

dn <- 'processed/'
dir.create(dn)

dn <- 'processed/SI_backward_Gyeonggi/'
dir.create(dn)

# import mosquito and temperature data
dt.Mos.daily <- fread('input/Gyeonggi_mos_on_off.csv')
dt.temp <- fread('input/Gyeonngi_temp.csv')

# dev_rate: developmental rate
dt.temp$thresh <- 14.5
dt.temp[, ':=' (dev_rate = (temp - 14.5)/105)]
dt.temp[temp < 14.5, ':=' (dev_rate = 0)]

#-------------------------------------------------------------------------------

# Calculate t_4, t_2 periods that can fall within weeks 14-44 from 2009-2018
# in Figure 1

list_t.3 <- lapply(2007:2018, function(year) {
  seq(from = as.Date(paste0(year, '141'), "%Y%U%u"),
      to = as.Date(paste0(year, "441"), "%Y%U%u"), by = 'day')
})
list_t.3 <- do.call("c", list_t.3) # t.3 refers to t_2 in Figure 1

dt.t.3_1 <- foreach(u = list_t.3) %dopar% {
  
  # t.2 (date when mosquito incubation is completed) is determined deterministically
  dt_I_M <- dt.temp[u <= date, .(date, dev_rate)][order(date)]
  dt_I_M[, ':=' (dev_rate_cumsum = cumsum(dev_rate))]
  t.2 <- dt_I_M[dev_rate_cumsum > 1, min(date)]
  
  return(data.table(
    t.3 = u,
    t.2 = t.2,
    t.1 = seq(from = t.2, to = t.2 + 60, by = 'day') # Because T_MH max was set to 60
  ))
  
} %>% rbindlist # t.2, t.1 refer to t_3, t_4 in Figure 1 respectively

dt.t.3_1[, ':=' (t.1.week = as.integer(strftime(t.1, format = '%V')) - 1)]
possible_t.1 <- dt.t.3_1[14 <= t.1.week & t.1.week <= 44, t.1] %>% unique %>% sort

# Even after calculating possible_t.1 and generating from t.0 to t.3,
# t.3 may not fall within weeks 14-44.
# This is because there is degeneracy in the mosquito biting period (0-60 days).
# After all calculations are complete, check t.3 and delete rows not within weeks 14-44.

#------------------------------------------------------------------------------- 
 
t.0.list <- seq(from = as.Date('2009-01-07'), to = as.Date('2018-12-26'), by = 'week')

alpha <- 0.1
beta <- 0.2

for (ind in 1:length(t.0.list)) {
  
  set.seed(ind)
  
  t.0 <- t.0.list[ind]
  
  #-----------------------------------------------------------------------------
  
  # t.1: sampling I_{H}
  p <- 0.7423
  theta <- 1.11405
  k <- 22.8197
  mu <- 5.78509
  sigma <- 0.140988
  N <- 5000
  
  dt.t.01 <- data.table()
  
  while (nrow(dt.t.01) < 1000) {
    
    # adaptive rejection Metropolis sampling from given pdf (Nah et al, 2010)
    I_H.sample <- arms(n_samples = N,
                       log_pdf = function(x) {
                         log(p*x^(k-1)*exp(-x/theta)/(theta^k*gamma(k)) +
                               (1-p)*exp(-(log(x) - mu)^2/(2*sigma^2))/(x*sigma*sqrt(2*pi)))
                       }, lower = 1, upper = 500)
    
    dt.add <- data.table(
      t.0 = t.0,
      t.1 = t.0 - as.integer(I_H.sample)
    )
    
    dt.t.01 <- rbindlist(list(dt.t.01, dt.add[t.1 %in% possible_t.1, ]))
  }
  
  #-----------------------------------------------------------------------------
  
  # t.2: sampling T_{MH}
  
  dt.t.012 <- dt.t.01[1:1000, ][rep(1:.N, each = 73), ]
  # To sample 5 million serial intervals even after excluding those where t.3 
  # is not within weeks 14-44, choose 73 (1000 * 73 * 73)
  
  t.2 <- foreach(u = dt.t.01[1:1000, t.1]) %dopar% {
    
    Pr_T_MH <- (dt.Mos.daily[u <= date & date <= u + 60, on_off] != 0) * beta * exp(-beta * 0:60)
        
    if (sum(Pr_T_MH) != 0) {
      T_MH.sample <- sample(x = 0:60, size = 73, replace = TRUE, prob = Pr_T_MH)
    } else {
      T_MH.sample <- rep(NA, 73) # If all probabilities are 0, treat as NA
                                 # When creating serial interval distribution from 
                                 # serial interval sampling results, exclude all NA values
    }
    return(u - T_MH.sample)
  }
  
  dt.t.012$t.2 <- as.Date(unlist(t.2))
  
  #-----------------------------------------------------------------------------
  
  # t.3: computing I_{M}
  
  dt.t.0123 <- dt.t.012
  
  t.3 <- foreach(u = dt.t.012$t.2) %dopar% {
    
    dt_I_M <- dt.temp[date <= u, .(date, dev_rate)
    ][order(date, decreasing = TRUE)]
    dt_I_M[, ':=' (dev_rate_cumsum = cumsum(dev_rate))]
    return(dt_I_M[dev_rate_cumsum > 1, max(date)])
  }
  
  dt.t.0123$t.3 <- as.Date(unlist(t.3))
  
  #-----------------------------------------------------------------------------
  
  # t.4: sampling T_{HM} 
  
  dt.t.01234 <- dt.t.0123[rep(1:.N, each = 73), ]
  
  t.4 <- foreach(u = dt.t.0123$t.3) %dopar% {
    
    Pr_T_HM <- (dt.Mos.daily[u <= date & date <= u + 365, on_off] != 0) * alpha * exp(-alpha * 0:365)
    # Because T_HM max was set to 365
    
    if (sum(Pr_T_HM) != 0) {
      T_HM.sample <- sample(x = 0:365, size = 73, replace = TRUE, prob = Pr_T_HM)
    } else {
      T_HM.sample <- rep(NA, 73)
    }
    
    return(u - T_HM.sample)
  }
  
  dt.t.01234$t.4 <- as.Date(unlist(t.4))
  
  #-----------------------------------------------------------------------------
  
  fn <- paste0(dn, t.0, ".RData")
  save(dt.t.01234, file = fn)
  
}