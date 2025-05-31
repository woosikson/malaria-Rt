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

dn <- 'processed/SI_forward_Incheon/'
dir.create(dn)

# import mosquito and temperature data
dt.Mos.daily <- fread('input/Incheon_mos_on_off.csv')
dt.temp <- fread('input/Incheon_temp.csv')

# dev_rate: developmental rate
dt.temp$thresh <- 14.5
dt.temp[, ':=' (dev_rate = (temp - 14.5)/105)]
dt.temp[temp < 14.5, ':=' (dev_rate = 0)]

#-------------------------------------------------------------------------------

# Calculate t_1 periods where t_4, t_2 in Figure 1 can fall within
# weeks 14-44 from 2009-2018

list_t.1 <- lapply(2009:2018, function(year) {
  seq(from = as.Date(paste0(year, '141'), "%Y%U%u"),
      to = as.Date(paste0(year, "441"), "%Y%U%u"), by = 'day')
})
list_t.1 <- do.call("c", list_t.1) # t.1 refers to t_4 in Figure 1

dt.t.1_3 <- foreach(u = list_t.1) %dopar% {
  data.table(
    t.1 = u,
    t.2 = seq(from = u - 60, to = u, by = 'day') # Because T_MH max was set to 60
  )
} %>% rbindlist # t.2 refers to t_3 in Figure 1

t.3 <- foreach(u = dt.t.1_3$t.2) %dopar% {
  
  dt_I_M <- dt.temp[date <= u, .(date, dev_rate)
  ][order(date, decreasing = TRUE)]
  dt_I_M[, ':=' (dev_rate_cumsum = cumsum(dev_rate))]
  return(dt_I_M[dev_rate_cumsum > 1, max(date)])
}

dt.t.1_3$t.3 <- as.Date(unlist(t.3)) # t.3 refers to t_2 in Figure 1

dt.t.1_4 <- foreach(u = dt.t.1_3$t.3) %dopar% {
  data.table(
    t.3 = u,
    t.4 = seq(from = u - 365, to = u, by = 'day') # Because T_HM max was set to 365
  )
} %>% rbindlist # t.4 refers to t_1 in Figure 1

dt.t.1_4[, ':=' (t.3.week = as.integer(strftime(t.3, format = '%V')) - 1)]
  # Calculate week of t.3 (i.e., t_2 in Figure 1) because biting can only occur between weeks 14-44
possible_t.4 <- dt.t.1_4[14 <= t.3.week & t.3.week <= 44 & 
                           t.4 >= as.Date('2009-01-01') & t.4 <= as.Date('2018-12-31'), t.4] %>% 
  unique %>% sort

rm('dt.t.1_4', 'dt.t.1_3')

# Infector's onset/diagnosis/report dates: every Wednesday from 2009-2018
  
t.4.list <- seq(from = as.Date('2009-01-07'), to = as.Date('2018-12-26'), by = 'week')

alpha <- 0.1
beta <- 0.2

try(for (ind in 1:length(t.4.list)) {
 
  set.seed(ind)
  
  # t.4: given
  t.4 <- t.4.list[ind]
  
  if (t.4 %in% possible_t.4) {
    
    #-----------------------------------------------------------------------------
    
    # t.3: sampling T_{HM}
    
    Pr_T_HM <- sapply(0:365, function(T_HM) {
      return((dt.Mos.daily[date == (t.4 + T_HM), on_off] != 0) * alpha * exp(-alpha * T_HM))})
    
    if (sum(Pr_T_HM) != 0) {
      T_HM.sample <- sample(x = 0:365, size = 73, replace = TRUE, prob = Pr_T_HM)
      # To sample 5 million serial intervals even after excluding those where t.3 
      # is not within weeks 14-44, choose 73 (1000 * 73 * 73)
    } else {
      T_HM.sample <- rep(NA, 73) # If all probabilities are 0, treat as NA
                                 # When creating serial interval distribution from 
                                 # serial interval sampling results, exclude all NA values
    }
    
    dt.t.43 <- data.table(
      t.4 = t.4,
      t.3 = t.4 + T_HM.sample
    )
    
    dt.t.43 <- dt.t.43[t.3 < as.Date('2019-01-01'), ]
    
    #-----------------------------------------------------------------------------
    
    # t.2: computing I_{M}
    
    dt.t.432 <- dt.t.43
    
    t.2 <- foreach(u = dt.t.43$t.3) %dopar% {
      dt_I_M <- dt.temp[u <= date, .(date, dev_rate)][order(date)]
      dt_I_M[, ':=' (dev_rate_cumsum = cumsum(dev_rate))]
      return(dt_I_M[dev_rate_cumsum > 1, min(date)])
    }
    
    dt.t.432$t.2 <- as.Date(unlist(t.2))
    dt.t.432 <- dt.t.432[t.2 < as.Date('2019-01-01'), ]
    
    #-----------------------------------------------------------------------------
    
    # t.1: sampling T_{MH}
    
    dt.t.4321 <- dt.t.432[rep(1:.N, each = length(dt.t.432$t.2)), ]
    
    t.1 <- foreach(u = dt.t.432$t.2) %dopar% {
      
      Pr_T_MH <- sapply(0:60, function(T_MH) {
        return((dt.Mos.daily[date == (u + T_MH), on_off] != 0) * beta * exp(-beta * T_MH))}) 
      
      if (sum(Pr_T_MH) != 0) {
        T_MH.sample <- sample(x = 0:60, size = length(dt.t.432$t.2), replace = TRUE, prob = Pr_T_MH)
      } else {
        T_MH.sample <- rep(NA, length(dt.t.432$t.2)) # If all probabilities are 0, treat as NA
                                                     # When creating serial interval distribution from 
                                                     # serial interval sampling results, exclude all NA values
      }
      
      return(u + T_MH.sample)
    }
    
    dt.t.4321$t.1 <- as.Date(unlist(t.1))
    
    #-----------------------------------------------------------------------------
    
    # t.0: sampling I_{H}
    
    dt.t.43210 <- dt.t.4321[rep(1:.N, each = 1000), ]
    
    t.0 <- foreach(u = dt.t.4321$t.1) %dopar% {
      
      # t.1: sampling I_{H}
      p <- 0.7423
      theta <- 1.11405
      k <- 22.8197
      mu <- 5.78509
      sigma <- 0.140988
      N <- 1000
      
      # adaptive rejection Metropolis sampling from given pdf (Nah et al, 2010)
      I_H.sample <- arms(n_samples = N,
                         log_pdf = function(x) {
                           log(p*x^(k-1)*exp(-x/theta)/(theta^k*gamma(k)) +
                                 (1-p)*exp(-(log(x) - mu)^2/(2*sigma^2))/(x*sigma*sqrt(2*pi)))
                         }, lower = 1, upper = 500)
      
      return(u + as.integer(I_H.sample))
      
    }
    
    dt.t.43210$t.0 <- as.Date(unlist(t.0))
    
  } else {
  
    dt.t.43210 <- data.table()
    
  }
  
  #-----------------------------------------------------------------------------
  
  fn <- paste0(dn, t.4, ".RData")
  save(dt.t.43210, file = fn)
  
}, silent = TRUE)