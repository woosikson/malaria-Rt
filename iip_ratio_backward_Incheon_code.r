library(tidyverse)
library(data.table)
library(doMC)
library(parallel)

registerDoMC(cores = detectCores())

# short IIP ratio for Incheon

dn <- 'processed/SI_backward_Incheon/'
ls <- list.files(dn)

si.dist <- foreach(ind = 1:length(ls)) %dopar% {
  
  str.1 <- str_split(string = ls[ind], pattern = '[.]') %>% unlist
  date <- str.1[1]
  
  fn <- paste0(dn, ls[ind])
  load(fn)
  
  dt.t.01234 <- dt.t.01234[!is.na(t.0), ] # removing rows such as t.0 = NA
  
  # Although we placed restrictions when sampling t.1 from t.0 
  # to ensure t.3 falls within weeks 14-44,
  # when t.2 is sampled from t.1, due to the following variability (t.1 - t.2 = 0 ~ 6 day),
  # t.3 may fall outside weeks 14-44.
  # Let's remove these cases.
  
  dt.t.01234[, ':=' (t.1.week = as.integer(strftime(t.1, format = '%V')) - 1,
                     t.3.week = as.integer(strftime(t.3, format = '%V')) - 1)]
  
  dt.1 <- dt.t.01234[14 <= t.3.week & t.3.week <= 44 &
                       14 <= t.1.week & t.1.week <= 44, ]
  
  dt.1[, ":=" (iip = as.integer(t.0 - t.1))]
  
  return(list(date = date,
              ratio_short_iip = ifelse(nrow(dt.1) == 0, 0, sum(dt.1$iip < 70)/nrow(dt.1))))
  
}

dn <- 'processed/'

fn <- paste0(dn, 'Incheon_backward_ratio_short_iip.RData')
save(si.dist, file = fn)