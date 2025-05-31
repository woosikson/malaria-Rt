library(tidyverse)
library(data.table)
library(doMC)
library(parallel)

registerDoMC(cores = detectCores())

dn <- 'processed/SI_forward_Gyeonggi/'
ls <- list.files(dn)

si.dist <- foreach(ind = 1:length(ls)) %dopar% {
  
  str.1 <- str_split(string = ls[ind], pattern = '[.]') %>% unlist
  date <- str.1[1]
  
  fn <- paste0(dn, ls[ind])
  load(fn)
  
  dt.t.43210 <- dt.t.43210[!is.na(t.0), ] # removing rows such as t.0 = NA

  if (nrow(dt.t.43210) == 0) {
    dt.t.43210 <- data.table(
      t.4 = as.Date('1999-12-31'),
      t.3 = as.Date('1999-12-31'),
      t.2 = as.Date('1999-12-31'),
      t.1 = as.Date('1999-12-31'),
      t.0 = as.Date('1999-12-31')
    )
  }
  
  # Although we placed restrictions when sampling t.3 from t.4 
  # to ensure t.1 falls within weeks 14-44,
  # when t.2 is sampled from t.3, due to the following variability (t.2 - t.3 = 0 ~ 6 day),
  # t.1 may fall outside weeks 14-44.
  # Let's remove these cases.
  
  dt.t.43210[, ':=' (t.1.week = as.integer(strftime(t.1, format = '%V')) - 1,
                     t.3.week = as.integer(strftime(t.3, format = '%V')) - 1)]
  
  dt.1 <- dt.t.43210[14 <= t.3.week & t.3.week <= 44 &
                       14 <= t.1.week & t.1.week <= 44, ]
  dt.1[, ':=' (si_week = round(as.integer(t.0 - t.4)/7))]
  
  dt.si.dist <- dt.1[, .N, by = .(si_week)][order(si_week)]
  dt.si.dist[, ':=' (prob = N/sum(N))]
  
  return(list(date = date,
              si.dist = dt.si.dist))
 
}

dn <- 'processed/'

fn <- paste0(dn, 'Gyeonggi_forward_infector_si.RData')
save(si.dist, file = fn)