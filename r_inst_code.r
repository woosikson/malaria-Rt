library(tidyverse)
library(data.table)
library(doMC)

registerDoMC(cores = detectCores())

# Load record data
dt <- fread('input/malaria_kdca.csv')
dt[, ':=' (date = as.Date(date))]

#-------------------------------------------------------------------------------

func.R.inst <- function(dt.record, dt.record.full, si.dist, dt.index.si,
                        dt.possible.si, region) {
  
  R.inst <- foreach (ind = 1:length(si.dist), .combine='c') %dopar% {
    
    I.t <- dt.record[date_infectee == si.dist[[ind]]$date %>% as.Date, N]
    t <- si.dist[[ind]]$date %>% as.Date
    w.st <- si.dist[[ind]]$si.dist$prob
    w.st.time <- si.dist[[ind]]$si.dist$si
    I.ts <- dt.record.full[date_infectee %in% (t - w.st.time * 7), 
    ][order(-date_infectee)]$N
    R <- ifelse(nrow(si.dist[[ind]]$si.dist) == 0, 0, I.t/sum(I.ts * w.st))
    R
  }
  
  dt.R <- data.table(
    date = dt.record$date_infectee,
    region = region,
    R.inst = R.inst
  )
  
  return(dt.R)
  
}

#-------------------------------------------------------------------------------

# Analysis for Gyeonggi region

dt.record.full <- dt[region == 'Gyeonggi', .(date_infectee = date, N)] 
dt.record <- dt[region == 'Gyeonggi' & date >= as.Date('2009-01-01'), 
                .(date_infectee = date, N)] 

fn <- ('processed/Gyeonggi_backward_infectee_si.dist.RData')
load(file = fn)

# Create dt.possible.si
dt.possible.si <- lapply(1:length(si.dist), function(x) {
  
  if (nrow(si.dist[[x]]$si.dist) == 0) {
    
    return(data.table(
      date_infectee = si.dist[[x]]$date %>% as.Date,
      possible.si = NA
    ))
    
  } else {
    
    return(data.table(
      date_infectee = si.dist[[x]]$date %>% as.Date,
      possible.si = si.dist[[x]]$si.dist$si_week
    ))
    
  }
}) %>% rbindlist

dt.possible.si[, ':=' (date_infector = date_infectee - 7 * possible.si)]

# Create dt.index.si
dt.index.si <- lapply(1:length(si.dist), function(x) {
  return(data.table(
    date_infectee = si.dist[[x]]$date %>% as.Date,
    index = x
  ))
}) %>% rbindlist

dt.R.inst.Gyeonggi <- func.R.inst(dt.record, dt.record.full, 
                                  si.dist, dt.index.si, dt.possible.si, 
                                  'Gyeonggi')

#-------------------------------------------------------------------------------

# Analysis for Incheon region

dt.record.full <- dt[region == 'Incheon', .(date_infectee = date, N)] 
dt.record <- dt[region == 'Incheon' & date >= as.Date('2009-01-01'), 
                .(date_infectee = date, N)] 

fn <- ('processed/Incheon_backward_infectee_si.dist.RData')
load(file = fn)

# Create dt.possible.si
dt.possible.si <- lapply(1:length(si.dist), function(x) {
  
  if (nrow(si.dist[[x]]$si.dist) == 0) {
    
    return(data.table(
      date_infectee = si.dist[[x]]$date %>% as.Date,
      possible.si = NA
    ))
    
  } else {
    
    return(data.table(
      date_infectee = si.dist[[x]]$date %>% as.Date,
      possible.si = si.dist[[x]]$si.dist$si_week
    ))
    
  }
}) %>% rbindlist

dt.possible.si[, ':=' (date_infector = date_infectee - 7 * possible.si)]

# Create dt.index.si
dt.index.si <- lapply(1:length(si.dist), function(x) {
  return(data.table(
    date_infectee = si.dist[[x]]$date %>% as.Date,
    index = x
  ))
}) %>% rbindlist

dt.R.inst.Incheon <- func.R.inst(dt.record, dt.record.full, 
                                 si.dist, dt.index.si, dt.possible.si, 
                                 'Incheon')

#-------------------------------------------------------------------------------

# Save results
dn <- 'processed/'
dir.create(dn, showWarnings = FALSE)

fn <- paste0(dn, 'R_inst.RData')
save(dt.R.inst.Gyeonggi,
     dt.R.inst.Incheon,
     file = fn)