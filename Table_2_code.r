library(tidyverse)
library(data.table)

# load record data
dt.record <- fread('input/malaria_kdca.csv')
dt.record[, ':=' (date = as.Date(date))]

# load R_case (confidence interval and mean)
dt.R <- fread('processed/CI_R_case.csv')

#-------------------------------------------------------------------------------

# Gyeonggi
dt.1 <- dt.record[region == 'Gyeonggi' & date >= as.Date('2010-01-01'), .(date, N)]
dt.2 <- merge(dt.1, dt.R[region == 'Gyeonggi', .(date, R_mean)])
dt.2 <- dt.2[date <= as.Date('2017-01-01') & date >= as.Date('2010-01-01'), ]
dt.2[, ':=' (year = year(date),
             month = month(date))]

# Incheon
dt.3 <- dt.record[region == 'Incheon' & date >= as.Date('2010-01-01'), .(date, N)]
dt.4 <- merge(dt.3, dt.R[region == 'Incheon', .(date, R_mean)])
dt.4 <- dt.4[date <= as.Date('2017-01-01') & date >= as.Date('2010-01-01'), ]
dt.4[, ':=' (year = year(date),
             month = month(date))]

#-------------------------------------------------------------------------------

# Include only weeks with symptom onset cases in the average

# yearly mean
annual_R_case <- merge(
  dt.2[N != 0, .(Gyeonggi = round(mean(R_mean), digits = 2)), by = .(year)],
  dt.4[N != 0, .(Incheon = round(mean(R_mean), digits = 2)), by = .(year)])

# save data
dn <- 'processed/'

fn <- paste0(dn, 'Table_2.csv')
fwrite(annual_R_case, file = fn, row.names = FALSE)

# monthly mean
monthly_R_case <- merge(
  dt.2[N != 0, .(Gyeonggi = round(mean(R_mean), digits = 2)), by = .(month)],
  dt.4[N!= 0, .(Incheon = round(mean(R_mean), digits = 2)), by = .(month)])