library(tidyverse)
library(data.table)
library(doMC)
library(foreach)
library(ggpubr)
library(latex2exp)

# import temperature data
dt.temp <- fread('input/Gyeonngi_temp.csv')

# dev_rate: developmental rate
dt.temp$thresh <- 14.5
dt.temp[, ':=' (dev_rate = (temp - 14.5)/105)]
dt.temp[temp < 14.5, ':=' (dev_rate = 0)]

dn.fig <- 'fig/'
dir.create(dn.fig)

dt.date <- data.table(
  date = seq(from = as.Date('2014-12-31'), to = as.Date('2016-01-01'),
             by = 'week')
)
dt.date[, ':=' (week = as.integer(strftime(date, format = '%V')),
                year = year(date))]
dates <- dt.date[14 <= week & week <= 44, date]

# Calculate EIP (Extrinsic Incubation Period)
# Plot date vs EIP for each year from 2009 to 2018
dt.eip <- foreach(u = dates) %dopar% {
  dt_I_M <- dt.temp[u <= date, .(date, dev_rate)][order(date)]
  dt_I_M[, ':=' (dev_rate_cumsum = cumsum(dev_rate))]
  return(data.table(
    date = u,
    eip = as.integer(dt_I_M[dev_rate_cumsum > 1, min(date)] - u))
  )
} %>% rbindlist

ggplot(data = dt.eip) +
  geom_area(aes(x = date, y = eip), fill = 'darkgrey', alpha = 0.6) +
  geom_line(aes(x = date, y = eip), linewidth = 0.3) +
  scale_x_date(breaks = as.Date(c('2015-04-01', '2015-06-01', '2015-08-01', '2015-10-01')),
               date_labels = "%b %Y",
               limits = as.Date(c('2015-03-15',
                                  '2015-11-15'))) +  
  ylab(TeX(r'($I_{M}$)', bold = TRUE)) +
  xlab(TeX(r'($t_{2}$)', bold = TRUE)) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 12))

fn <- paste0(dn.fig, 'Fig_2.eps')
ggsave(fn, width = 7, height = 7*2.2/3, device = cairo_ps)