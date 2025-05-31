library(tidyverse)
library(data.table)
library(latex2exp)
library(ggpubr) # multiple plot

# Load record data
dt.record <- fread('input/malaria_kdca.csv')
dt.record[, ':=' (date = as.Date(date))]

# Load instantaneous R data
load(file = 'processed/R_inst.RData')

dt.one <- data.table(
  date = dt.R.inst.Gyeonggi$date,
  one = 1
)

#-------------------------------------------------------------------------------

# Analysis for Gyeonggi region
dt.1 <- dt.record[region == 'Gyeonggi' & date >= as.Date('2009-01-01'), .(date, N)]

dt.R <- dt.R.inst.Gyeonggi[, .(date, R.inst)]

# Record the first time each year when R.inst > 1
first_day <- paste0(as.character(2009:2019), '-01-01')

dt.early.warning <- lapply(1:length(first_day), function(ind) {
  dt.R[first_day[ind] <= date & date <= first_day[ind + 1] &
         R.inst >= 1, ] %>% head(1)
}) %>% rbindlist

dt.2 <- merge(dt.1, dt.early.warning)

fig.1 <- ggplot(dt.1) +
  geom_area(aes(x = date, y = N), fill = 'darkgrey', alpha = 0.6) +
  geom_line(aes(x = date, y = N), linewidth = 0.3) +
  geom_point(data = dt.2, aes(x = date, y = N), color = 'red') +
  geom_vline(xintercept = dt.2$date, linetype = "dotted", color = 'red') +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 60) +
  ylab(TeX(r'($I_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 12,face="bold"),
        plot.title = element_text(size = 12))

fig.2 <- ggplot(dt.R) +
  geom_line(aes(x = date, y = R.inst), linewidth = 0.3) +
  geom_line(data = dt.one, aes(x = date, y = one), linewidth = 0.15, linetype = "dotted") +
  geom_point(data = dt.early.warning, aes(x = date, y = R.inst), color = 'red') +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 10) +
  ylab(TeX(r'($R^{inst}_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 12,face="bold"),
        plot.title = element_text(size = 12))

#-------------------------------------------------------------------------------

# Analysis for Incheon region
dt.1 <- dt.record[region == 'Incheon' & date >= as.Date('2009-01-01'), .(date, N)]

dt.R <- dt.R.inst.Incheon[, .(date, R.inst)]

# Record the first time each year when R.inst > 1
first_day <- paste0(as.character(2009:2019), '-01-01')

dt.early.warning <- lapply(1:length(first_day), function(ind) {
  dt.R[first_day[ind] <= date & date <= first_day[ind + 1] &
         R.inst >= 1, ] %>% head(1)
}) %>% rbindlist

dt.2 <- merge(dt.1, dt.early.warning)

fig.3 <- ggplot(dt.1) +
  geom_area(aes(x = date, y = N), fill = 'darkgrey', alpha = 0.6) +
  geom_line(aes(x = date, y = N), linewidth = 0.3) +
  geom_point(data = dt.2, aes(x = date, y = N), color = 'red') +
  geom_vline(xintercept = dt.2$date, linetype = "dotted", color = 'red') + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 20) +
  ylab(TeX(r'($I_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 12,face="bold"),
        plot.title = element_text(size = 12))

fig.4 <- ggplot(dt.R) +
  geom_line(aes(x = date, y = R.inst), linewidth = 0.3) +
  geom_line(data = dt.one, aes(x = date, y = one), linewidth = 0.15, linetype = "dotted") +
  geom_point(data = dt.early.warning, aes(x = date, y = R.inst), color = 'red') +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 10) +
  ylab(TeX(r'($R^{inst}_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 12,face="bold"),
        plot.title = element_text(size = 12))

# Combine all plots
fig <- ggarrange(fig.1, fig.2, fig.3, fig.4,
                 labels = c("(a) Gyeonggi", "(b) Gyeonggi",
                            "(c) Incheon", "(d) Incheon"), 
                 label.x = 0.72, label.y = 0.96,
                 hjust = c(-0.5, -0.5, -0.565, -0.565),
                 nrow = 4, ncol = 1, align = 'v')

# Save figure
dn.fig <- 'fig/'
dir.create(dn.fig, showWarnings = FALSE)

fn <- paste0(dn.fig, 'Fig_7.eps')
ggsave(fn, width = 7, height = 7*4/3, device = cairo_ps)