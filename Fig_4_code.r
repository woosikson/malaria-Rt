library(tidyverse)
library(data.table)
library(latex2exp)
library(ggpubr) # multiple plot

# load record data
dt.record <- fread('input/malaria_kdca.csv')
dt.record[, ':=' (date = as.Date(date))]

# load R_case (confidence interval and mean)
dt.R <- fread('processed/CI_R_case.csv')

dt.one <- data.table(
  date = dt.R[region == 'Gyeonggi', date],
  one = 1
)

#-------------------------------------------------------------------------------

# Gyeonggi
dt.1 <- dt.record[region == 'Gyeonggi', .(date, N)]

#-------------------------------------------------------------------------------

# Short/long incubation period

# infectee: serial interval distribution
load(file = 'processed/Gyeonggi_backward_ratio_short_iip.RData')

dt.2 <- lapply(1:length(si.dist), function(ind) {
  
  return(data.table(
    date = si.dist[[ind]]$date %>% as.Date,
    ratio_short_iip = si.dist[[ind]]$ratio_short_iip
  ))
}) %>% rbindlist

dt.3 <- merge(dt.1, dt.2)
dt.3[, ':=' (short_iip = round(N*ratio_short_iip))]
dt.3[, ':=' (long_iip = N - short_iip)]

dt.4 <- dt.3[, .(date, long_iip, short_iip)] %>%
  pivot_longer(!date, names_to = 'type', values_to = 'N') %>%
  as.data.table
dt.4$type <- factor(dt.4$type, 
                    levels=c("short_iip", "long_iip"))

fig.1 <- ggplot(dt.4, aes(x = date, y = N, fill = type)) +
  geom_area() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 60) +
  ylab(TeX(r'($I_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(size = 12), legend.position = "none")

fig.2 <- ggplot(dt.R[region == 'Gyeonggi' & date < as.Date('2017-01-01'), 
                     .(date, R_ll, R_mean, R_ul)], aes(date)) +
  geom_ribbon(aes(ymin = R_ll, ymax = R_ul), fill = "grey70") +
  geom_line(aes(y = R_mean), linewidth = 0.3) +
  geom_line(data = dt.one, aes(x = date, y = one), linewidth = 0.5, linetype = "dotted") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 8) +
  ylab(TeX(r'($R^{case}_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(size = 12), legend.position = "none")

#-------------------------------------------------------------------------------

# Incheon
dt.1 <- dt.record[region == 'Incheon', .(date, N)]

#-------------------------------------------------------------------------------

# Short/long incubation period

# infectee: serial interval distribution
load(file = 'processed/Incheon_backward_ratio_short_iip.RData')

dt.2 <- lapply(1:length(si.dist), function(ind) {
  
  return(data.table(
    date = si.dist[[ind]]$date %>% as.Date,
    ratio_short_iip = si.dist[[ind]]$ratio_short_iip
  ))
}) %>% rbindlist

dt.3 <- merge(dt.1, dt.2)
dt.3[, ':=' (short_iip = round(N*ratio_short_iip))]
dt.3[, ':=' (long_iip = N - short_iip)]

dt.4 <- dt.3[, .(date, long_iip, short_iip)] %>%
  pivot_longer(!date, names_to = 'type', values_to = 'N') %>%
  as.data.table
dt.4$type <- factor(dt.4$type, 
                    levels=c("short_iip", "long_iip"))

fig.3 <- ggplot(dt.4, aes(x = date, y = N, fill = type)) +
  geom_area() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 21) +
  ylab(TeX(r'($I_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(size = 12), legend.position = "none")

fig.4 <- ggplot(dt.R[region == 'Incheon' & date < as.Date('2017-01-01'), 
                     .(date, R_ll, R_mean, R_ul)], aes(date)) +
  geom_ribbon(aes(ymin = R_ll, ymax = R_ul), fill = "grey70") +
  geom_line(aes(y = R_mean), linewidth = 0.3) +
  geom_line(data = dt.one, aes(x = date, y = one), linewidth = 0.5, linetype = "dotted") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c('2010-01-01','2019-01-01'))) +
  ylim(0, 8) +
  ylab(TeX(r'($R^{case}_{t}$)', bold = TRUE)) +
  xlab(TeX(r'($t$)', bold = TRUE)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(size = 12), legend.position = "none")

#-------------------------------------------------------------------------------

fig <- ggarrange(fig.1, fig.2, fig.3, fig.4,
                 labels = c("(a) Gyeonggi", "(b) Gyeonggi",
                            "(c) Incheon", "(d) Incheon"), 
                 label.x = 0.72, label.y = 0.96,
                 hjust = c(-0.5, -0.5, -0.565, -0.565),
                 nrow = 4, ncol = 1, align = 'v')

dn.fig <- 'fig/'
dir.create(dn.fig)

fn <- paste0(dn.fig, 'Fig_4.eps')
ggsave(fn, width = 7, height = 14*2/3, device = cairo_ps)