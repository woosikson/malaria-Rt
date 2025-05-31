library(tidyverse)
library(data.table)
library(ggpubr) # multiple plot
library(latex2exp)

dn.fig <- 'fig/'
dir.create(dn.fig)

load('processed/Gyeonggi_backward_infectee_si.dist.RData')

#-------------------------------------------------------------------------------

# Include results for 2013-04-10, 2013-08-07

# ind = 223; 2013-04-10
# ind = 240; 2013-08-07

ind <- 223

fig.1 <- ggplot(data = si.dist[[ind]]$si.dist) +
  geom_area(aes(x = si_week, y = prob), fill = 'darkgrey', alpha = 0.6) +
  geom_line(aes(x = si_week, y = prob), linewidth = 0.3) +
  ylab(TeX(r'($w_{x,t}$)', bold = TRUE)) +
  xlab(TeX(r'($x_{\[week\]}$)', bold = TRUE)) +
  ylim(0, 0.3) +
  xlim(0, 85) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 12))

ind <- 240

fig.2 <- ggplot(data = si.dist[[ind]]$si.dist) +
  geom_area(aes(x = si_week, y = prob), fill = 'darkgrey', alpha = 0.6) +
  geom_line(aes(x = si_week, y = prob), linewidth = 0.3) +
  ylab(TeX(r'($w_{x,t}$)', bold = TRUE)) +
  xlab(TeX(r'($x_{\[week\]}$)', bold = TRUE)) +
  ylim(0, 0.3) +
  xlim(0, 85) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 12))

fig <- ggarrange(fig.1, fig.2,
                 labels = c("(a) 15th week 2013", "(b) 32nd week 2013"), 
                 label.x = c(0.5, 0.48), label.y = 0.96,
                 hjust = c(-0.4, -0.4),
                 nrow = 1, ncol = 2, align = 'v')

fn <- paste0(dn.fig, 'Fig_3.eps')
ggsave(fn, width = 7*6/4, height = 7*4/6, device = cairo_ps)