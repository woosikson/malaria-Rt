library(tidyverse)
library(data.table)
library(scales)

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

# monthly mean
monthly_R_case_1 <- merge(
  dt.2[N != 0, .(Gyeonggi = round(mean(R_mean), digits = 2)), by = .(month)],
  dt.4[N!= 0, .(Incheon = round(mean(R_mean), digits = 2)), by = .(month)])

# monthly mean
monthly_R_case_2 <- merge(
  dt.2[N != 0, .(Gyeonggi = round(mean(R_mean * N), digits = 2)), by = .(month)],
  dt.4[N!= 0, .(Incheon = round(mean(R_mean * N), digits = 2)), by = .(month)])

# Data
months <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb")
Gyeonggi_A <- c(monthly_R_case_1$Gyeonggi[3:12], monthly_R_case_1$Gyeonggi[1:2])
Incheon_A <- c(monthly_R_case_1$Incheon[3:12], monthly_R_case_1$Incheon[1:2])
Gyeonggi_B <- c(monthly_R_case_2$Gyeonggi[3:12], monthly_R_case_2$Gyeonggi[1:2])
Incheon_B <- c(monthly_R_case_2$Incheon[3:12], monthly_R_case_2$Incheon[1:2])

# Create data frame
df <- data.frame(
  month = factor(rep(months, 4), levels = months),
  region = rep(c("Gyeonggi", "Incheon", "Gyeonggi", "Incheon"), each = 12),
  type = rep(c("Case Reproduction Number", "Case Reproduction Number", 
               "Expected Secondary Cases", "Expected Secondary Cases"), each = 12),
  value = c(Gyeonggi_A, Incheon_A, Gyeonggi_B, Incheon_B)
)

# Define colors
colors <- c("Gyeonggi" = "#56B4E9", "Incheon" = "#D55E00")

# Create the plot
p <- ggplot(df, aes(x = month)) +
  # Bar chart for Expected Secondary Cases (scaled to secondary axis)
  geom_col(data = filter(df, type == "Expected Secondary Cases"),
           aes(y = value / 10, fill = region), 
           position = position_dodge(width = 0.7), 
           width = 0.6, alpha = 0.6) +
  
  # Line plot for Case Reproduction Number (using original scale for primary axis)
  geom_line(data = filter(df, type == "Case Reproduction Number"),
            aes(y = value, color = region, group = region), 
            linewidth = 1, alpha = 1) +
  geom_point(data = filter(df, type == "Case Reproduction Number"),
             aes(y = value, color = region, shape = region), 
             size = 3, alpha = 1) +
  
  # Scale and labels - Bar data scaled to secondary axis
  scale_y_continuous(
    name = "Case Reproduction Number (Line)",
    sec.axis = sec_axis(~ . * 10, name = "Expected Secondary Cases per Month (Bar)")
  ) +
  
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c("Gyeonggi" = 16, "Incheon" = 15)) +  # circle and square
  
  # Themes and formatting
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.y.left = element_text(size = 14, color = "black"),
    axis.title.y.right = element_text(size = 14, color = "black"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  
  xlab("") +
  
  # Add region labels inside the plot
  annotate("text", x = 1.2, y = 1.5, label = "Gyeonggi", 
           color = "#56B4E9", size = 4, fontface = "bold") +
  annotate("text", x = 1.2, y = 0.8, label = "Incheon", 
           color = "#D55E00", size = 4, fontface = "bold")

# Save the plot
dn.fig <- 'fig/'
dir.create(dn.fig)
fn <- paste0(dn.fig, 'Fig_6.eps')

ggsave(fn, plot = p, 
       width = 9, height = 5, units = "in", device = cairo_ps)