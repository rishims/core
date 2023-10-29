# Plot Production Code
# Rishi Shah
# May 13, 2023

# Create plots using processed data (fig.width = 12, fig.height = 12)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggsci)

results <- read.csv('ratefile.csv')

# no insurance
ni_white_rate = results[results$outcome == "noinsurance" & results$race_group == "white",]$rate
ni_black_rate = results[results$outcome == "noinsurance" & results$race_group == "black",]$rate
ni_asian_rate = results[results$outcome == "noinsurance" & results$race_group == "asian",]$rate
ni_hispanic_rate = results[results$outcome == "noinsurance" & results$race_group == "hispanic",]$rate

ni_white_lb = results[results$outcome == "noinsurance" & results$race_group == "white",]$lb
ni_black_lb = results[results$outcome == "noinsurance" & results$race_group == "black",]$lb
ni_asian_lb = results[results$outcome == "noinsurance" & results$race_group == "asian",]$lb
ni_hispanic_lb = results[results$outcome == "noinsurance" & results$race_group == "hispanic",]$lb

ni_white_ub = results[results$outcome == "noinsurance" & results$race_group == "white",]$ub
ni_black_ub = results[results$outcome == "noinsurance" & results$race_group == "black",]$ub
ni_asian_ub = results[results$outcome == "noinsurance" & results$race_group == "asian",]$ub
ni_hispanic_ub = results[results$outcome == "noinsurance" & results$race_group == "hispanic",]$ub

# no visit
nv_white_rate = results[results$outcome == "novisit" & results$race_group == "white",]$rate
nv_black_rate = results[results$outcome == "novisit" & results$race_group == "black",]$rate
nv_asian_rate = results[results$outcome == "novisit" & results$race_group == "asian",]$rate
nv_hispanic_rate = results[results$outcome == "novisit" & results$race_group == "hispanic",]$rate

nv_white_lb = results[results$outcome == "novisit" & results$race_group == "white",]$lb
nv_black_lb = results[results$outcome == "novisit" & results$race_group == "black",]$lb
nv_asian_lb = results[results$outcome == "novisit" & results$race_group == "asian",]$lb
nv_hispanic_lb = results[results$outcome == "novisit" & results$race_group == "hispanic",]$lb

nv_white_ub = results[results$outcome == "novisit" & results$race_group == "white",]$ub
nv_black_ub = results[results$outcome == "novisit" & results$race_group == "black",]$ub
nv_asian_ub = results[results$outcome == "novisit" & results$race_group == "asian",]$ub
nv_hispanic_ub = results[results$outcome == "novisit" & results$race_group == "hispanic",]$ub

# no place
np_white_rate = results[results$outcome == "noplace" & results$race_group == "white",]$rate
np_black_rate = results[results$outcome == "noplace" & results$race_group == "black",]$rate
np_asian_rate = results[results$outcome == "noplace" & results$race_group == "asian",]$rate
np_hispanic_rate = results[results$outcome == "noplace" & results$race_group == "hispanic",]$rate

np_white_lb = results[results$outcome == "noplace" & results$race_group == "white",]$lb
np_black_lb = results[results$outcome == "noplace" & results$race_group == "black",]$lb
np_asian_lb = results[results$outcome == "noplace" & results$race_group == "asian",]$lb
np_hispanic_lb = results[results$outcome == "noplace" & results$race_group == "hispanic",]$lb

np_white_ub = results[results$outcome == "noplace" & results$race_group == "white",]$ub
np_black_ub = results[results$outcome == "noplace" & results$race_group == "black",]$ub
np_asian_ub = results[results$outcome == "noplace" & results$race_group == "asian",]$ub
np_hispanic_ub = results[results$outcome == "noplace" & results$race_group == "hispanic",]$ub

# bad care cost
bcc_white_rate = results[results$outcome == "badcarecost" & results$race_group == "white",]$rate
bcc_black_rate = results[results$outcome == "badcarecost" & results$race_group == "black",]$rate
bcc_asian_rate = results[results$outcome == "badcarecost" & results$race_group == "asian",]$rate
bcc_hispanic_rate = results[results$outcome == "badcarecost" & results$race_group == "hispanic",]$rate

bcc_white_lb = results[results$outcome == "badcarecost" & results$race_group == "white",]$lb
bcc_black_lb = results[results$outcome == "badcarecost" & results$race_group == "black",]$lb
bcc_asian_lb = results[results$outcome == "badcarecost" & results$race_group == "asian",]$lb
bcc_hispanic_lb = results[results$outcome == "badcarecost" & results$race_group == "hispanic",]$lb

bcc_white_ub = results[results$outcome == "badcarecost" & results$race_group == "white",]$ub
bcc_black_ub = results[results$outcome == "badcarecost" & results$race_group == "black",]$ub
bcc_asian_ub = results[results$outcome == "badcarecost" & results$race_group == "asian",]$ub
bcc_hispanic_ub = results[results$outcome == "badcarecost" & results$race_group == "hispanic",]$ub


plot_data <- data.frame(year = unique(results$year), ni_white_rate, ni_black_rate, ni_asian_rate, ni_hispanic_rate,
                        ni_white_lb, ni_black_lb, ni_asian_lb, ni_hispanic_lb,
                        ni_white_ub, ni_black_ub, ni_asian_ub, ni_hispanic_ub,
                        nv_white_rate, nv_black_rate, nv_asian_rate, nv_hispanic_rate,
                        nv_white_lb, nv_black_lb, nv_asian_lb, nv_hispanic_lb,
                        nv_white_ub, nv_black_ub, nv_asian_ub, nv_hispanic_ub,
                        np_white_rate, np_black_rate, np_asian_rate, np_hispanic_rate,
                        np_white_lb, np_black_lb, np_asian_lb, np_hispanic_lb,
                        np_white_ub, np_black_ub, np_asian_ub, np_hispanic_ub,
                        bcc_white_rate, bcc_black_rate, bcc_asian_rate, bcc_hispanic_rate,
                        bcc_white_lb, bcc_black_lb, bcc_asian_lb, bcc_hispanic_lb,
                        bcc_white_ub, bcc_black_ub, bcc_asian_ub, bcc_hispanic_ub)


colors <- c("Black" = "gray", "White" = "blue", "Asian" = "green", "Latino/Hispanic" = "orange")

# Define lighter colors
light_gray <- alpha("#8B8B83", 0.2)
light_blue <- alpha("#00FFFF", 0.2)
light_navy <- alpha("#4169E1", 0.2) 
light_orange <- alpha("#FF7F50", 0.2)

ni_plot <- ggplot(plot_data, aes(x = year)) + scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  scale_x_continuous(limits = c(1999, 2022), breaks = c(1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020)) + 
  geom_line(aes(y = ni_black_rate*100, color = "Black")) +
  geom_ribbon(aes(ymin = ni_black_lb*100, ymax = ni_black_ub*100, fill = "light_gray"), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes(y = ni_black_rate*100, color = "Black")) +
  geom_line(aes(y = ni_white_rate*100, color = "White")) +
  geom_ribbon(aes(ymin = ni_white_lb*100, ymax = ni_white_ub*100, fill = "light_blue"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = ni_white_rate*100, color = "White")) +
  geom_line(aes(y = ni_asian_rate*100, color = "Asian")) +
  geom_ribbon(aes(ymin = ni_asian_lb*100, ymax = ni_asian_ub*100, fill = "light_navy"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = ni_asian_rate*100, color = "Asian")) +
  geom_line(aes(y = ni_hispanic_rate*100, color = "Latino/Hispanic")) +
  geom_ribbon(aes(ymin = ni_hispanic_lb*100, ymax = ni_hispanic_ub*100, fill = "light_orange"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = ni_hispanic_rate*100, color = "Latino/Hispanic")) +
  scale_color_manual(values = c("Black" = "#8B8878", "White" = "#008B8B", "Asian" = "#27408B", "Latino/Hispanic" = "#CD5B45")) +
  scale_fill_manual(values = c(light_blue, light_gray, light_navy, light_orange)) + theme_pubclean() +
  labs(color = "Legend") +
  ggtitle("Uninsured") +
  ylab("Uninsured individuals, %") +
  xlab("Year of Interview") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

bcc_plot <- ggplot(plot_data, aes(x = year)) + scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  scale_x_continuous(limits = c(1999, 2022), breaks = c(1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020)) + 
  geom_line(aes(y = bcc_black_rate*100, color = "Black")) +
  geom_ribbon(aes(ymin = bcc_black_lb*100, ymax = bcc_black_ub*100, fill = "light_gray"), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes(y = bcc_black_rate*100, color = "Black")) +
  geom_line(aes(y = bcc_white_rate*100, color = "White")) +
  geom_ribbon(aes(ymin = bcc_white_lb*100, ymax = bcc_white_ub*100, fill = "light_blue"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = bcc_white_rate*100, color = "White")) +
  geom_line(aes(y = bcc_asian_rate*100, color = "Asian")) +
  geom_ribbon(aes(ymin = bcc_asian_lb*100, ymax = bcc_asian_ub*100, fill = "light_navy"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = bcc_asian_rate*100, color = "Asian")) +
  geom_line(aes(y = bcc_hispanic_rate*100, color = "Latino/Hispanic")) +
  geom_ribbon(aes(ymin = bcc_hispanic_lb*100, ymax = bcc_hispanic_ub*100, fill = "light_orange"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = bcc_hispanic_rate*100, color = "Latino/Hispanic")) +
  scale_color_manual(values = c("Black" = "#8B8878", "White" = "#008B8B", "Asian" = "#27408B", "Latino/Hispanic" = "#CD5B45")) +
  scale_fill_manual(values = c(light_blue, light_gray, light_navy, light_orange)) + theme_pubclean() +
  labs(color = "Legend") +
  ggtitle("Forgoing or delaying care due to cost in the past 12 mo") + ylab("Individuals forgoing or delaying care due to cost in the past, %") +
  xlab("Year of Interview") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

np_plot <- ggplot(plot_data, aes(x = year)) + scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  scale_x_continuous(limits = c(1999, 2022), breaks = c(1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020)) + 
  geom_line(aes(y = np_black_rate*100, color = "Black")) +
  geom_ribbon(aes(ymin = np_black_lb*100, ymax = np_black_ub*100, fill = "light_gray"), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes(y = np_black_rate*100, color = "Black")) +
  geom_line(aes(y = np_white_rate*100, color = "White")) +
  geom_ribbon(aes(ymin = np_white_lb*100, ymax = np_white_ub*100, fill = "light_blue"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = np_white_rate*100, color = "White")) +
  geom_line(aes(y = np_asian_rate*100, color = "Asian")) +
  geom_ribbon(aes(ymin = np_asian_lb*100, ymax = np_asian_ub*100, fill = "light_navy"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = np_asian_rate*100, color = "Asian")) +
  geom_line(aes(y = np_hispanic_rate*100, color = "Latino/Hispanic")) +
  geom_ribbon(aes(ymin = np_hispanic_lb*100, ymax = np_hispanic_ub*100, fill = "light_orange"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = np_hispanic_rate*100, color = "Latino/Hispanic")) +
  scale_color_manual(values = c("Black" = "#8B8878", "White" = "#008B8B", "Asian" = "#27408B", "Latino/Hispanic" = "#CD5B45")) +
  scale_fill_manual(values = c(light_blue, light_gray, light_navy, light_orange)) + theme_pubclean() +
  labs(color = "Legend") + 
  ggtitle("Without a usual source of care") + ylab("Individuals without a usual source of care, %") +
  xlab("Year of Interview") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

nv_plot <- ggplot(plot_data, aes(x = year)) + scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  scale_x_continuous(limits = c(1999, 2022), breaks = c(1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020)) + 
  geom_line(aes(y = nv_black_rate*100, color = "Black")) +
  geom_ribbon(aes(ymin = nv_black_lb*100, ymax = nv_black_ub*100, fill = "light_gray"), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes(y = nv_black_rate*100, color = "Black")) +
  geom_line(aes(y = nv_white_rate*100, color = "White")) +
  geom_ribbon(aes(ymin = nv_white_lb*100, ymax = nv_white_ub*100, fill = "light_blue"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = nv_white_rate*100, color = "White")) +
  geom_line(aes(y = nv_asian_rate*100, color = "Asian")) +
  geom_ribbon(aes(ymin = nv_asian_lb*100, ymax = nv_asian_ub*100, fill = "light_navy"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = nv_asian_rate*100, color = "Asian")) +
  geom_line(aes(y = nv_hispanic_rate*100, color = "Latino/Hispanic")) +
  geom_ribbon(aes(ymin = nv_hispanic_lb*100, ymax = nv_hispanic_ub*100, fill = "light_orange"), alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(y = nv_hispanic_rate*100, color = "Latino/Hispanic")) +
  scale_color_manual(values = c("Black" = "#8B8878", "White" = "#008B8B", "Asian" = "#27408B", "Latino/Hispanic" = "#CD5B45")) +
  scale_fill_manual(values = c(light_blue, light_gray, light_navy, light_orange)) + theme_pubclean() +
  labs(color = "Legend") +
  ggtitle("Not seen or talked to a health\nprofessional in the past 12 mo") + ylab("Individuals who had not seen or talked to a\nhealth professional in the past 12 mos, %") +
  xlab("Year of Interview") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggarrange(ni_plot, np_plot, nv_plot, bcc_plot, ncol = 2, nrow = 2, common.legend = TRUE, legend= "top")