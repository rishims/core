# LISTEN LC Figures
# Rishi Shah
# August 1, 2023

# load libraries
library(broom)
library(data.table)
library(haven)
library(tidyverse)
library(labelled)
library(naniar)
library(corrplot)
library(gtsummary)
library(gt)
library(reshape2)
library(scales)


# import data (these won't work as the data is private to be HIPAA compliant)
data_symp <- fread('/Volumes/LISTEN_Study/analysis/shah/sx_vacc_covid_dx_or_07072023.csv')
data_init <- fread("/Volumes/LISTEN_Study/analysis/shah/initial_07072023.csv", sep = ",")
data_nmed <- fread("/Volumes/LISTEN_Study/analysis/shah/non_medical_07072023.csv", sep = "|")
data_treat <- fread('/Volumes/LISTEN_Study/analysis/shah/treat_07072023.csv')
data_hosp <- fread('/Volumes/LISTEN_Study/analysis/warner/initial_extra_07072023.csv')

# merge data
full_data <- left_join(data_symp, data_init, by = 'userId')
full_data <- left_join(full_data, data_nmed, by = 'userId')
full_data <- left_join(full_data, data_treat, by = 'userId')
full_data <- left_join(full_data, data_hosp, by = 'userId')

# filter for long covid data
lc_only <- subset(full_data, label == 'LC')

# plot densities for condition
library(plyr)
library(ggsci)

med1 <- ddply(full_data, "label", summarise, grp.median = median(health_vas))

eq_vas_cond <- ggplot(full_data, aes(x=health_vas, color=label)) +
  geom_density(size = 1) +  # Increase line width
  geom_vline(data = med1, aes(xintercept = grp.median, color=label),
             linetype="dashed", size = 1) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Condition") + 
  xlim(0, 100) +
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

eq_vas_cond <- ggpar(eq_vas_cond, legend = "right", legend.title = "Condition") + 
  theme(legend.position = c(0.9, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

med2 <- ddply(subset(full_data, !is.na(nonmed_worstdays)), "label", summarise, grp.median = median(nonmed_worstdays))

wd_cond <- ggplot(subset(full_data, !is.na(nonmed_worstdays)), aes(x=nonmed_worstdays, color=label)) +
  geom_density(size = 1) +  # Increase line width
  geom_vline(data = med2, aes(xintercept = grp.median, color=label),
             linetype="dashed", size = 1) + theme_pubr() + 
  labs(x = "Symptom Severity", 
       y = "Density", 
       title = "Symptom Severity by Condition") + 
  xlim(0, 100) + ylim(0, 0.030) +
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

wd_cond <- ggpar(wd_cond, legend = "right", legend.title = "Condition") + 
  theme(legend.position = c(0.15, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

eq_vas_cond
wd_cond


healthstat_plots <- ggarrange(eq_vas_cond, wd_cond, labels = c("A", "B"),
                              nrow = 2, ncol = 1)

ggsave("healthstat_plots_v1.jpeg", plot = healthstat_plots, device = "jpeg", width = 8, height = 8, dpi = 320)


# health status plots for LC
# plot densities for whole group for EQ-VAS
eq_vas <- ggplot(lc_only, aes(x=health_vas)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = lc_only, aes(xintercept = median(health_vas)),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score for\nIndividuals Reporting Long COVID") + 
  ylim(0, 0.020) + xlim(0, 100) + 
  scale_color_jama(palette = "default") + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# plot densities for gender
library(plyr)
library(ggsci)

medx <- ddply(subset(lc_only, gender %in% c("Male", "Female")), 
              "gender", summarise, grp.median = median(health_vas))

eq_vas_gender <- ggplot(subset(lc_only, gender %in% c("Male", "Female")), 
                        aes(x=health_vas, color=gender)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = medx, aes(xintercept = grp.median, color=gender),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Gender") + 
  xlim(0, 100) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


eq_vas_gender <- ggpar(eq_vas_gender, legend = "right", legend.title = "Gender") + 
  theme(legend.position = c(0.9, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# plot densities for age
lc_only$ageStat <- NA
lc_only$ageStat[(lc_only$age <= 59)] <- "Young"
lc_only$ageStat[(lc_only$age >= 60)] <- "Old"
medx2 <- ddply(lc_only, "ageStat", summarise, grp.median2 = median(health_vas))

eq_vas_age <- ggplot(lc_only, aes(x=health_vas, color=ageStat)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = medx2, aes(xintercept = grp.median2, color=ageStat),
             linetype="dashed", size = 1) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Age Group") + 
  xlim(0, 100) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


eq_vas_age <- ggpar(eq_vas_age, legend = "right", legend.title = "Age Group") + 
  theme(legend.position = c(0.9, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# infection stuff
lc_only$index_infection <- factor(lc_only$index_infection, c("Pre-Delta", "Delta", "Omicron", "Post-Omicron"))
medx3 <- ddply(subset(lc_only, !is.na(index_infection)), "index_infection", 
               summarise, grp.median3 = IQR(health_vas))

calculate_iqr <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- paste("(", q1, ",", q3, ")", sep = "")
  return(iqr)
}

medx3 <- ddply(subset(lc_only, !is.na(index_infection)), "index_infection", 
               summarise, grp.iqr = calculate_iqr(health_vas))

eq_vas_infc <- ggplot(subset(lc_only, !is.na(index_infection)),
                      aes(x=health_vas, color=index_infection)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx3, aes(xintercept = grp.median3, color=index_infection),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Index Infection Period") + 
  xlim(0, 100) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


eq_vas_infc <- ggpar(eq_vas_infc, legend = "right", legend.title = "Index Vaccination\nPeriod") + 
  theme(legend.position = c(0.88, 0.75), legend.title=element_text(size=16)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

eq_vas_plots <- ggarrange(eq_vas, eq_vas_gender, eq_vas_age, eq_vas_infc, 
                          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)



ggsave("eq_vas_plots_lc_1.jpeg", plot = eq_vas_plots, device = jpeg, width = 15, height = 15, dpi = 320)

# for worst_days question
wd <- ggplot(subset(lc_only, !is.na(nonmed_worstdays)), aes(x=nonmed_worstdays)) +
  geom_density(size = 1.2) + scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) +
  geom_vline(data = subset(lc_only, !is.na(nonmed_worstdays)), aes(xintercept = median(nonmed_worstdays)),
             linetype="dashed", size = 1.2) + theme_pubr() + labs(x = "Symptom Severity", y = "Density", title = "Symptom Severity for Individuals Reporting Long COVID") + xlim(0, 100) + scale_color_jama(palette = "default")+ theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

# plot densities for gender
medx1 <- ddply(subset(lc_only, !is.na(nonmed_worstdays) & gender %in% c("Male", "Female")),
               "gender", summarise, grp.median = median(nonmed_worstdays))

wd_gender <- ggplot(subset(lc_only, !is.na(nonmed_worstdays) & gender %in% c("Male", "Female")), 
                    aes(x = nonmed_worstdays, color = gender)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx1, aes(xintercept = grp.median, 
                               color = gender, linetype = gender),
             show.legend = FALSE, size = 1.2) +
  scale_linetype_manual(values = c("Male" = "dashed", "Female" = "solid")) +
  theme_pubr() +
  labs(x = "Symptom Severity",
       y = "Density",
       title = "Symptom Severity by Gender") +
  xlim(0, 100) +
  scale_color_jama(palette = "default") +
  scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

wd_gender <- ggpar(wd_gender, legend = "right", legend.title = "Gender") +
  theme(legend.position = c(0.15, 0.85)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# plot densities for age
medx2_2 <- ddply(subset(lc_only, !is.na(nonmed_worstdays)), "ageStat", summarise, grp.median2 = median(nonmed_worstdays))

wd_age <- ggplot(subset(lc_only, !is.na(nonmed_worstdays)), aes(x = nonmed_worstdays, color = ageStat)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx2_2, aes(xintercept = grp.median2, color = ageStat),
             linetype = "dashed", size = 1.2) +
  theme_pubr() +
  labs(x = "Symptom Severity",
       y = "Density",
       title = "Symptom Severity by Age Group") +
  xlim(0, 100) +
  scale_color_jama(palette = "default") +
  scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

wd_age <- ggpar(wd_age, legend = "right", legend.title = "Age Group") +
  theme(legend.position = c(0.15, 0.85)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# infection stuff
medx3_3 <- ddply(subset(lc_only, !is.na(nonmed_worstdays) & !is.na(index_infection)),
                 "index_infection", summarise, grp.median3 = median(nonmed_worstdays))

wd_infc <- ggplot(subset(lc_only, !is.na(nonmed_worstdays) & !is.na(index_infection)),
                  aes(x = nonmed_worstdays, color = index_infection)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx3_3, aes(xintercept = grp.median3, color = index_infection),
             linetype = "dashed", size = 1.2) +
  scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) +  
  theme_pubr() +
  labs(x = "Symptom Severity",
       y = "Density",
       title = "Symptom Severity by Index Infection Period") +
  xlim(0, 100) +
  scale_color_jama(palette = "default") + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

wd_infc <- ggpar(wd_infc, legend = "right", legend.title = "Index Infection\nPeriod") + 
  theme(legend.position = c(0.15, 0.85), legend.title=element_text(size=16)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

wd_plots <- ggarrange(wd, wd_gender, wd_age, wd_infc, labels = c("A", "B", "C", "D"),
                      nrow = 2, ncol = 2)

ggsave("wd_plots_lc_v1.jpeg", plot = wd_plots, device = "jpeg", width = 15, height = 15, dpi = 320)


pure health status plots - ref from NHIS?
  
  
lc_only$health_new <- factor(lc_only$health_new, 
                             c("Excellent", "Very good", "Good", "Fair", "Poor", "Don’t know")) # 16 don't know
table(lc_only$health_new)

lc_only$health_new <- recode(lc_only$health_new, "'Don’t know' = NA")
lc_only$health_new <- factor(lc_only$health_new, 
                             c("Excellent", "Very good", "Good", "Fair", "Poor"))

health_new_gender <- ggplot(subset(lc_only, !is.na(health_new) & gender %in% c("Male", "Female")), aes(fill=health_new, y=1, x=gender)) + 
  geom_bar(position="fill", stat="identity") + theme(legend.position = "right") + theme_pubr() + labs(title = "Self-reported Health Status by Gender",
                                                                                                      x = "Gender",
                                                                                                      y = "",
                                                                                                      fill = "Health Status") + scale_fill_jama(palette = "default") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

health_new_age <- ggplot(subset(lc_only, !is.na(health_new)), aes(fill=health_new, y=1, x=as.factor(ageStat))) + 
  geom_bar(position="fill", stat="identity") + theme(legend.position = "right") + theme_pubr() + 
  labs(title = "Self-reported Health Status by Age Group",
       x = "Age Group",
       y = "",
       fill = "Health Status") + scale_fill_jama(palette = "default") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

health_new_infc <- ggplot(subset(lc_only, !is.na(health_new) & !is.na(index_infection)),
                          aes(fill = health_new, y = 1, x = index_infection)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_jama(palette = "default") +
  theme(legend.position = "right") +
  theme_pubr() +
  labs(title = "Self-reported Health Status by Index Infection Period",
       x = "Index Infection Period",
       y = "",
       fill = "Health Status") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# subset the data to remove any missing values of "health_new"
data <- subset(lc_only, health_new %in% c("Excellent", "Very good", "Good", "Fair", "Poor"))

# calculate the proportions of each category
category_proportions <- table(data$health_new) / length(data$health_new) * 100

# convert the proportions and categories to a data frame
bar_data <- data.frame(categories = names(category_proportions), proportions = category_proportions)
bar_data$categories <- factor(bar_data$categories, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))

health_all <- ggplot(subset(bar_data, categories != "Don't know"), aes(x = categories, y = proportions.Freq, fill = proportions.Var1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Self-reported Health Status for\nIndividuals Reporting Long COVID",
       x = "Health Status", y = "",
       fill = "Health Status") +
  theme_pubr() + scale_fill_jama(palette = "default") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +   
  theme(legend.text = element_text(size = 16), 
        legend.title = element_text(size = 18), 
        legend.key.size = unit(1.25, 'cm'))


# define a function to format labels as percentages
percent_labels <- function(x) {
  percent(x, scale = 100)
}

health_new_gender <- health_new_gender +
  scale_y_continuous(labels = percent_labels)

health_new_age <- health_new_age +
  scale_y_continuous(labels = percent_labels)

health_new_infc <- health_new_infc +
  scale_y_continuous(labels = percent_labels)

health_new_all <- health_all +
  scale_y_continuous(labels = percent_labels)

health_plots <- ggarrange(health_new_all, health_new_gender, health_new_age, health_new_infc,
                          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2, common.legend = TRUE) 

ggsave("health_new_plots_lc_v1.jpeg", plot = health_plots, device = jpeg, width = 14.1, height = 14.1, dpi = 320)
