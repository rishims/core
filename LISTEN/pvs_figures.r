# LISTEN PVS Figures
# Rishi Shah
# July 27, 2023
# This code reads in LISTEN data regarding participants experiencing post-vaccination syndrome (PVS) and produces
# figures relating to health status, EQ-VAS quality of life scores, and symptom severity. All metrics are also stratified
# by gender, age group, and index vaccination type.


# Read in all relevant data (these won't work as the data is secured to be HIPAA compliant)
data_vaccineinjury_symp <- fread('/Volumes/LISTEN_Study/analysis/shah/sx_vacc_covid_dx_or_07072023.csv')
data_vaccineinjury_init <- fread("/Volumes/LISTEN_Study/analysis/shah/initial_07072023.csv", sep = ",")
data_vaccineinjury_nmed <- fread("/Volumes/LISTEN_Study/analysis/shah/non_medical_07072023.csv", sep = "|")
data_vaccineinjury_treat <- fread('/Volumes/LISTEN_Study/analysis/shah/treat_07072023.csv')

# Filter to only include those that have PVS but do not have long covid
data_vaccineinjury_symp <- subset(data_vaccineinjury_symp, label == 'VI')
data_vaccineinjury_init <- subset(data_vaccineinjury_init, userId %in% data_vaccineinjury_symp$userId)
data_vaccineinjury_nmed <- subset(data_vaccineinjury_nmed, userId %in% data_vaccineinjury_symp$userId)
data_vaccineinjury_treat <- subset(data_vaccineinjury_treat, userId %in% data_vaccineinjury_symp$userId)

# merge data
merged <- left_join(data_vaccineinjury_symp, data_vaccineinjury_init, by = 'userId')
merged <- left_join(merged, data_vaccineinjury_nmed, by = 'userId')
merged <- left_join(merged, data_vaccineinjury_treat, by = 'userId')

# Age categories
library(hmisc)
merged$ageStat <- NA
merged$ageStat[(merged$age <= 59)] <- "Young"
merged$ageStat[(merged$age >= 60)] <- "Old"

label(merged$ageStat) <- "Old/Young"
levels(merged$ageStat) <- c("Young", "Old")

# Refactor 'health_new'
merged$health_new <- factor(merged$health_new, 
                            c("Excellent", "Very good", "Good", "Fair", "Poor", "Don't know"))
# table(merged$health_new)

health_new_gender <- ggplot(subset(merged, !is.na(health_new)), aes(fill=health_new, y=1, x=gender)) + 
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

health_new_age <- ggplot(subset(merged, !is.na(health_new)), aes(fill=health_new, y=1, x=as.factor(ageStat))) + 
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

health_new_vax <- ggplot(subset(merged, !is.na(health_new) & index_vax_type %in% c("Pfizer", "Moderna")),
                         aes(fill = health_new, y = 1, x = index_vax_type)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_jama(palette = "default") +
  theme(legend.position = "right") +
  theme_pubr() +
  labs(title = "Self-reported Health Status by Index Vaccination Type",
       x = "Index Vaccination Type",
       y = "",
       fill = "Health Status") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# Subset the data to remove any missing values of "health_new"
data <- subset(merged, health_new %in% c("Excellent", "Very good", "Good", "Fair", "Poor"))

# Calculate the proportions of each category
category_proportions <- table(data$health_new) / length(data$health_new) * 100

# Convert the proportions and categories to a data frame
bar_data <- data.frame(categories = names(category_proportions), proportions = category_proportions)
bar_data$categories <- factor(bar_data$categories, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))

health_all <- ggplot(subset(bar_data, categories != "Don't know"), aes(x = categories, y = proportions.Freq, fill = proportions.Var1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Self-reported Health Status for Individuals Reporting PVS",
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

# Define a custom function to format labels as percentages
library(scales)
percent_labels <- function(x) {
  percent(x, scale = 100)
}

# For health_new_gender plot
health_new_gender <- health_new_gender +
  scale_y_continuous(labels = percent_labels)

# For health_new_age plot
health_new_age <- health_new_age +
  scale_y_continuous(labels = percent_labels)

# For health_new_vax plot
health_new_vax <- health_new_vax +
  scale_y_continuous(labels = percent_labels)

# For health_new_all plot
health_new_all <- health_new_all +
  scale_y_continuous(labels = percent_labels)

# Arrange all the plots together and save results
health_plots <- ggarrange(health_new_all, health_new_gender, health_new_age, health_new_vax,
                          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2, common.legend = TRUE) 


ggsave("health_new_plots.jpeg", plot = health_plots, device = jpeg, width = 14.1, height = 14.1, dpi = 320)


# Plot densities for whole group for EQ-VAS
eq_vas <- ggplot(merged, aes(x=health_vas)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = merged, aes(xintercept = median(health_vas)),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score for Individuals Reporting PVS") + 
  ylim(0, 0.020) + xlim(0, 100) + 
  scale_color_jama(palette = "default") + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# Plot densities for gender
library(plyr)
library(ggsci)

medx <- ddply(merged, "gender", summarise, grp.median = median(health_vas))

eq_vas_gender <- ggplot(merged, aes(x=health_vas, color=gender)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = medx, aes(xintercept = grp.median, color=gender),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Gender") + 
  xlim(0, 100) + ylim(0, 0.020) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


eq_vas_gender <- ggpar(eq_vas_gender, legend = "right", legend.title = "Gender") + 
  theme(legend.position = c(0.9, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot densities for age
medx2 <- ddply(merged, "ageStat", summarise, grp.median2 = median(health_vas))

eq_vas_age <- ggplot(merged, aes(x=health_vas, color=ageStat)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = medx2, aes(xintercept = grp.median2, color=ageStat),
             linetype="dashed", size = 1) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Age Group") + 
  xlim(0, 100) + ylim(0, 0.020) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


eq_vas_age <- ggpar(eq_vas_age, legend = "right", legend.title = "Age Group") + 
  theme(legend.position = c(0.9, 0.85)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Plot densities for index vaccination type
library(car)
merged$vacc1_date <- as.Date(substring(merged$vacc1_date, first = 0, last = 10))
merged$vacc2_date <- as.Date(substring(merged$vacc2_date, first = 0, last = 10))
merged$vacc3_date <- as.Date(substring(merged$vacc3_date, first = 0, last = 10))
merged$vacc4_date <- as.Date(substring(merged$vacc4_date, first = 0, last = 11))
merged$vacc5_date <- as.Date(substring(merged$vacc5_date, first = 0, last = 10))

merged$index_vax_type <- NA
merged$index_vax_date <- NA
merged$index_vax <- recode(merged$dose_vax, "'1st shot' = 1; '2nd shot' = 2; '3rd shot' = 3; '4th shot' = 4; '5th shot' = 5")

for (i in 1:nrow(merged)){
  merged$index_vax_type[i] <- switch(merged$index_vax[i], merged$vacc1_type[i], 
                                     merged$vacc2_type[i], merged$vacc3_type[i],
                                     merged$vacc4_type[i], merged$vacc5_type[i])
  
  merged$index_vax_date[i] <- as.character(switch(merged$index_vax[i], merged$vacc1_date[i],
                                                  merged$vacc2_date[i], merged$vacc3_date[i],
                                                  merged$vacc4_date[i], merged$vacc5_date[i]))
  
}

medx3 <- ddply(subset(merged, index_vax_type %in% c("Pfizer", "Moderna")),
               "index_vax_type", summarise, grp.median3 = median(health_vas))

eq_vas_vax <- ggplot(subset(merged, index_vax_type %in% c("Pfizer", "Moderna")),
                     aes(x=health_vas, color=index_vax_type)) +
  geom_density(size = 1.2) +  # Increase line width
  geom_vline(data = medx3, aes(xintercept = grp.median3, color=index_vax_type),
             linetype="dashed", size = 1.2) + theme_pubr() + 
  labs(x = "Euro-QoL Visual Analogue Score", 
       y = "Density", 
       title = "Euro-QoL Visual Analogue Score by Index Vaccination Type") + 
  xlim(0, 100) + 
  scale_color_jama(palette = "default") +
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

eq_vas_vax <- ggpar(eq_vas_vax, legend = "right", legend.title = "Index Vaccination Type") + 
  theme(legend.position = c(0.8, 0.85), legend.title=element_text(size=12)) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange and save EQ-VAS plots
eq_vas_plots <- ggarrange(eq_vas, eq_vas_gender, eq_vas_age, eq_vas_vax, 
                          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)


ggsave("eq_vas_plots_3.jpeg", plot = eq_vas_plots, device = jpeg, width = 15, height = 15, dpi = 320)


# Plot densities for worst days question or symptom severity
wd <- ggplot(subset(merged, !is.na(nonmed_worstdays)), aes(x=nonmed_worstdays)) +
  geom_density(size = 1.2) + scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) +
  geom_vline(data = subset(merged, !is.na(nonmed_worstdays)), aes(xintercept = median(nonmed_worstdays)),
             linetype="dashed", size = 1.2) + theme_pubr() + labs(x = "Symptom Severity", y = "Density", title = "Symptom Severity for Individuals Reporting PVS") + xlim(0, 100) + scale_color_jama(palette = "default")+ theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

# Plot densities for gender
medx <- ddply(subset(merged, !is.na(nonmed_worstdays)), "gender", summarise, grp.median = median(nonmed_worstdays))

wd_gender <- ggplot(subset(merged, !is.na(nonmed_worstdays)), aes(x = nonmed_worstdays, color = gender)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx, aes(xintercept = grp.median, color = gender),
             linetype = "dashed", size = 1.2) +
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

# Plot densities for age
medx2 <- ddply(subset(merged, !is.na(nonmed_worstdays)), "ageStat", summarise, grp.median2 = median(nonmed_worstdays))

wd_age <- ggplot(subset(merged, !is.na(nonmed_worstdays)), aes(x = nonmed_worstdays, color = ageStat)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx2, aes(xintercept = grp.median2, color = ageStat),
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

# Plot densities for index vaccination type
medx3 <- ddply(subset(merged, !is.na(nonmed_worstdays) & index_vax_type %in% c("Pfizer", "Moderna")),
               "index_vax_type", summarise, grp.median3 = median(nonmed_worstdays))

wd_vax <- ggplot(subset(merged, !is.na(nonmed_worstdays) & index_vax_type %in% c("Pfizer", "Moderna")),
                 aes(x = nonmed_worstdays, color = index_vax_type)) +
  geom_density(size = 1.2) +
  geom_vline(data = medx3, aes(xintercept = grp.median3, color = index_vax_type, linetype = index_vax_type),
             show.legend = FALSE, size = 1.2) +
  scale_linetype_manual(values = c("Pfizer" = "dashed", "Moderna" = "solid")) +
  theme_pubr() +
  labs(x = "Symptom Severity",
       y = "Density",
       title = "Symptom Severity by Index Vaccination Type") +
  xlim(0, 100) +
  scale_color_jama(palette = "default") + scale_y_continuous(breaks = c(0, 0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) + 
  theme(legend.position = c(0.9, 0.85),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

wd_vax <- ggpar(wd_vax, legend = "right", legend.title = "Index Vaccination Type") +
  theme(legend.position = c(0.25, 0.85), legend.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange and save worst days or symptom severity plots
wd_plots <- ggarrange(wd, wd_gender, wd_age, wd_vax, labels = c("A", "B", "C", "D"),
                      nrow = 2, ncol = 2)

ggsave("wd_plots_v2.jpeg", plot = wd_plots, device = "jpeg", width = 15, height = 15, dpi = 320)
