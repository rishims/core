# NHIS Pre-Processcing Code
# Data source is the National Health Interview Survey from the year 1999 to 2021
# Rishi Shah
# May 13, 2023

library(foreign)
library(dplyr)
library(survey)

# Load the data
df <- read.csv("nhis_99-21.csv") # the exact data extract can be produced by visiting https://www.cdc.gov/nchs/nhis/data-questionnaires-documentation.htm
names(df) <- tolower(names(df))

# Keeping only sample adult
df <- df[df$astatflg == 1, ]

# Correction for pooled years
df$sampweight_ipums_pooled <- df$sampweight / 23
df$strata_ipums_pooled <- df$strata / 23
df$psu_ipums_pooled <- df$psu / 23

# Survey set
library(survey)

design_1 <- svydesign(
  ids = ~psu_ipums_pooled,
  strata = ~strata_ipums_pooled,
  weights = ~sampweight_ipums_pooled,
  data = df,
  nest = TRUE
)

######################################
# 1. Independent variables           #
######################################

# 1.1 Race/Ethnicity
df$race <- NA
df$race[df$racea == 100 & df$hispyn == 1] <- 1
df$race[df$racea == 200 & df$hispyn == 1] <- 3
df$race[(df$racea > 201 & df$racea < 889) & df$hispyn == 1] <- 7
df$race[(df$racea >= 400 & df$racea < 500) & df$hispyn == 1] <- 5
df$race[df$hispyn == 2] <- 9

levels <- c("NH White", "NH Black", "NH Asian", "NH Other", "Hispanic")
labels <- c(1, 3, 5, 7, 9)
df$race <- factor(df$race, levels = labels, labels = levels)

df$white <- ifelse(df$race == "NH White", 1, 0)
df$black <- ifelse(df$race == "NH Black", 1, 0)
df$asian <- ifelse(df$race == "NH Asian", 1, 0)
df$otherrace <- ifelse(df$race == "NH Other", 1, 0)
df$hispanic <- ifelse(df$race == "Hispanic", 1, 0)
df$allfour <- ifelse(df$race %in% c("NH White", "NH Black", "NH Asian", "Hispanic"), 1, 0)

# 1.2 Multiply imputed income level indicators
for (G in 1:5) {
  df[, paste0("lowincome_", G)] <- ifelse(df[, paste0("povimp", G)] >= 8 & df[, paste0("povimp", G)] != 98, 0, 1)
  df[, paste0("mhincome_", G)] <- ifelse(df[, paste0("lowincome_", G)] == 0, 1, 0)
  df[, paste0("highincome_", G)] <- ifelse(df[, paste0("povimp", G)] < 12, 0, 1)
}

######################################
# 2. Dependent variables             #
######################################

# 2.1 No insurance coverage
df$noinsurance <- ifelse(df$hinotcove == 1, 0, ifelse(df$hinotcove == 2, 1, NA))

# 2.2 Foregone or delayed medical care due to cost
df$nocarecost <- ifelse(df$ybarcare == 1, 0, ifelse(df$ybarcare == 2, 1, NA))
df$latecarecost <- ifelse(df$delaycost == 1, 0, ifelse(df$delaycost == 2, 1, NA))
df$nomedscost <- ifelse(df$ybarmeds %in% c(0, 1), 0, ifelse(df$ybarmeds == 2, 1, NA))
df$badcarecost <- ifelse(df$nocarecost == 0 & df$latecarecost == 0 & df$nomedscost == 0, 0, 1)

# 2.3 No usual source of care
df$noplace <- ifelse(df$usualpl %in% c(2, 3), 0, ifelse(df$usualpl == 1 | df$typplsick == 200, 1, NA))

# 2.4 Not seen/talked to a health care professional in the past year
df$novisit <- ifelse(df$dvint >= 200 & df$dvint < 300, 0, ifelse(df$dvint == 100 | (df$dvint >= 300 & df$dvint < 997), 1, NA))

# Create 'female' variable and set initial value to 0
df$female <- 0

# Replace 'female' with 1 if 'sex' is equal to 2
df$female[df$sex == 2] <- 1

# Create 'agecat' variable and set initial value to missing
df$agecat <- NA

# Replace 'agecat' based on age categories
df$agecat[df$age < 40] <- 1
df$agecat[df$age >= 40 & df$age < 65] <- 3
df$agecat[df$age >= 65] <- 5

# Label 'agecat' variable
library(Hmisc)
label(df$agecat) <- "Age Category"
levels(df$agecat) <- c("18–39 years", "40–64 years", "≥65 years")

# Create 'yearcat' variable and set initial value to missing
df$yearcat <- NA

# Replace 'yearcat' based on groups of years
df$yearcat[df$year < 2004] <- 1
df$yearcat[df$year >= 2004 & df$year < 2009] <- 3
df$yearcat[df$year >= 2009 & df$year < 2013] <- 7
df$yearcat[df$year >= 2013] <- 9

# Label 'yearcat' variable
label(df$yearcat) <- "Years"
levels(df$yearcat) <- c("1999–2003", "2004–2008", "2009–2013", "2014–2018")

# Create 'firstyears' variable and set initial value to 0
df$firstyears <- 0

# Replace 'firstyears' based on specific years
df$firstyears[df$year %in% c(1999, 2000)] <- 1

# Label 'firstyears' variable
label(df$firstyears) <- "1999-2000"
levels(df$firstyears) <- c("No, other yrs", "Yes")

# Create 'midyears' variable and set initial value to 0
df$midyears <- 0

# Replace 'midyears' based on specific years
df$midyears[df$year %in% c(2008, 2009)] <- 1

# Label 'midyears' variable
label(df$midyears) <- "2008-2009"
levels(df$midyears) <- c("No, other yrs", "Yes")

# Create 'lastyears' variable and set initial value to 0
df$lastyears <- 0

# Replace 'lastyears' based on specific years
df$lastyears[df$year %in% c(2017, 2018)] <- 1

# Label 'lastyears' variable
label(df$lastyears) <- "2017-2018"
levels(df$lastyears) <- c("No, other yrs", "Yes")

# Create single year indicators
for (i in 1999:2018) {
  df[paste0("year_", i)] <- ifelse(df$year == i, 1, 0)
}

# Create region indicators
df$neast <- ifelse(df$region == 1, 1, 0)
df$midwest <- ifelse(df$region == 2, 1, 0)
df$south <- ifelse(df$region == 3, 1, 0)
df$west <- ifelse(df$region == 4, 1, 0)

# Exclude observations without race/ethnicity data
df <- df[!(df$hispyn > 6 | df$racea >= 900), ]

# Exclude those that identified as Other Race
df <- df[df$race != 7, ]

# Center age, sex, and region variables by their study population mean
df$age <- df$age - mean(df$age, na.rm = TRUE)
df$neast <- df$neast - mean(df$neast, na.rm = TRUE)
df$midwest <- df$midwest - mean(df$midwest, na.rm = TRUE)
df$south <- df$south - mean(df$south, na.rm = TRUE)
df$west <- df$west - mean(df$west, na.rm = TRUE)
df$female <- df$female - mean(df$female, na.rm = TRUE)

# Save the modified data frame to a new file
write.csv(df, "nhis_final.csv", row.names = FALSE)