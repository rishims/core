# Main Analysis on Outcomes
# Rishi Shah
# May 13, 2023

library(boot)
library(survey)

# Load the processed dataset
nhis_final <- read.csv("nhis_final.csv")

# Set the seed for reproducibility
set.seed(202305013)

# Define the outcomes
outcomes <- c("noinsurance", "badcarecost", "noplace", "novisit")

# Create an empty data frame to store the results
results <- data.frame(outcome = character(), race_group = character(), year = integer(), 
                      rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                      stringsAsFactors = FALSE)

# Perform main analysis for each outcome
for (outcome in outcomes) {
  for (race_group in c("white", "black", "asian", "hispanic", "allfour")) {
    temp <- nhis_final[nhis_final[, race_group] == 1, ]
    temp$year <- as.factor(temp$year)
    svy_design <- svydesign(ids = ~psu_ipums_pooled, strata = ~strata_ipums_pooled, 
                            weights = ~sampweight_ipums_pooled, data = temp, nest = TRUE)
    formula <- paste(outcome, "~ age + female + neast + midwest + south + west + year")
    
    fit <- svyglm(as.formula(formula), design = svy_design, data = temp, family = quasibinomial(link = 'logit'))
    
    # Get the coefficients and their corresponding indices
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    coef_names <- names(b)
    coef_indices <- match(coef_names, colnames(draws))
    
    # Subset the draws matrix using the correct indices
    subset_draws <- draws[, coef_indices]
    
    for (year in 1999:2021) {
      n <- year - 2000 + 7
      
      if(year == 1999)
      {
        rates <- inv.logit(subset_draws[, 1])
      }
      else
      {
        rates <- inv.logit(subset_draws[, n] + subset_draws[, 1])
      }
      
      rate_mean <- mean(rates)
      rate_sd <- sd(rates)
      rate_lb <- quantile(rates, 0.025)
      rate_ub <- quantile(rates, 0.975)
      
      results <- rbind(results, data.frame(outcome = outcome, race_group = race_group, year = year, rate = rate_mean,
                                           stderr = rate_sd, lb = rate_lb, ub = rate_ub,
                                           stringsAsFactors = FALSE))
    }
  }
}

# Save the results to a file
write.csv(results, file = "ratefile.csv", row.names = FALSE)