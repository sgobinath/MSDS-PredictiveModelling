install.packages("pwr")
library(pwr)
library(dplyr)

# Get the effect size of t test
effect_size <- cohen.ES(test = "t", size = "large")$effect.size
effect_size

# Perform power analysis to get the number of samples required
power_analysis <- pwr.t.test(d = effect_size, power = 0.8, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
power_analysis
round(power_analysis$n)

# Plot the power analysis result
plot(power_analysis)

# Get the mortality count data set with Cause of death, Male mortality count and Female mortality count as the attributes.
df_gender_mortality <- read.csv("Data/GenderMortality.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(df_gender_mortality)

# Get the random sample of data from the dataset based on the count from power analysis
df_sample <- sample_n(df_gender_mortality, round(power_analysis$n))

# Perform statistical to get p value
t.test(df_sample$Male.Mortality.Count, df_sample$Female.Mortality.Count, alternative = "two.sided", var.equal = FALSE)

