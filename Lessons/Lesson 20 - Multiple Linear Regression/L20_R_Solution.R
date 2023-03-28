library(tidyverse)
library(janitor)

bloodpressure <- read_csv("blood_pressure.csv")

BP_HT_model = bloodpressure %>% 
  lm(SBP ~ HT, data = .)

summary(BP_HT_model)

BP_HT_S_model = bloodpressure %>% 
  lm(SBP ~ HT + SEX, data = .)

summary(BP_HT_S_model)


##############

df <- read_csv("ACFT2.csv")

colnames(df)

# Explore SDC vs IOCT Time 

df %>% 
  ggplot(aes(x = SDC_Raw, y = IOCT_time)) +
  geom_point() +
  labs(x = "SDC Time (mins)", y = "IOCT Time (mins)", title = "SDC vs IOCT Time")

cor(df$SDC_Raw, df$IOCT_time)

# Make regression model

lrmodel <- df %>%
  lm(IOCT_time ~ SDC_Raw, data = .)
summary(lrmodel)

# Check validity conditions
  
lrmodel %>%
  fortify(lrmodel$model) %>%
  ggplot(aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0) +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs. Predicted Values")
  
lrmodel %>%
  fortify(lrmodel$model) %>%
  mutate(row = row_number()) %>%
  ggplot(aes(x = row, y = .resid)) + geom_point() + geom_hline(yintercept = 0) +
  labs(x = "Order of Occurence", y = "Residuals", title = "Residuals in Order of Occurence")
  
lrmodel %>%
  fortify(lrmodel$model) %>%
  ggplot(aes(x = .resid)) + geom_histogram() + labs(x = "Residuals", title = "Histogram of Residuals")
  
## Looking at MDL as confounding variable

df %>% 
  ggplot(aes(x = MDL_Raw, y = IOCT_time)) +
  geom_point() +
  labs(x = "Max Deadlift Raw (lbs)", y = "IOCT Time (mins)", title = "MDL vs IOCT Time")

df %>% 
  ggplot(aes(x = MDL_Raw, y = SDC_Raw)) +
  geom_point() +
  labs(x = "Max Deadlift Raw (lbs)", y = "SDC Time (mins)", title = "MDL vs SDC Time")

cor(df$MDL_Raw, df$IOCT_time)

cor(df$MDL_Raw, df$SDC_Raw)

cor(df$SDC_Raw, df$IOCT_time)

lrmodel <- df %>%
  lm(IOCT_time ~ SDC_Raw + MDL_Raw, data = .)
summary(lrmodel)

### New model with Sex as confounding

lrmodel <- df %>%
  lm(IOCT_time ~ SDC_Raw + MDL_Raw + Sex, data = .)
summary(lrmodel)


############## LEGO

Lego <- read_csv("Lego.csv")


lrmodel <- Legos %>%
  lm(price ~ pieces + reviews, data = .)
summary(lrmodel)

lrmodel <- Legos %>%
  lm(price ~ pieces + reviews + Licensed, data = .)
summary(lrmodel)



