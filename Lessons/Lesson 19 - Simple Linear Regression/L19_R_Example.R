library(tidyverse)
library(janitor)

df <- read_csv("math.csv")

colnames(df)

df %>%
  ggplot(aes(x = age, y = G3)) + geom_point() + labs(x = "Age",
                                                                             y = "Score (cm)", title = "Year vs. Height of Wimbeldon Champions")

cor(df$age, df$G3)


df_model = df %>%
  lm(G3 ~ age, data = .)

summary(df_model)

df %>%
  ggplot(aes(x = age, y = G3)) + geom_point() + geom_smooth(method = "lm",
                                                                       se = FALSE)
lrmodel <- df_model

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
