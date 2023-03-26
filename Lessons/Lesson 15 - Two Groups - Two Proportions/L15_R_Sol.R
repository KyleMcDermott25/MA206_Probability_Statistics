library(tidyverse)
library(janitor)

Nurse <- read_delim("http://www.isi-stats.com/isi/data/chap5/Gilbert.txt", delim="\t")

Nurse <- Nurse %>%
  mutate(`GilbertWorked?` = as.factor(`GilbertWorked?`), Patient = as.factor(Patient))  # convert the categorical variables to a factors

Nurse %>%
  tabyl(`GilbertWorked?`, Patient) %>%
  adorn_totals(c("row", "col"))

null =  0          # Enter the value of your Null Hypothesis Parameter
  successes_1 =  40   # number of successes in group 1
  successes_2 =  34   # number of successes in group 2
  n_1 = 257   # sample size of group 1     
  n_2 = 1384   # sample size of group 2
  n = n_1 + n_2     # total sample size
phat_1 = successes_1/n_1
phat_2 = successes_2/n_2
phat_t = (successes_1 + successes_2)/(n_1 + n_2)
stat = phat_1-phat_2 # ensure this matches your null hypothesis order
sd = sqrt(phat_t*(1-phat_t)*(1/n_1 + 1/n_2))
z = (stat-null)/sd  # standardized statistic

pvalue = 1 - pnorm(z)
