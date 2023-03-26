library(tidyverse)
library(janitor)




##### Board Sheet 
#Restaurant Tips




null =  0    #Enter the value of your Null Hypothesis Parameter
  xbar_1 =  5.44  # sample mean of group 1
  xbar_2 =  3.49  # sample mean of group 2
  s_1 =     1.75  # sample standard deviation of group 1
  s_2 =     1.13  # sample standard deviation of group 2
  n_1 =     20  # sample size of group 1
  n_2 =     20  # sample size of group 2
  n = n_1 + n_2   # total sample size
stat = xbar_1 - xbar_2
sd = sqrt(s_1^2/n_1 + s_2^2/n_2)
t = (stat-null)/sd   # standardized statistic

pvalue = 1 - pt(t, n - 2)

siglevel =    .05          #Enter your significance level (alpha)
  multiplier = qt(1-siglevel/2, n-2)
se = sqrt(s_1^2/n_1 + s_2^2/n_2) # standard error
CI = c(stat-multiplier*se, stat+multiplier*se) # confidence interval



## Star Wars problem




null =    0        # Enter the value of your Null Hypothesis Parameter
  successes_1 = 229    # number of successes in group 1
  successes_2 = 86    # number of successes in group 2
  n_1 =  413  # sample size of group 1     
  n_2 =  187  # sample size of group 2
  n = n_1 + n_2     # total sample size
phat_1 = successes_1/n_1
phat_2 = successes_2/n_2
phat_t = (successes_1 + successes_2)/(n_1 + n_2)
stat = phat_1-phat_2 # ensure this matches your null hypothesis order
sd = sqrt(phat_t*(1-phat_t)*(1/n_1 + 1/n_2))
z = (stat-null)/sd  # standardized statistic

pvalue = 2 * (1 - pnorm(abs(z)))

siglevel =   .10           # Enter your significance level (alpha)
  multiplier = qnorm(1-siglevel/2)
se = sqrt(phat_1*(1-phat_1)/n_1+phat_2*(1-phat_2)/n_2) # Standard Error
CI = c(stat-multiplier*se, stat+multiplier*se)  # Confidence Interval


## Breakfast problem

df <- read.csv("BreakfastGPA.csv")

df <- df %>%
  mutate(Breakfast = as.factor(Breakfast))  # convert the categorical variable to a factor

df %>%
  group_by(Breakfast) %>%
  summarize(mean = mean(GPA), s = sd(GPA), n = n())

df %>%
  ggplot(aes(x = GPA)) + geom_histogram() + facet_grid(Breakfast ~
                                                                   .)
null =  0    #Enter the value of your Null Hypothesis Parameter
  xbar_1 =  3.41  # sample mean of group 1
  xbar_2 =  3.56  # sample mean of group 2
  s_1 =      .435 # sample standard deviation of group 1
  s_2 =      .310 # sample standard deviation of group 2
  n_1 =      43 # sample size of group 1
  n_2 =      63 # sample size of group 2
  n = n_1 + n_2   # total sample size
stat = xbar_1 - xbar_2
sd = sqrt(s_1^2/n_1 + s_2^2/n_2)
t = (stat-null)/sd   # standardized statistic

pvalue = 2 * (1 - pt(abs(t), n - 2))

siglevel =   .05           #Enter your significance level (alpha)
  multiplier = qt(1-siglevel/2, n-2)
se = sqrt(s_1^2/n_1 + s_2^2/n_2) # standard error
CI = c(stat-multiplier*se, stat+multiplier*se) # confidence interval




## FDA Problem

fish <- read.csv("HudsonFish.csv")


fish <- fish %>%
  mutate(Season = as.factor(Season))  # convert the categorical variable to a factor

fish %>%
  group_by(Season) %>%
  summarize(mean = mean(Total.PCB.ppm.), s = sd(Total.PCB.ppm.), n = n())


fish %>%
  ggplot(aes(x = Total.PCB.ppm.)) + geom_histogram() + facet_grid(Season ~.) + xlim(c(0,5))

fish2 <- fish %>%
  mutate(Spring2 = Season=="Spring")


fish2 %>%
  group_by(Spring2) %>%
  summarize(mean = mean(Total.PCB.ppm.),
            s = sd(Total.PCB.ppm.),
            count=n())
fish2 %>%
  ggplot(aes(x = Total.PCB.ppm.)) + geom_histogram() + facet_grid(Spring2 ~.) + xlim(c(0,5))

null =  0    #Enter the value of your Null Hypothesis Parameter
  xbar_1 = .978  # sample mean of group 1
  xbar_2 =  1.91  # sample mean of group 2
  s_1 =     3.78  # sample standard deviation of group 1
  s_2 =     1.79  # sample standard deviation of group 2
  n_1 =     1869  # sample size of group 1
  n_2 =      603 # sample size of group 2
  n = n_1 + n_2   # total sample size
stat = xbar_1 - xbar_2
sd = sqrt(s_1^2/n_1 + s_2^2/n_2)
t = (stat-null)/sd   # standardized statistic

pvalue = pt(t, n - 2)   

siglevel =       .05       #Enter your significance level (alpha)
  multiplier = qt(1-siglevel/2, n-2)
se = sqrt(s_1^2/n_1 + s_2^2/n_2) # standard error
CI = c(stat-multiplier*se, stat+multiplier*se) # confidence interval
