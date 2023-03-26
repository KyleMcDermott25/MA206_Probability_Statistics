library(tidyverse)
library(janitor)

# Olympics question 4
oly <- read_csv("Olympics2016.csv")
colnames(oly)

oly %>% 
  ggplot(aes(x=Weight)) +
  geom_histogram()

oly %>% 
  summarize(mean = mean(Weight),
            s = sd(Weight),
            n = n())

null = 80.7            # Enter the value of your Null Hypothesis Parameter
n =    2014            # Enter the sample size
stat = 74             # Enter the value of your statistic
s =    16.2            # Sample standard deviation (from summary statistics)
sd = s/sqrt(n)     # Standard deviation of the null distribution
t = (stat-null)/sd # Standardized statistic

pvalue = pt(t, n-1)


siglevel =  .10            # Enter your significance level (alpha)
multiplier = qt(1-siglevel/2, n-1)
se = s/sqrt(n)          # Standard error
CI = c(stat-multiplier*se, stat+multiplier*se)  # Confidence Interval
CI


## Basketball question 5
bb <- read_csv("Basketball.csv")
head(bb)

bb %>% 
  tabyl(Win) %>% 
  adorn_totals()

null = 0.28          # Enter the value of your Null Hypothesis Parameter
n =   41           # Enter the sample size
stat =  0.3659         # Enter the value of your statistic
sd = sqrt(null*(1 - null)/n)  # Standard deviation of the null distribution
z = (stat-null)/sd  # Standardized Statistic

pvalue = 1-pnorm(z)


siglevel = 0.05              # Enter your significance level (alpha)
  multiplier = qnorm(1-siglevel/2) 
se = sqrt(stat*(1 - stat)/n)     # Standard Error
CI = c(stat-multiplier*se, stat+multiplier*se)  # Confidence Interval
CI


## CI for not sold out games
##
bb %>% 
  count(`Sold Out`, Win) %>% 
  pivot_wider(names_from = `Sold Out`, values_from = n) %>% 
  adorn_totals()

Basketball %>% 
  table() %>% 
  plot()






stat = .5217
n = 23

siglevel = 0.05             # Enter your significance level (alpha)
  multiplier = qnorm(1-siglevel/2) 
se = sqrt(stat*(1 - stat)/n)     # Standard Error
CI = c(stat-multiplier*se, stat+multiplier*se)  # Confidence Interval
CI


