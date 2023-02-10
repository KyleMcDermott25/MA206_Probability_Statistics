library(tidyverse)
library(janitor)

# Question 2
xbar <- 3.01
s <- 1.97
n <- 100

# Find Confidence Interval (95%)
siglevel <- 0.05
multiplier <- qt(1-siglevel/2,n-1)
se <- s/sqrt(n)

CI <- c(xbar - multiplier*se, xbar + multiplier*se)
CI

# Find t-statistic and pvalue
mu <- 2.84

t <- (xbar - mu )/(s/sqrt(n))
t
pvalue <- 2*(1-pt(abs(t),n-1))
pvalue


### Question 3

# Confidence Internal  (99%)
phat <- 1774 / 2815
n <- 2815

se <- sqrt((phat *(1- phat))/n)
siglevel <- 0.01
multiplier <- qnorm(1-siglevel/2)

CI <- c(phat - multiplier*se, phat + multiplier*se)
CI

# z and pvalue
pi <- 0.70

z <- (phat - pi)/sqrt((pi*(1-pi))/n)
z
pvalue <- 2*(1-pnorm(abs(z)))
pvalue

### Question 4 - Age First Child
AgeFirstChild <- read_csv("AgeFirstChild.csv")
head(AgeFirstChild)

AgeFirstChild %>% 
  summarize(mean = mean(Age),
            median = median(Age),
            s = sd(Age),
            n = n())

xbar <- 24.3
s <- 5.74
n <- 1666

siglevel <- 0.01

multiplier <- qt(1-siglevel/2, n-1)
se <- s/sqrt(n)

CI = c(xbar - multiplier * se, xbar + multiplier*se )
CI



### Question 5
Phones <- read_csv("Phones.csv")
head(Phones)

Phones %>%
  ggplot(aes(x = talk, fill = sex)) +
  geom_bar(position = "dodge")

# Calculate CI (99%)
Phones %>% 
  tabyl(talk) %>% 
  adorn_totals()

n <- 242
phat <- 126/n
se <- sqrt((phat * (1-phat))/n)

siglevel <- 0.05
multiplier <- qnorm(1-siglevel/2)
CI = c(phat - multiplier * se, phat + multiplier * se)
CI

# Only females CI (5d)
Phones %>%
  ggplot(aes(x = talk, fill = sex)) +
  geom_bar(position = "dodge")

#Look at proportions that talked on phone for just females
Phones %>% 
  filter(sex == "Female") %>% 
  summarize(tabyl(talk)) %>% 
  adorn_totals

n <- 127
phat <- 72/n
se <- sqrt((phat * (1-phat))/n)

siglevel <- 0.05
multiplier <- qnorm(1-siglevel/2)
CI = c(phat - multiplier*se, phat + multiplier*se)
CI


#Look at proportions that talked on phone for just females
Phones %>% 
  filter(sex == "Male") %>% 
  summarize(tabyl(talk)) %>% 
  adorn_totals

n <- 115
phat <- 54/n
se <- sqrt((phat * (1-phat))/n)

siglevel <- 0.05
multiplier <- qnorm(1-siglevel/2)
CI = c(phat - multiplier*se, phat + multiplier*se)
CI


Phones %>% 
  filter(sex == "Male") %>% 
  tabyl(talk) %>% 
  adorn_totals()

Phones
            