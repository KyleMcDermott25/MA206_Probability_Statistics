library(tidyverse)
library(janitor)

# Question 1

#Load data
Kissing <- read_csv("Kissing.csv")

#Explore Data
head(Kissing)

Kissing %>% 
  tabyl(Direction) %>% 
  adorn_totals()

phat <- 80/124
n <- 124
pi <- 0.60

sd <- sqrt((pi*(1-pi))/n)

z <- (phat - pi)/sd

pvalue <- 1-pnorm(z)

#Calculate confidence interval
phat
se <- sqrt((phat*(1-phat))/n)
siglevel <- 0.01
multiplier <- qnorm(1-siglevel/2)
CI <- c(phat - multiplier*se, phat + multiplier*se)
CI


# Question 3: Facebook Friend Request
phat <- 18/101
n = 101
se = sqrt(phat*(1-phat)/n)
siglevel = 0.01 # the significance level for the test
multiplier = qnorm(1-siglevel/2) #For a  CI

CI = c(phat - multiplier*se, phat + multiplier*se)
CI
