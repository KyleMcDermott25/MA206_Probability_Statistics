library(tidyverse)
library(janitor)

#### Question 1 - Flat Tire

null <- 0.25
n <- 28
phat <- 14/n

sd <- sqrt(null * (1 - null)/n)
z <- (phat - null)/sd #3.055 - very strong evidence against the null
pvalue <- 1-pnorm(z)  #0.001 - very strong evidence against the null


#### Question 2 - Cookies

Results <- read_csv("Cookies.csv")

# visulaize the distribtion of preferences
Results %>% 
  ggplot(aes(x = Best)) + 
  geom_bar()

# Compute the statistic
Results %>% 
  tabyl(Best) %>% 
  adorn_totals()

null <- 0.333
n <- 67
phat <- 50/n

# Calculate standardized statistic and pvalue
sd <- sqrt(null * (1 - null)/n)
z <-  (phat - null)/sd
pvalue = 1-pnorm(z)

#### Question 3 - Faces

FaceResults <- read_csv("Faces.csv")

FaceResults %>% 
  ggplot(aes(x = Faces)) + 
  geom_bar()

FaceResults %>% 
  tabyl(Faces) %>% 
  adorn_totals()

# do not meet validity condtions, need to get sd from simulation
null <- 0.50
n <- 63
phat <- 59/n
sd <- .062

z <- (phat - null)/sd


#### Question 4 - Toast

Toast <- read_csv("Toast.csv")

Toast %>% 
  ggplot(aes(x = Butter)) +
  geom_bar()

Toast %>% 
  tabyl(Butter) %>% 
  adorn_totals()

null <- 0.5
n <- 48
phat <- 19/n

sd <- sqrt(null * (1-null)/n)
z <- (phat - null)/sd
pvalue = 2* (1-pnorm(abs(z)))
