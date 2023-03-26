library(tidyverse)
library(janitor)

Sleep <- read.csv("sleep.csv")

Sleep <- Sleep %>%
  mutate(sleep = as.factor(group))  # convert the categorical variable to a factor

Sleep %>%
  group_by(group) %>%
  summarize(mean = mean(improvement), s = sd(improvement), n = n())

Sleep %>%
  ggplot(aes(x = improvement)) + geom_histogram() + facet_grid(group ~.)

Sleep %>%
  group_by(group) %>%
  summarize(Minimum = min(improvement), LowerQuartile = quantile(prob = 0.25, improvement), Median = median(improvement),
            UpperQuartile = quantile(prob = 0.75, improvement), Maximum = max(improvement))


## MOVIES LIST


Movies <- read.csv("Movies.csv")

Movies2 <- Movies %>% 
  mutate(Profit = Worldwide.Box.Office - Production.Budget)

Movies2 %>%
  ggplot(aes(x=Profit))+
  geom_histogram(color="black", fill="gray")+
  geom_boxplot(color="blue", fill="lightblue", lwd=2)+
  theme_classic()+
  labs(title="Histogram", x="Profits", y="Count")+
  scale_x_continuous(labels=scales::dollar_format())

Movies2 %>%
  summarize(Minimum = min(Profit),
            LowerQuartile = quantile(prob =.25, Profit),
            Median = median(Profit),
            UpperQuartile = quantile(prob=.75, Profit),
            Maximum = max(Profit))


Movies2 %>%
  ggplot(aes(x=Profit))+
  geom_histogram(color="black", fill="gray")+
  geom_boxplot(color="blue", fill="lightblue", lwd=2)+
  facet_grid(Brand ~.)+
  theme_classic()+
  labs(title="Histogram", x="Profits", y="Count")+
  scale_x_continuous(labels=scales::dollar_format())
