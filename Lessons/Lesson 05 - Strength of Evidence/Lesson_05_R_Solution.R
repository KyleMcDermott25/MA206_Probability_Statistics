library(tidyverse)
library(janitor)

resign = read_csv("resignations.csv")

resign %>%
  count(State) %>%
  mutate(Proportion = n/sum(n)) %>%
  adorn_totals()