library(tidyverse)
library(janitor)

Smokers = read_csv("smokers.csv")

Smokers <- Smokers %>% 
  mutate_if(is.character, as.factor) # convert Parents and Child variables to factors

Smokers %>%
  table() %>%
  plot()

Smokers %>%
  ggplot(aes(x = Parents, fill = Child)) +
  geom_bar(position = position_fill()) +
  labs(x = "Smoking Status of Parents", y = "Proportion of Births by Sex",
       title = "Difference in the Proportion of Boys Born to Non-smokers vs. Smokers", fill = "Sex of Child", caption = "Segmented Bar Plot")


Smokers %>%
  tabyl(Child, Parents) %>%
  adorn_totals(c("row", "col"))
