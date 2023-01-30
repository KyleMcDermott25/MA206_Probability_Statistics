## Basics of R

# working directory
# Layout
# script vs. console
# loading packages

## Shortcuts
# `ctrl + enter`: runs current line

## R Basics

library(tidyverse)

# variables
x = 1 #assigns value to variable
y <- 2 # another way to do assignment
rm(x) # remove a variable from the global environment
rm(y,z) # remove several variables


## Vectors
# a vector in R (an ordered list)
list1 = c(1, 2, 3, 2)
list1
list2 = c("a", "b", "c", "a")
list2
class(list1)
class(list2)


# R likes to operate on vectors
1 + list1 # adds 1 to each entry of list1

df = tibble(list1,list2) #combines lists into a data frame
df
class(df)
rm(list1, list2) #removes list variables


# indexing in R
df$list1 # returns the variable `list1`
df$list2
df[1, ] # returns the first row
df[ ,1] # returns the first column


# Functions in R
# format:    function(arg1, arg2, arg3)
mean(df$list1)
?mean # to get help on a function
# Google is also a great reference for troubleshooting R
# You can copy error messages into Google and often find a solution


## Tidyverse

# `%>%` : pipe operator\
# used in `tidyverse` package to simplify nested commands
# `ctrl + shift + m`: inserts pipe operator


#### Installing and Loading Packages ####

#install.packages("tidyverse") # only run this once
library(tidyverse) # run this once per script to get the functions in the 
# tidyverse package

#install.packages("janitor")
library(janitor) # for clean_names and adorn_totals functions

# A package is a collection of functions.\
# Packages only need to be installed once, but we run the `library(insert_function_name_here)` at the start of each script.


# Two ways of doing the same thing:
df %>% filter(list1 > 1)
filter(df, list1 > 1)
# The piping function makes the object to the left the first argument in the function to the right.



# %>% becomes useful for multiple operations on an object:
df %>% 
  filter(list1 > 1) %>% 
  summarize(mean = mean(list1))

summarize(filter(df,list1 > 1), mean = mean(list1))

# the first way is much more intuitive






#### Installing and Loading Packages ####
#install.packages("tidyverse") # only run this once
library(tidyverse) # run this once per script to get the functions in the 
# tidyverse package
#install.packages("janitor")
library(janitor) # for clean_names and adorn_totals functions


#### Setting the Working Directory

# While you can use the `setwd` command to set you working directory, it is 
# simpler to ensure your data file is saved in the same folder as your `.r` file.



