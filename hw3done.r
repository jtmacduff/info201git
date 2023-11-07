#Jason Macduff
#jasontm1@uw.edu
#I worked alone on this assignment and I plan to use a late day for this assignment. 

#all library statements must be at the top of the page!
library(dplyr)
library(stringr)
library(testthat)

# Overview ---------------------------------------------------------------------

# Homework 3: Food Desert Analysis
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to calculate the answer.
# For grading, it's important that you store your answers in the variable names
# listed with each question in `backtics`. Make sure you DO NOT hardcode values
# unless specified to do so in the instructions! Make sure you also do not use the
# pipe operator on assignments!

# Loads in your datasets
food_df <- read.csv("food_access.csv") #DO NOT CHANGE!
county_df <- read.csv("CountyCodes.csv") #DO NOT CHANGE!


# Data Joining -----------------------------------------------------------------
#
# In this homework assignment, you will be working with two related datasets. 
# So, one of your first tasks is to properly join the two datasets into one!
# In the join process, you will need to join based on the names of the counties. 
# The problem is that there are multiple counties with the same name but are in 
# located in different states. Ex: Greene County, Alabama and 
# Greene County, Virginia. So you'll need to join based on both county name and 
# state name! The next problem is that the food_df dataset uses the full name of 
# the state whereas county_df uses the two letter state abbreviations. Joins 
# require exact string matches in order to work.
#
# ------------------------------------------------------------------------------

# Before you join the two dataframes, the first thing you need to do is create
# and populate a state abbreviation column called `state_code` to food_df that 
# stores the two letter state abbreviation for each state in the dataset. 
# HINT - Base R has two built in objects called state.abb and state.name that 
# stores the two letter state abbreviation and full state names respectively. See
# lecture notes on how to do this. 
food_df$state_code <- state.abb[match(food_df$State, state.name)]


# Now we can do a join! Do a left join on the food_df with the county_df and store
# the combined dataframe into a variable `df`. Make sure you join using your newly
# created state_code column in the process. Make sure you read the instructions
# for this section carefully!
df <- merge(food_df, county_df, by.x = c("County", "state_code"), by.y = c("County_name", "State"), all.x = TRUE) 
df <- df[order(df$State), ]





# Federal Level Analysis --------------------------------------------------------
#
# Let's gather some key summary statistics about food deserts at the federal level.
# Before you begin this section, make sure you read the documentation on these
# datasets carefully! You will be selecting different columns depending on if 
# the county is Rural or Urban!
#
# ------------------------------------------------------------------------------

# How many total people live in food deserts? Store your answer in a variable 
# named `total_fd_pop`. Remember that you will use different columns depending on
# if the county is rural or urban!! 
urban_county <- filter(df, Metro.nonmetro.status == 1)
rural_county <- filter(df, Metro.nonmetro.status == 0)
urban_total <- sum(county_urban$Low.Access.Numbers.People.1.Mile)
rural_total <- sum(county_rural$Low.Access.Numbers.People.10.Miles)
total_fd_pop <- sum(total_urban + total_rural)
print(total_fd_pop)

  


# What percent of the US total population live in food deserts? Round your answer
# to the nearest whole number are store in a variable named `total_fd_per`
total_fd_per <- round(total_fd_pop / sum(df$Population) * 100) 

# How many Children in total live in food deserts? Store your answer in a variable 
# named `child_fd_pop`
child_urban <- sum(county_urban$Low.Access.Numbers.Children.1.Mile)
child_rural <- sum(county_rural$Low.Access.Numbers.Children.10.Miles)
child_fd_pop <- (child_urban + child_rural)

# What percent of people living in food deserts are children? Round to the
# nearest whole number and store in a variable called `child_fd_per`
child_fd_per <- round((child_fd_pop / total_fd_pop) * 100)

# How many people who are low income live in food deserts? Store your answer in 
# a variable named `LI_fd_pop`
li_urban <- sum(county_urban$Low.Access.Numbers.Low.Income.People.1.Mile)
li_rural <- sum(county_rural$Low.Access.Numbers.Low.Income.People.10.Miles)
LI_fd_pop <- (li_urban + li_rural)

# What percentage of the overall US total population live are low income AND live
# in food deserts? Round to the nearest whole number and store in a variable 
# called `LI_fd_per`
LI_fd_per <- round(LI_fd_pop / sum(df$Population) * 100)

# County Level Analysis --------------------------------------------------------
#
# Let's gather some key summary statistics about food deserts at the county level.
#
# ------------------------------------------------------------------------------

# In your cleaned & joined df, How many counties are Rural? Store your answer in 
# a variable called `rural`
rural <- sum(df$Metro.nonmetro.status == 0)

# In your cleaned & joined df, How many counties are Urban? Store your answer in 
# a variable called `urban` 
urban <- sum(df$Metro.nonmetro.status == 1)

# What percent of counties are rural? Round to the nearest whole number and store
# your answer in a variable called `rural_per`
rural_per <- round(rural / nrow(df) * 100)

# What percent of counties are urban? Round to the nearest whole number and store
# your answer in a variable called `urban_per`
urban_per <- round(urban / nrow(df) * 100)


# County Level Analysis Continued  ---------------------------------------------
#
# Now lets do some more complicated analysis. In this section, you will create a
# function that labels whether a county on its own is a food dessert (which will 
# be different than looking at whether an individual person lives in a food desert
# which is what you did in the earlier section). Per the assignment instructions,
# a county is considered a "food desert county" if 33% or more of the population 
# in that county is located more than 1 mile (in urban counties) or 
# 10 miles (in rural counties) from the nearest supermarket or large grocery store.
# 
# ------------------------------------------------------------------------------


# Fill out the following function called `is_food_desert` that 
# given a county and a state name as a string returns TRUE if that county is a food 
# desert county and FALSE otherwise.
# NOTE: Read the assignment instructions on canvas to see how to determine if a 
# county is a food desert or not. 
# NOTE2: Your function should not error if given county name "District of Columbia"

is_food_desert <- function(county, state) {
  function_df <- df[df$County == county & df$State == state, ]
  
  if (function_df$Metro.nonmetro.status == 1 && 
      round(function_df$Low.Access.Numbers.People.1.Mile / function_df$Population, 2) >= 0.33) {
    return(TRUE)
  } else if (function_df$Metro.nonmetro.status == 0 &&
             round(function_df$Low.Access.Numbers.People.10.Mile / function_df$Population, 2) >= 0.33) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Using the `is_food_desert` function, add and populate a new column in your df 
# called `food_desert` that stores TRUE if that county is a food desert and 
# FALSE if that county is not a food desert. 
# NOTE: you can do this using the mapply function or by using a for loop. 
# NOTE2: if you use a for loop for this task, your code might take a few minutes
# to run. 
df$food_desert <- mapply(is_food_desert, df$County, df$State)

# How many counties in the dataset are food desert counties? Store your answer in a
# variable called `fd_desert`.
fd_desert <- sum(df$food_desert)

# What percentage of counties are food desert counties? Round you answer to the nearest
# whole number and store in a variable called `fd_desert_per`
fd_desert_per <- round(fd_desert / nrow(df) * 100)

# What percentage of rural counties are food desert counties? Round you answer to the nearest
# whole number and store in a variable called `per_rural_fd`.
fd_rural <- filter(df, Metro.nonmetro.status == 0 & food_desert == 1)
per_rural_fd <- round(sum(nrow(fd_rural)) / rural * 100)

# What percentage of urban counties are food desert counties? Round you answer to the nearest
# whole number and store in a variable called `per_urban_fd`.
fd_urban <- filter(df, Metro.nonmetro.status == 1 & food_desert == 1)
per_urban_fd <- round(sum(nrow(fd_urban)) / urban * 100)

# What is the relative risk of urban counties versus rural counties. To get relative
# risk, use the percent difference formula. Make sure you use the correct formula for this! Check class 
# notes for more details on how to get percent difference! Store your answer in
# a variable called `rel_risk`. 
rel_risk <- ((per_urban_fd - per_rural_fd) / per_rural_fd)

# Once complete, you may uncomment this line in order to see your results!
# print(paste0("Urban counties are ", round(rel_risk), " times more likely than Rural counties to be a food desert"))

#To run the test file, uncomment the line below before running source. 
test_file("hw3_test.r")
