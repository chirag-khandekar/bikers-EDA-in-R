library(dplyr)
library(ggplot2)
install.packages('tidyverse')
library(tidyverse)

bike = read.csv('D:/Downloads/bike_buyers.csv')
View(bike)

# Number of rows and columns
dim(bike)

# Structure of dataset
str(bike)

# Basic statistics of each column
summary(bike)

# DATA CLEANING

# Checking for null values column wise
colSums(is.na(bike))

# Replacing null values in Income column with mean
avg_sal = mean(bike$Income, na.rm = TRUE)

bike$Income[is.na(bike$Income)] = avg_sal

# Function to calculate the mode
mode = function(input)
{
  unique_in = unique(input)
  tab = tabulate(match(input, unique_in))
  unique_in[tab == max(tab)]
}

# Replacing null values in Children column with mode
bike$Children[is.na(bike$Children)] = mode(bike$Children)

# Replacin null values in Cars column with mode
bike$Cars[is.na(bike$Cars)] = mode(bike$Cars)

# Replacing null values in Age column with mean
avg_age = mean(bike$Age, na.rm = TRUE)

bike$Age[is.na(bike$Age)] = avg_age

