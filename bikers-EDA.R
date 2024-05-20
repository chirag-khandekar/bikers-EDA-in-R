library(dplyr)
library(ggplot2)
install.packages('tidyverse')
library(tidyverse)

bike = read.csv('C:/Users/07/Downloads/bikers-EDA-in-R-main/bikers-EDA-in-R-main/bike_buyers.csv')
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

# Replacing blank values in Gender by mode
bike$Gender[bike$Gender == ''] = mode(bike$Gender)

# Replacing blank values in Marital Status by mode
bike$Marital.Status[bike$Marital.Status == ''] = mode(bike$Marital.Status)

# Visual Analysis (EDA)

# Distribution of Income with histogram
ggplot(bike, aes(Income))+
  geom_histogram(fill = 'Steelblue', color = 'Black', bins = 20)+
  labs(title = 'Distribution of Income',
       x='Income slabs')

# Distribution of Age with histogram
ggplot(bike, aes(Age))+
  geom_histogram(fill = 'Steelblue', color = 'Black', bins = 20)+
  labs(title = 'Distribution of Age',
       x='Age slabs')

# Visualizing count of gender using bar graph
ggplot(bike,aes(x = factor(Gender)))+
  geom_bar(stat = 'count', width = 0.3, color = 'Black',
           fill = 'Salmon')+
  labs(x = 'Gender',
       y = 'Count')

# Visualizing count of Marital Status using bar graph
ggplot(bike, aes(x= factor(Marital.Status)))+
  geom_bar( stat = 'count',fill = 'Salmon', color = 'Black',
            width = 0.3)+
  labs(x='Marital Status',
       y = 'Count')

# Visualizing Purchased bike proportion using Pie chart
ggplot(bike, aes(x = '', y = factor(Purchased.Bike), 
       fill = as.factor(Purchased.Bike)))+
  geom_bar(stat = 'identity')+
  coord_polar('y',start = 90)+
  labs(title = 'Proportion of Marital Status')

# Visualizing Education proportion using Pie chart
ggplot(bike, aes(x = '', y= factor(Education),
                 fill = as.factor(Education)))+
  geom_bar(stat = 'identity')+
  coord_polar('y', start = 0)+
  labs(title = 'Proportion of Education')

# Visualizing relationship between age and income
ggplot(bike, aes(x=Age, y=Income))+
  geom_point(color = 'Salmon')+
  labs(x='Age',
       y='Income')

# LINE GRAPH
ggplot(bike, aes(Age, Occupation))+
  geom_line(aes(color = Age))


