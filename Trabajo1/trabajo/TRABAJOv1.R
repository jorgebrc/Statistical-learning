---
  title: "HOMEWORK 1"
author: "Jorge Barcia & Paloma Núñez"
date: "2023-11-05"
output: html_notebook
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
  
rm(list = ls())
#WD
setwd("C:/Users/JORGE/OneDrive/Escritorio/apuntes/Aprendizaje estadistico/Trabajo1")
#note that we have find our dataset in https://archive.ics.uci.edu/dataset/2/adult
#lets load some libraries we will use later 
library(dplyr)
library(tidyverse)
library(ggplot2)
#load data keep in mind that there is no header and that missing values are represent as "?"
data = read.csv("adult.data",header = FALSE, na.strings = " ?")
#As there is no header we need to add lables 
colnames(data) = c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status",
                          "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss",
                          "hours_per_week", "native_country", "income")

# has a curiosity this dataset have leading or trailing spaces which were a problem in some parts of our project, this problem
# took us quite a large amount of time to solve as we were not able to find the cause 
data[] = lapply(data, function(x) if (is.character(x)) trimws(x) else x)
#lets see what structure have  
str(data)
#we can see that we have chr values and not as factors lets solve

data = mutate(data,across(c(workclass, education, marital_status, occupation, relationship, race, sex, native_country, income), as.factor))

#we need some of the factor in an specific order
data$education = factor(data$education, levels = c(
  "Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th",
  "HS-grad", "Some-college", "Assoc-acdm", "Assoc-voc", "Bachelors", "Masters", "Doctorate"))

str(data)
#we can see that we have both numerical and categorical features

#lets see a summary of our data 
summary(data)


#          ==================================FEATURES EXPLANATION=============================================
#not every label is intuitive so here there are some explanation we deem necessary 

# fnlwgt: the fnlwgt value indicates how many people in the entire population have similar characteristics to those of the record.
#         The Census Bureau calculates these weights based on factors like age, race, gender, and region, allowing analysts to make 
#         inferences about the broader population without bias from the sampling process.

# education_num: corresponds to levels of education beeing 1 the lowest and 15 the highest

# capital_gain: refers to income earned from capital assets, such as stocks, bonds, or real estate, that an individual has sold 
#               at a profit during the census year.

# capital_loss: represents the amount of money lost from the sale of capital assets, such as stocks, bonds, or real estate, 
#               during the census year. 

# hours_per_week: hours an individual works per week in their primary job.


#  =================NA VALUES HANDELING=====================


#For seeing clearly the Number NA values and were are they located
sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))
#Lets plot this info
na_counts = sapply(data, function(x) sum(is.na(x)))
aux_df = data.frame(Column = names(na_counts), NA_Count = na_counts)

ggplot(aux_df, aes(x = reorder(Column, -NA_Count), y = NA_Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Number of NA", x = "Columns", y = "Count of NA Values") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

#we can see that all of the NA values are in categorical features taking into acount that we have a fairly large dataset
#the best option is to omit these values as no substitution of these values will be satisfactory 
data = na.omit(data)


# ======================FEATURE ENGINEERING====================================
# Age is a useful statistic however for some parts of our project we are not interested in the exact age and the age group is 
# much more useful
data = mutate(data,age_group = case_when(age < 25 ~ "Young",age >= 25 & age < 45 ~ "Adult",
                                         age >= 45 & age < 65 ~ "Middle-aged",age >= 65 ~ "Senior"))
data$age_group = as.factor(data$age_group)

# similarly to age we have the case of worked hours, we are interested in seeing if overworking or part-timing has an influence 
data = mutate(data,work_hours_category = case_when(hours_per_week < 30 ~ "Part-time",
                                                   hours_per_week >= 30 & hours_per_week <= 40 ~ "Full-time",
                                                   hours_per_week > 40 ~ "Over-time"))
data$work_hours_category = as.factor(data$work_hours_category)

# as the country of origin is EEUU we want to know if the native born have any advantage 
data$foreign_born = ifelse(data$native_country == 'United-States', 0, 1)


# ====================== OUTLIERS IDENTIFICATION===============================


#now we will check for outliers that could ruin our analisis 
#note that we can only have outliers in our numerical features
numeric_cols = select(data, age, capital_gain, capital_loss, hours_per_week)
#lets plot them
numeric_cols_tidy =  numeric_cols_long = pivot_longer(numeric_cols, 
                                                      cols = everything(), 
                                                      names_to = "Variable", 
                                                      values_to = "Value")

ggplot(numeric_cols_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.color = "lightcoral", outlier.size = 4) +
  labs(title = "Outliers in Numerical Columns", x = "Variables", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "none")

#lets uses 3-sigma rule 
detect_outliers_3sigma = function(x) {
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  lower_bound = mean_x - 3 * sd_x
  upper_bound = mean_x + 3 * sd_x
  return(x < lower_bound | x > upper_bound)
}
outliers = data.frame(
  age = detect_outliers_3sigma(numeric_cols$age),
  capital_gain = detect_outliers_3sigma(numeric_cols$capital_gain),
  capital_loss = detect_outliers_3sigma(numeric_cols$capital_loss),
  hours_per_week = detect_outliers_3sigma(numeric_cols$hours_per_week)
)
outlier_counts = sapply(outliers, sum)
outlier_counts

# ====================================================
# from the analysis we infer that age outliers are feasible 17-90 as min and max are acceptable values
# in capital_gain there's a few top outliers with a value of 99,999$ this suggests that this is the maximum value accepted for the
# survey how ever as the real value is higher than this value we are going to leave these outliers.
# capital_loss does not have this problem and the value of the outliers is feasible 
# hours_per_week is a especial case as 99 seem to be the maximum value similarly to what happens in capital_gain how ever here
# more than 99h a week seem inhumane, however as the dataset is from EEUU and from 1994 we can not rule out the possibility of these 
# are true values 
# ====================================================


#      ========================Visualization tools======================


#lets see some visualizations to learn a bit from the dataset
#we are going to start with the age visualization to see if our distribution is normal
ggplot(data)+
  aes(x = age)+
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Count")
#we see is not a normal distribution and more of a Geometric distribution center around 30 Years and skewed to the right

#this causes that our grups of age are not equaly represented as we can see in the next graph
ggplot(data) +
  aes(x = age_group) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age Groups", x = "Age Group", y = "Count")

# one the questions we want to answer is that if the hours worked per week have an impact in the income 
ggplot(data) +
  aes(x = income, y = hours_per_week, fill = income) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightpink", "lightgreen")) +
  labs(title = "Weekly Hours Worked by Income Level", x = "Income", y = "Hours per Week")+
  theme_minimal()
#we see that both groups have similar means how ever the +50k group is more spread than the -50K

#now lets check if education have an impact 

ggplot(data, aes(x = education, fill = income)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightpink", "lightgreen")) +
  theme_minimal() +
  labs(title = "Income Distribution by Education Level", x = "Education Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# from the graph we can see that the amount of people without HS-graduation that make more than 50K are almost non-existing
# similarly Bachelors degree seems to be the high level of study that is significant as from that point if you have 
# masters or doctorates there are more people that make more than 50k than people that don't in that specific group 
