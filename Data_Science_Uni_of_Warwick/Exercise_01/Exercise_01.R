# Exercise - 01
# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read arff file as csv
data <- read.csv(
  "adult.arff", 
  header = TRUE, 
  comment.char = "@", 
  stringsAsFactors = TRUE)
summary(data)

# with Rweka for proper column naming
require(RWeka)
data_rweka <- read.arff("adult.arff")
attach(data_rweka)

#1.
# Select the “age” attribute, and read off the minimum, maximum and mean values [15]
# for age.
summary(age)

#2.
# Tracking your mouse over the plot of the age attribute will give the count for each [15]
# bar, and the range of input values it covers. Why do some bars stick out, about
# twice as far as their neighbours? What does this say about the data?
boxplot(age)
hist(age)

#3.
# categorial attribute:
# workclass, education, marital-status, occupation
# relationship, race, sex, native-country, class

#4.
# What is the average number of hours worked per week?
summary(`hours-per-week`)
# avg = 40.42 h-p-w

#5.
# Which is the most popular occupation in the data set? Which is the most common [15]
# native country?
head(occupation, 100)
which.max(occupation)
occupation[which.max(occupation)]
# Armed-Forces

#6.
# Look at the interaction between ‘sex’ and ‘relationship’. Some attribute values are [15]
# ‘pure’ — only associated with one class attribute value. Why is this?
plot(sex, relationship)
# better plot:
plot(relationship, sex)

#7.
# Study the relationship between education and education-num. What do you learn [15]
# about these two attributes?
plot(education, `education-num`)
cor(as.numeric(education), `education-num`)
#  -0.2463497






