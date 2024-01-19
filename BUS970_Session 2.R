############################################################
# BUS 970 | Spring 2024 | Session 2
# Getting Started with R - Data Types and Data Manipulations
# CK 
# 19 Jan 2024 
## REFERENCES / Attribution: 
# R Style GUIDE: http://adv-r.had.co.nz/Style.html
## attribution: adopted from  PhilChodrow @ MIT https://philchodrow.github.io/cos_2017/ 
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

#install.packages("data.table")
library(data.table)

# data  link on canvas in Module 3
#Download data from  https://philchodrow.github.io/cos_2017/1_terminal_and_git/taxi_data.csv  
# courtesy of of the New York City Taxi and Limousine Commission and PhilChodrow @ MIT
  
############################
## WORKING WITH CSV FILES ##
############################
# STEP A: Load the DATA


#1/  Use GUI  to open file

  # file - Import Dataset-  From text (base) or From text (readr) - "select file and appropriate selections"

#2/ # traditional # Load csv files using read.csv
taxi_data <- read.csv("C:/Users/CK/Downloads/taxi_data.csv", header = TRUE)
# header = TRUE is usually ASSUMED, so not strictly necessary

#3 / Using data table alternate - fast and effecient!
dt <- fread("C:/Users/CK/Downloads/taxi_data.csv")

#4/ directly from the internet
TaxiData <- fread("https://philchodrow.github.io/cos_2017/1_terminal_and_git/taxi_data.csv")


#Step B: Look at the DATA

# Use names() to extract column names
names(taxi_data)

# Use str to look at details of columns
str(taxi_data)

# Use head() to look at the first several rows
head(taxi_data)

# Use the $ command to look at specific columns
taxi_data$

taxi_data$rate_code

####################################################
## BASIC STATISTICS, PLOTTING, AND SUMMARY TABLES ##
####################################################

# Calculate the mean, standard deviation, and other statistics
mean(taxi_data$passenger_count)
sd(taxi_data$passenger_count)
summary(taxi_data$passenger_count)
hist(taxi_data$passenger_count)
# Plot fare amount vs trip distance
plot(taxi_data$trip_distance, taxi_data$fare_amount)


# Plot with a title, x- and y-axis labels
plot(taxi_data$trip_distance, taxi_data$fare_amount, 
     main="Fare Amount vs. Trip Distance", 
     xlab = "Trip Distance [mi]", 
     ylab = "Fare Amount [$]")

# For other plots and information about the default graphics package
library(help = "graphics")

# We'll use ggplot2 soon enough - that is really really COOL!

# Create a table to summarize the data
# Here, we look at mean trip distance, based on the number of passengers in the taxi
tapply(taxi_data$trip_distance, taxi_data$passenger_count, mean)

# We can also create a table to look at counts 
table(taxi_data$passenger_count, taxi_data$payment_type)

# we can save these as R data objects!


## Try out a few other basic statistics and graphing functions

min(taxi_data$trip_distance)
median(taxi_data$trip_distance)
max(taxi_data$trip_distance)
summary(taxi_data$trip_distance)
hist(log(taxi_data$trip_distance))

sum(taxi_data$total_amount)

hist(taxi_data$total_amount)
boxplot(taxi_data$fare_amount)


#####################################3
##### BONUS: IF TIME PERMITS!
### DEALING WITH RAW DATA
#####################################3


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()



## Loading Libraries / Packages ####
# if you haven't instlled these packages, run
# install.packages("data.table","dplyr")
require(data.table)
require(dplyr)

## LOADING DATA ####
# for this example, we load data from the internet directly
url <- "https://www.dropbox.com/s/9bykqkzuw2i9qih/nashville_airbnb.csv?dl=1"

#dt is an R object called a data table 
## Other ways to read data (read_csv, etc...)
dt <- fread(url) # Data.table command to read in data
#  if you want to load from your own computer, command is
dt <- fread("C:/Users/kalig/Downloads/nashville_airbnb.csv") # remember - / and not \

## Looking at the data ####
head(dt) # First 5 entries
tail(dt) # LAst 5 entries
dim(dt) # Num of rows and column

# view entire data
View(dt)
# also see details in the global enviroment


### THIS IS WHAT what most raw/ scapped data looks like!


# Summarizing the data:
ls(dt) # lists all column names
objects(dt) # Same as above 

summary(dt) # produces summar stats for numerical / int data.


## It's too large -  let's go by individual colums:
# To call columns in data frames or data tables, format is <data.table.name>$<column name>
# column names should autofill

summary(dt$bathrooms)

summary(dt$bedrooms)

# check individual columns
class(dt$name)
attributes(dt$price)

# we can visualize numeric / factor columns
hist(dt$bedrooms)
plot(dt$bathrooms,dt$bedrooms)


#In-class exercise:
### How many unique host id's exist in this data?



##########################
##### COMMAND LISTS ######
##########################
##1/ EXPLORATION

mydata <- dt
# for some data table / data frame object called mydata, these are some commands and desc. 
summary(mydata)  # Provides basic descriptive statistics and frequencies. 
edit(mydata)     # Open data editor
str(mydata)      # Provides the structure of the dataset
names(mydata)    # Lists variables in the dataset

head(mydata)     # First 6 rows of dataset
head(mydata, n=10)# First 10 rows of dataset
head(mydata, n= -10)  # All rows but the last 10

tail(mydata)     # Last 6 rows
tail(mydata, n=10)    # Last 10 rows
tail(mydata, n= -10)  # All rows but the first 10
mydata[1:10, ]   # First 10 rows
mydata[1:10,1:3] # First 10 rows of data of the first 3 variables

