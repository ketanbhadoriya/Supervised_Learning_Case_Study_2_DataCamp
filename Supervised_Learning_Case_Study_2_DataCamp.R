#Start of the Script

###Loading the required packages and the Data

library(dplyr)
library(tidyr)
library(ggplot2)
library(RCurl)
library(caret)

url_data <- "https://assets.datacamp.com/production/course_6013/datasets/stackoverflow.csv"

x <- getURL(url_data)

#cars2018 <- read.csv(textConnection(x))