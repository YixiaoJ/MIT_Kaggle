# 1-data_sets

library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(caret)
library(purrr)

# set ordered factor levels
income <- c("under 25", "25-50", "50-74", "75-100", "100-150", "over 150")

education <- c("Current K-12", "High School Diploma", "Current Undergraduate",
               "Associate's Degree", "Bachelor's Degree", "Master's Degree",
               "Doctoral Degree")

# get training and testing data
training <- read_csv("train2016.csv") %>%
    mutate(Income = str_replace_all(Income, "\\$|,[0-9]{3}", ""),
           Income = str_replace_all(Income, " - ", "-")) %>%
    mutate_each(funs(factor), -USER_ID, -YOB, -Income, -EducationLevel) %>%
    mutate(Income = ordered(Income, levels = income),
           EducationLevel = ordered(EducationLevel, levels = education))

testing <- read_csv("test2016.csv") %>%
    mutate(Income = str_replace_all(Income, "\\$|,[0-9]{3}", "")) %>%
    mutate_each(funs(factor), -USER_ID, -YOB, -Income, -EducationLevel) %>%
    mutate(Income = ordered(Income, levels = income),
           EducationLevel = ordered(EducationLevel, levels = education))

# train / validate sets --------------------------------

set.seed(123)
inTrain <- createDataPartition(y = training$Party, p = 0.8, list = FALSE)
train.data <- training[inTrain, ]
valid.data <- training[-inTrain, ]

# remove invalid YOB
train.data$YOB[train.data$YOB > 2005 | train.data$YOB < 1910] <- NA

train.na <- train.data %>%
    mutate_each(funs(is.na), -USER_ID, -Party)

names(train.na[, -1]) <- paste(names(train.na[, -1]), "NA", sep = "_")

train.set <- inner_join(train.data, train.na, by = c(""))

# make outcome vectors
train.party <- train.data$Party
valid.party <- valid.data$Party

# convert factors to numeric
train.set <- dmap_if(train.data[, -7], is.factor, as.numeric)
valid.set <- dmap_if(valid.data[, -7], is.factor, as.numeric)
test.set <- dmap_if(testing, is.factor, as.numeric)
