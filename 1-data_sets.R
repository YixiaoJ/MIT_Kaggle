# 1-data_sets

library(readr)
library(plyr)
library(dplyr)
library(tibble)
library(stringr)
library(caret)
library(purrr)
library(mice)

# use saved imputed data set if it exists, else do imputing (slow)
get_impute <- function(file.save, set, method = NULL) {
    if (file.exists(file.save)) {
        imp <- readRDS(file.save)
    } else {
        if (!is.null(method)) {
            imp <- mice(set, method = method)
        } else {
            imp <- mice(set)
        }
        saveRDS(imp, file.save)
    }
    imp
}

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
valid.data$YOB[valid.data$YOB > 2005 | valid.data$YOB < 1910] <- NA
testing$YOB[testing$YOB > 2005 | testing$YOB < 1910] <- NA

# make outcome vectors
train.party <- train.data$Party
valid.party <- valid.data$Party

# convert factors to numeric
# train.set <- dmap_if(train.data[, -7], is.factor, as.numeric)
# valid.set <- dmap_if(valid.data[, -7], is.factor, as.numeric)
# test.set <- dmap_if(testing, is.factor, as.numeric)

# feature processing -------------------------------------------

# create an "Unknown" level for factor vars before converting to dummy vars
train.set <- train.data %>%
    select(-Party) %>%
    mutate_each(funs(as.character), -USER_ID, -YOB) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

# impute missing data (should only be YOB variable)
train.mice <- get_impute("train_mice.Rds", train.set)
train.set <- complete(train.mice)

# make dummy variables for factor data
dv <- dummyVars(~ ., data = train.set)
train.dv <- predict(dv, newdata = train.set) %>% as_data_frame()

# repeat for validation set
valid.set <- valid.data %>%
    select(-Party) %>%
    mutate_each(funs(as.character), -USER_ID, -YOB) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

valid.mice <- get_impute("valid_mice.Rds", valid.set, train.mice$method)
valid.set <- complete(valid.mice)

valid.dv <- predict(dv, newdata = valid.set) %>% as_data_frame()

# repeat for test set
test.set <- testing %>%
    mutate_each(funs(as.character), -USER_ID, -YOB) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

test.mice <- get_impute("test_mice.Rds", test.set, train.mice$method)
test.set <- complete(test.mice)

test.dv <- predict(dv, newdata = test.set) %>% as_data_frame()

# high correlation -------------------------------------

hcor.set <- dmap_if(train.set, is.factor, as.numeric) %>%
    cor(use = "na.or.complete")
hc.set <- findCorrelation(hcor.set)

hcor.dv <- cor(train.dv, use = "na.or.complete")
hc.dv <- findCorrelation(hcor.dv)
