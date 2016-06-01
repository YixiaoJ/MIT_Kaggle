# 1-data_sets

library(readr)
library(plyr)
library(dplyr)
library(tibble)
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
valid.data$YOB[valid.data$YOB > 2005 | valid.data$YOB < 1910] <- NA
testing$YOB[testing$YOB > 2005 | testing$YOB < 1910] <- NA

# make outcome vectors
train.party <- train.data$Party
valid.party <- valid.data$Party

# make vars indicating presence of NA
# train.na <- train.data[, -7] %>%
#     mutate_each(funs(is.na), -USER_ID)
#
# names(train.na)[-1] <- paste(names(train.na)[-1], "NA", sep = "_")
#
# valid.na <- valid.data[, -7] %>%
#     mutate_each(funs(is.na), -USER_ID)
#
# names(valid.na)[-1] <- paste(names(valid.na)[-1], "NA", sep = "_")

# convert factors to numeric
train.set <- dmap_if(train.data[, -7], is.factor, as.numeric)
valid.set <- dmap_if(valid.data[, -7], is.factor, as.numeric)
test.set <- dmap_if(testing, is.factor, as.numeric)

# dummy vars -------------------------------------------

# use unordered levels
# tmp <- train.data %>%
#     mutate(Income = factor(Income, ordered = FALSE),
#            EducationLevel = factor(EducationLevel, ordered = FALSE))
#
# dv <- dummyVars(~ ., data = tmp[, -7])
# train.dv <- predict(dv, newdata = tmp[, -7]) %>% as_data_frame()
#
# dv3 <- dummyVars(~ ., data = tmp[, -7], fullRank = TRUE)
# train.dv3 <- predict(dv3, newdata = tmp[, -7]) %>% as_data_frame()
#
# tmp <- valid.data %>%
#     mutate(Income = factor(Income, ordered = FALSE),
#            EducationLevel = factor(EducationLevel, ordered = FALSE))
#
# valid.dv <- predict(dv, newdata = tmp) %>% as_data_frame()
# valid.dv3 <- predict(dv3, newdata = tmp) %>% as_data_frame()
#
# tmp <- testing %>%
#     mutate(Income = factor(Income, ordered = FALSE),
#            EducationLevel = factor(EducationLevel, ordered = FALSE))
#
# test.dv <- predict(dv, newdata = tmp) %>% as_data_frame()
# test.dv3 <- predict(dv3, newdata = tmp) %>% as_data_frame()

# create an "Unknown" level for factor vars before converting to dummy vars
tmp <- train.data %>%
    mutate_each(funs(as.character), -USER_ID, -YOB, -Party) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB, -Party) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB, -Party)

dv2 <- dummyVars(~ ., data = tmp[, -7])
train.dv2 <- predict(dv2, newdata = tmp[, -7]) %>% as_data_frame()

# dv4 <- dummyVars(~ ., data = tmp[, -7], fullRank = TRUE)
# train.dv4 <- predict(dv4, newdata = tmp[, -7]) %>% as_data_frame()

tmp <- valid.data %>%
    mutate_each(funs(as.character), -USER_ID, -YOB, -Party) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB, -Party) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB, -Party)

valid.dv2 <- predict(dv2, newdata = tmp[, -7]) %>% as_data_frame()
# valid.dv4 <- predict(dv4, newdata = tmp[, -7]) %>% as_data_frame()

tmp <- testing %>%
    mutate_each(funs(as.character), -USER_ID, -YOB) %>%
    mutate_each(funs(ifelse(is.na(.), "Unknown", .)), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

test.dv2 <- predict(dv2, newdata = tmp) %>% as_data_frame()
# test.dv4 <- predict(dv4, newdata = tmp) %>% as_data_frame()

rm(tmp)

# impute missing data for YOB variable
library(mice)

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

train.mice <- get_impute("train_mice.Rds", train.dv2)
train.dv2i <- complete(train.mice)

valid.mice <- get_impute("valid_mice.Rds", valid.dv2, train.mice$method)
valid.dv2i <- complete(valid.mice)

test.mice <- get_impute("test_mice.Rds", test.dv2, train.mice$method)
test.dv2i <- complete(test.mice)

# high correlation -------------------------------------

# hcor <- cor(train.dv, use = "na.or.complete")
# hc <- findCorrelation(hcor)
# train.hc <- train.dv[, -hc]
# valid.hc <- valid.dv[, -hc]
# test.hc <- test.dv[, -hc]

hcor2 <- cor(train.dv2i, use = "na.or.complete")
hc2 <- findCorrelation(hcor2)
train.hc2 <- train.dv2i[, -hc2]
valid.hc2 <- valid.dv2i[, -hc2]
test.hc2 <- test.dv2i[, -hc2]

# hcor4 <- cor(train.dv4, use = "na.or.complete")
# hc4 <- findCorrelation(hcor4)
# train.hc4 <- train.dv4[, -hc4]
# valid.hc4 <- valid.dv4[, -hc4]
# test.hc4 <- test.dv4[, -hc4]

lc <- findLinearCombos(train.hc2[, -2])
train.lc <- train.hc2[, -lc$remove]
valid.lc <- valid.hc2[, -lc$remove]
test.lc <- test.hc2[, -lc$remove]

# save data --------------------------------------------


