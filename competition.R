# competition

library(readr)
library(dplyr)
library(caret)

# get training and testing data
train <- read_csv("train2016.csv") %>%
    mutate_each(funs(factor), -USER_ID, -YOB)

test <- read_csv("test2016.csv") %>%
    mutate_each(funs(factor), -USER_ID, -YOB)

# create validation set from training data
inTrain <- createDataPartition(y = train$Party, p = 0.75, list = FALSE)
train.data <- train[inTrain, ]
valid.data <- train[-inTrain, ]

# preproccess data -------------------------------------
train.set <- select(train.data, -USER_ID, -Party)
near.zero <- nearZeroVar(train.set)
train.set <- train.set[]
