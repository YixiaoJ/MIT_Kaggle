# competition

library(readr)
library(dplyr)
library(caret)

library(doParallel)
registerDoParallel()

# get training and testing data
train <- read_csv("train2016.csv") %>%
    mutate_each(funs(factor), -USER_ID, -YOB)

# check for incorrect data in YOB
summary(train$YOB)
tmp <- filter(train, YOB < 1940 | YOB > 2000)


test <- read_csv("test2016.csv") %>%
    mutate_each(funs(factor), -USER_ID, -YOB)

# create validation set from training data
inTrain <- createDataPartition(y = train$Party, p = 0.75, list = FALSE)
train.data <- train[inTrain, ]
valid.data <- train[-inTrain, ]

# preproccess data -------------------------------------
train.set <- dplyr::select(train.data, -USER_ID, -Party)
valid.set <- dplyr::select(valid.data, -USER_ID, -Party)
test.set <- dplyr::select(test, -USER_ID)

# how many complete cases
# comp <- complete.cases(train.set)
# sum(comp)

# impute missing data
library(mice)
# miss <- md.pattern(train.set)
# miss <- md.pairs(train.set)

# use mice to impute missing values

get_impute <- function(file.save, set, method = NULL) {
    if (file.exists(file.save)) {
        imp <- readRDS(file.save)
    } else {
        if(!is.null(method)) {
            imp <- mice(set, method = method)
        } else {
            imp <- mice(set)
        }
        saveRDS(imp, file.save)
    }
    imp
}

train.imp <- get_impute("train_imp.Rds", train.set)
train.comp <- complete(train.imp)

valid.imp <- get_impute("valid_imp.Rds", valid.set, train.imp$method)
valid.comp <- complete(valid.imp)

test.imp <- get_impute("test_imp.Rds", test.set, train.imp$method)
test.comp <- complete(test.imp)

# check for near-zero covariates
# near.zero.fields <- nearZeroVar(train.set, saveMetrics = TRUE)
# cond <- checkConditionalX(train.set, levels(train.set$Q124742))
# near.zero <- nearZeroVar(train.set)
# train.set <- train.set[, -near.zero]

# remove highly-correlated variables - doesn't work for categorical data
# train.cor <- cor(train.set, use = "complete.obs")
# cor(train.set$Gender, train.set$Income)

set.seed(1235)
pre.proc <- preProcess(train.comp, outcome = train.data$Party,
                       method = c("conditionalX"))

train.comp <- predict(pre.proc, train.comp)
train.comp$Party <- train.data$Party

valid.comp <- predict(pre.proc, valid.comp)
valid.comp$Party <- valid.data$Party

test.comp <- predict(pre.proc, test.comp)

# random forest ----------------------------------------

myseeds <- list(c(123,234,345), c(456,567,678), c(789,890,901),
                c(987,876,765), c(654,543,432), c(321,210,109),
                c(135,246,357), c(468,579,680), c(791,802,913),
                c(975,864,753), 54321)

trCtrl <- trainControl(method="repeatedcv", seeds=myseeds)

library(randomForest)
modelRF <- train(Party ~ ., method="rf", data=train.comp, trControl=trCtrl)

# boosting ---------------------------------------------

library(gbm)
modelGBM <- train(Party ~ ., method="gbm", data=train.comp, verbose=FALSE,
                  trControl=trCtrl)

# lda --------------------------------------------------

modelLDA <- train(Party ~ ., method="lda", data=train.comp, trControl=trCtrl)

# confusion matrix -------------------------------------

predRF <- predict(modelRF, newdata = valid.comp)
cmRF <- confusionMatrix(predRF, valid.comp$Party)

predGBM <- predict(modelGBM, newdata = valid.comp)
cmGBM <- confusionMatrix(predGBM, valid.comp$Party)

predLDA <- predict(modelLDA, newdata = valid.comp)
cmLDA <- confusionMatrix(predLDA, valid.comp$Party)

predDF <- data.frame(predRF, predGBM, predLDA, Party = valid.comp$Party)
modStack <- train(Party ~ ., data = predDF, method = "rf")
predStack <- predict(modStack, newdata = valid.comp)
cmStack <- confusionMatrix(predStack, valid.comp$Party)

# out of sample error ----------------------------------


# Model | Out of Sample Error
# ------|------------------------------
#     Random Forest | `r round((1 - cmRF$overall["Accuracy"]) * 100, 2)`%
# Boosting | `r round((1 - cmGBM$overall["Accuracy"]) * 100, 2)`%
# Linear Discriminate Analysis | `r round((1 - cmLDA$overall["Accuracy"]) * 100, 2)`%
