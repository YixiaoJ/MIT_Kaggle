# competition

library(readr)
library(plyr)
library(dplyr)
library(tibble)
library(stringr)
library(caret)

library(doParallel)
registerDoParallel()

# get training and testing data
training <- read_csv("train2016.csv") %>%
    mutate(Income = str_replace_all(Income, "\\$|,[0-9]{3}", ""))
    # mutate_each(funs(factor), -USER_ID, -YOB)

testing <- read_csv("test2016.csv") %>%
    mutate(Income = str_replace_all(Income, "\\$|,[0-9]{3}", ""))
    # mutate_each(funs(factor), -USER_ID, -YOB)

# create validation set from training data
inTrain <- createDataPartition(y = training$Party, p = 0.75, list = FALSE)
train.data <- training[inTrain, ]
valid.data <- training[-inTrain, ]

# preproccess data -------------------------------------
train.set <- select(train.data, -USER_ID, -Party)
train.dv <- dummyVars(~ ., data = train.set)
train.dvp <- predict(train.dv, newdata = train.set) %>% as_data_frame()

set.seed(1235)
pre.proc <- preProcess(train.dvp, outcome = train.data$Party,
                       method = c("knnImpute"))

train.proc <- predict(pre.proc, train.dvp)
train.proc$Party <- train.data$Party

valid.set <- select(valid.data, -USER_ID, -Party)
valid.dvp <- predict(train.dv, newdata = valid.set) %>% as_data_frame()
valid.proc <- predict(pre.proc, valid.dvp)
valid.proc$Party <- valid.data$Party

test.set <- select(testing, -USER_ID)
test.dvp <- predict(train.dv, newdata = test.set) %>% as_data_frame()
test.proc <- predict(pre.proc, test.dvp)

# random forest ----------------------------------------

myseeds <- list(c(123,234,345), c(456,567,678), c(789,890,901),
                c(987,876,765), c(654,543,432), c(321,210,109),
                c(135,246,357), c(468,579,680), c(791,802,913),
                c(975,864,753), 54321)

trCtrl <- trainControl(method="repeatedcv", seeds=myseeds)

library(randomForest)

mod <- "modelRF.Rds"
if(file.exists(mod)) {
    modelRF <- readRDS(mod)
} else {
    modelRF <- train(Party ~ ., method="rf", data=train.proc, trControl=trCtrl)
    saveRDS(modelRF, mod)
}

# boosting ---------------------------------------------

library(gbm)
modelGBM <- train(Party ~ ., method="gbm", data=train.proc, verbose=FALSE,
                  trControl=trCtrl)

# lda --------------------------------------------------

modelLDA <- train(Party ~ ., method="lda", data=train.proc, trControl=trCtrl)

# confusion matrix -------------------------------------

predRF <- predict(modelRF, newdata = valid.proc)
cmRF <- confusionMatrix(predRF, valid.data$Party)

predGBM <- predict(modelGBM, newdata = valid.proc)
cmGBM <- confusionMatrix(predGBM, valid.data$Party)

predLDA <- predict(modelLDA, newdata = valid.proc)
cmLDA <- confusionMatrix(predLDA, valid.data$Party)

predDF <- data.frame(predRF, predGBM, predLDA, Party = valid.data$Party)
modStack <- train(Party ~ ., data = predDF, method = "rf")
predStack <- predict(modStack, newdata = valid.proc)
cmStack <- confusionMatrix(predStack, valid.data$Party)

# test data --------------------------------------------

predTest <- predict(modStack, newdata = test.proc)

test.submit <- data_frame(USER_ID = test.data$USER_ID, Predictions = predTest)

write_csv(test.submit, "submission.csv")
