# competition

library(readr)
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(caret)
library(mice)

library(doParallel)
registerDoParallel()

income <- c("under 25", "25-50", "50-74", "75-100", "100-150", "Over 150")
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

# create validation set from training data
inTrain <- createDataPartition(y = training$Party, p = 0.75, list = FALSE)
train.data <- training[inTrain, ]
valid.data <- training[-inTrain, ]

# impute -----------------------------------------------

get_impute <- function(file.save, set, method = NULL) {
    if (file.exists(file.save)) {
        imp <- readRDS(file.save)
    } else {
        if (!is.null(method)) {
            imp <- mice(set, method = method)
        } else {
            imp <- mice(set)
        }
    }
    imp
}

train.mice <- get_impute("train_mice.Rds", train.data)
train.mice.data <- complete(train.mice)

valid.mice <- get_impute("valid_mice.Rds", valid.data, train.mice$method)
valid.mice.data <- complete(valid.mice)

test.mice <- get_impute("test_mice.Rds", valid.data, train.mice$method)
test.mice.data <- complete(test.mice)

# modLME <- glmer(Party ~ ., data = train.mice.data[, -1], family = "binomial")

# with(data = train.mice, exp = glm(Party ~ YOB + Income, family = "binomial"))
# comp <- complete(train.imp)

# library(missForest)
# train.imp <- missForest(train.data[, 8])

# visualization ----------------------------------------

# featurePlot(train.data[, 2], train.data$Party, plot = "box")
library(ggplot2)
train.data %>%
    select(USER_ID, Party, Gender) %>%
    filter(!is.na(Gender)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(Gender, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(Gender, Percent, -Party) %>%
    ggplot(aes(x = Gender, y = Percent, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

train.data %>%
    select(USER_ID, Party, Income) %>%
    filter(!is.na(Income)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(Income, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(Income, Percent, -Party) %>%
    ggplot(aes(x = Income, y = Percent, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

train.data %>%
    select(USER_ID, Party, HouseholdStatus) %>%
    filter(!is.na(HouseholdStatus)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(HouseholdStatus, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(HouseholdStatus, Percent, -Party) %>%
    ggplot(aes(x = HouseholdStatus, y = Percent, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

train.data %>%
    select(USER_ID, Party, EducationLevel) %>%
    filter(!is.na(EducationLevel)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(EducationLevel, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(EducationLevel, Percent, -Party) %>%
    ggplot(aes(x = EducationLevel, y = Percent, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# preproccess data -------------------------------------
train.set <- dplyr::select(train.data, -USER_ID, -Party)
train.dv <- dummyVars(~ ., data = train.set)
train.dvp <- predict(train.dv, newdata = train.set) %>% as_data_frame()

set.seed(1235)
pre.proc <- preProcess(train.dvp, outcome = train.data$Party,
                       method = "knnImpute")

train.proc <- predict(pre.proc, train.dvp)
train.result <- as.factor(train.data$Party)

valid.set <- dplyr::select(valid.data, -USER_ID, -Party)
valid.dvp <- predict(train.dv, newdata = valid.set) %>% as_data_frame()
valid.proc <- predict(pre.proc, valid.dvp)
valid.result <- as.factor(valid.data$Party)

test.set <- dplyr::select(testing, -USER_ID)
test.dvp <- predict(train.dv, newdata = test.set) %>% as_data_frame()
test.proc <- predict(pre.proc, test.dvp)

# individual models ------------------------------------

myseeds <- list(c(123,234,345), c(456,567,678), c(789,890,901),
                c(987,876,765), c(654,543,432), c(321,210,109),
                c(135,246,357), c(468,579,680), c(791,802,913),
                c(975,864,753), 54321)

trCtrl <- trainControl(method = "repeatedcv", seeds = myseeds, classProbs = TRUE)

# http://stats.stackexchange.com/questions/95212/improve-classification-with-many-categorical-variables
# AdaBoost
# lme4
# ranger


# library(randomForest)
# library(gbm)

# mod <- "modelRF.Rds"
# if(file.exists(mod)) {
#     modelRF <- readRDS(mod)
# } else {
#     modelRF <- train(x = train.proc, y = train.result, method="rf",
#                      trControl=trCtrl)
#     saveRDS(modelRF, mod)
# }

# modelGLM <- train(x = train.proc, y = train.result, method = "glm",
#                   trControl = trCtrl)
library(C50)

myseeds2 <- list(c(123,234,345,456), c(456,567,678,789), c(789,890,901,012),
                c(987,876,765,654), c(654,543,432,321), c(321,210,109,098),
                c(135,246,357,468), c(468,579,680,802), c(791,802,913,135),
                c(975,864,753,642), 54321)
trCtrl2 <- trainControl(method = "repeatedcv", seeds = myseeds2, classProbs = TRUE)
trCtrl3 <- trainControl(method = "LOOCV", number = 10, repeats = 10, classProbs = TRUE)

modelGBM <- train(x = train.proc, y = train.result, method = "C5.0",
                  verbose = FALSE, trControl = trCtrl3)

modelLDA <- train(x = train.proc, y = train.result, method = "lda",
                  trControl = trCtrl2)

modelBag <- train(x = train.proc, y = train.result, method = "treebag",
                  trControl = trCtrl2)

# modelSVM <- train(x = train.proc, y = train.result, method = "svmRadial",
#                   trControl = trCtrl)

# confusion matrix -------------------------------------

# predRF <- predict(modelRF, newdata = valid.proc)
# cmRF <- confusionMatrix(predRF, valid.data$Party)
#
# predGLM <- predict(modelGLM, newdata = valid.proc)
# cmGLM <- confusionMatrix(predGLM, valid.data$Party)
#
predGBM <- predict(modelGBM, newdata = valid.proc)
cmGBM <- confusionMatrix(predGBM, valid.data$Party)
#
predLDA <- predict(modelLDA, newdata = valid.proc)
cmLDA <- confusionMatrix(predLDA, valid.data$Party)

predBag <- predict(modelBag, newdata = valid.proc)
cmBag <- confusionMatrix(predBag, valid.data$Party)

# PredSVM <- predict(modelSVM, newdata = valid.proc)
# cmSVM <- confusionMatrix(PredSVM, valid.data$Party)

# treebag, rpart, glm, knn, svmRadial

# stacked model ----------------------------------------

library(caretEnsemble)

# models <- caretList(x = train.proc, y = train.result, trControl = trCtrl,
#                     methodList = c("glm", "gbm", "lda", "treebag", "rf", "svmRadial"))
#
models <- caretList(x = train.proc, y = train.result, trControl = trCtrl2,
                    methodList = c("glm", "C5.0", "rf", "svmRadial"))

stack <- caretStack(models, method = "glm")

predStack <- predict(stack, newdata = valid.proc)
cmStack <- confusionMatrix(predStack, valid.data$Party)

# test data --------------------------------------------

# testRF <- predict(modelRF, newdata = test.proc)
# testGBM <- predict(modelGBM, newdata = test.proc)
# testLDA <- predict(modelLDA, newdata = test.proc)

predTest <- predict(stack, newdata = test.proc)

test.submit <- data_frame(USER_ID = testing$USER_ID, Predictions = predTest)

mod <- str_replace_all(as.character(Sys.time()), "-|:| ", "")
write_csv(test.submit, paste0("submission_", mod, ".csv"))
