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

# create validation set from training data
inTrain <- createDataPartition(y = training$Party, p = 0.85, list = FALSE)
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
        saveRDS(imp, file.save)
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

orig <- train.data %>%
    select(USER_ID, Party, Income) %>%
    filter(!is.na(Income)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(Income, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(Income, Percent, -Party) %>%
    mutate(Set = "orig")

imp <- train.mice.data %>%
    select(USER_ID, Party, Income) %>%
    filter(!is.na(Income)) %>%
    group_by(USER_ID, Party) %>%
    mutate(value = TRUE) %>%
    spread(Income, value, fill = FALSE) %>%
    group_by(Party) %>%
    summarize_each(funs(mean), -USER_ID) %>%
    gather(Income, Percent, -Party) %>%
    mutate(Set = "imp")

bind_rows(orig, imp) %>%
    ggplot(aes(x = Income, y = Percent, fill = Party)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(. ~ Set) +
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

# baseline model ---------------------------------------

baseline <- table(valid.data$Party)
baseline[1] / nrow(valid.data)

# preproccess data -------------------------------------
# train.set <- dplyr::select(train.data, -USER_ID, -Party)
# train.dv <- dummyVars(~ ., data = train.data[, -7])
# train.dvp <- predict(train.dv, newdata = train.data[, -7]) %>% as_data_frame()

library(purrr)
train.set <- dmap_if(train.data[, -7], is.factor, as.numeric)
train.set$Party <- train.data$Party
valid.set <- dmap_if(valid.data[, -7], is.factor, as.numeric)
test.set <- dmap_if(testing, is.factor, as.numeric)

train.set[is.na(train.set)] <- 0
valid.set[is.na(valid.set)] <- 0

nz <- nearZeroVar(train.set[, -108], freqCut = 85/15)
train.nz <- train.set[, -nz]

hcor <- cor(train.set[, -108], use = "na.or.complete")
hc <- findCorrelation(hcor, cutoff = 0.7)
train.hc <- train.set[, -hc]
valid.hc <- valid.set[, -hc]

# set.seed(1235)
# pre.proc <- preProcess(train.set, outcome = train.data$Party, method = "bagImpute")
#
# train.proc <- predict(pre.proc, newdata = train.set)
# train.proc$Party <- train.data$Party
# train.result <- as.factor(train.data$Party)

# valid.set <- dmap_if(valid.data[, -7], is.factor, as.numeric)
# valid.dvp <- predict(train.dv, newdata = valid.data[, -7]) %>% as_data_frame()
# valid.proc <- predict(pre.proc, valid.set)
# valid.proc$Party <- valid.data$Party
# valid.result <- as.factor(valid.data$Party)

# test.set <- dmap_if(testing, is.factor, as.numeric)
# test.dvp <- predict(train.dv, newdata = testing) %>% as_data_frame()
# test.proc <- predict(pre.proc, test.set)


# myseeds <- list(c(123,234,345), c(456,567,678), c(789,890,901),
#                 c(987,876,765), c(654,543,432), c(321,210,109),
#                 c(135,246,357), c(468,579,680), c(791,802,913),
#                 c(975,864,753), 54321)
#

# tuning parameters ------------------------------------
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

seeds[[51]] <- sample.int(1000, 1)

# trCtrl <- trainControl(method = "repeatedcv", classProbs = TRUE)
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds,
                       classProbs = TRUE, returnResamp = "all")

# individual models ------------------------------------

modelGLM <- train(Party ~ ., data = train.hc[, -1], method = "glm", trControl = trCtrl)
predGLM <- predict(modelGLM, newdata = valid.hc[, -1])
cmGLM <- confusionMatrix(predGLM, valid.data$Party)
cmGLM

featurePlot(x = train.set[, 16], y = train.data$Party,
            plot = "density", auto.key = list(columns = 2))


# remove Q123464 (9); Q122771 (13); Q122120 (14), Q121699 (15), Q121700 (16)

# 0.6024
modelAB <- train(Party ~ ., data = train.data[, -1], method = "adaboost", trControl = trCtrl, na.action = na.pass)
predAB <- predict(modelAB, newdata = valid.data[, -c(1, 7)], na.action = na.pass)
cmAB <- confusionMatrix(predAB, valid.data$Party)
saveRDS(modelAB, "model_adaboost.Rds")

modelAB <- train(Party ~ ., data = train.proc[, -1], method = "adaboost", na.action = na.pass, trControl = trCtrl)
predAB <- predict(modelAB, newdata = valid.proc[, -1], na.action = na.pass)
cmAB <- confusionMatrix(predAB, valid.data$Party)
cmAB

modelAB2 <- train(Party ~ ., data = train.proc[, -1], method = "AdaBoost.M1", na.action = na.pass, trControl = trCtrl)
predAB2 <- predict(modelAB2, newdata = valid.proc[, -1], na.action = na.pass)
cmAB2 <- confusionMatrix(predAB2, valid.data$Party)
cmAB2



# 0.6147
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds, returnResamp = "all")
modelC50 <- train(Party ~ ., data = train.data[, -1], method = "C5.0", na.action = na.pass, trControl = trCtrl, tuneGrid = trGrid)
predC50 <- predict(modelC50, newdata = valid.data[, -c(1, 7)], na.action = na.pass)
cmC50 <- confusionMatrix(predC50, valid.data$Party)
cmC50

# 0.6403
modelC50b <- train(Party ~ ., data = train.proc[, -1], method = "C5.0", na.action = na.pass, trControl = trCtrl)
predC50b <- predict(modelC50b, newdata = valid.proc[, -1], na.action = na.pass)
cmC50b <- confusionMatrix(predC50b, valid.data$Party)
cmC50b

trGrid <- expand.grid(.winnow = FALSE, .trials = c(1, 5, 10, 20, 30), .model = c("tree", "rules"))

# no imputing, box-cox
modelC50 <- train(Party ~ ., data = train.set[, -1], method = "C5.0",
                  na.action = na.pass, trControl = trCtrl, tuneLength = 5,
                  preProcess = "BoxCox")
predC50 <- predict(modelC50, newdata = valid.set[, -1], na.action = na.pass)
cmC50 <- confusionMatrix(predC50, valid.data$Party)
cmC50

saveRDS(modelC50, "submit_model_2.Rds")

# knn
modelKNN <- train(Party ~ ., data = train.set[, -1], method = "knn",
                  na.action = na.pass, trControl = trCtrl, tuneLength = 10,
                  preProcess = "knnImpute")
predKNN <- predict(modelKNN, newdata = valid.set[, -1], na.action = na.pass)
cmKNN <- confusionMatrix(predKNN, valid.data$Party)
cmKNN

# bag imputing
modelC50b <- train(Party ~ ., data = train.set[, -1], method = "C5.0",
                   na.action = na.pass, trControl = trCtrl, tuneLength = 20,
                   preProcess = "bagImpute")
predC50b <- predict(modelC50b, newdata = valid.set[, -1], na.action = na.pass)
cmC50b <- confusionMatrix(predC50b, valid.data$Party)
cmC50b

xyplot(modelC50, type = c("g", "p", "smooth"))
plot(modelC50)
# 0.624
modelC50i <- train(Party ~ ., data = train.mice.data[, -1], method = "C5.0", trControl = trCtrl, tuneGrid = trGrid)
predC50i <- predict(modelC50i, newdata = valid.mice.data[, -c(1, 7)])
cmC50i <- confusionMatrix(predC50i, valid.data$Party)
cmC50i

xyplot(modelC50i, type = c("g", "p", "smooth"))
plot(modelC50i)

# 0.606
modelC50p <- train(Party ~ ., data = train.proc[, -1], method = "C5.0", trControl = trCtrl, tuneGrid = trGrid)
predC50p <- predict(modelC50p, newdata = valid.proc[, -c(1, 224)])
cmC50p <- confusionMatrix(predC50p, valid.data$Party)
cmC50p

# select filter ----------------------------------------

filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(Party ~ ., data = train.proc[, -1], sbfControl = filterCtrl, na.action = na.pass)

predRFwF <- predict(rfWithFilter, newdata = valid.mice.data[, -1])

# custom -----------------------------------------------
source("custom_C50.R")

mygrid <- expand.grid(trials=c(1, 1:4*10),
                      model=c('rules', 'tree'),
                      winnow=FALSE,
                      fuzzy=c(TRUE, FALSE),
                      cutoff=c(0.01, seq(0.025, 0.5, by=0.025)))

mycontrol <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          classProbs = TRUE,
                          summaryFunction = fourStats)

set.seed(949)
mod1 <- train(Party ~ ., data = train.set[, -1],
              method = my_mod,
              ## Minimize the distance to the perfect model
              metric = "Dist",
              maximize = FALSE,
              tuneGrid = mygrid,
              trControl = mycontrol,
              na.action = na.pass,
              preProcess = "BoxCox")

pred.mod1 <- predict(mod1, newdata = valid.set[, -1], na.action = na.pass)
cm.mod1 <- confusionMatrix(pred.mod1, valid.data$Party)
cm.mod1

xyplot(mod1, type = c("g", "p", "smooth"))
plot(mod1)

# 0.6305
modelRngr <- train(Party ~ ., data = train.mice.data[, -1], method = "ranger", trControl = trCtrl, na.action = na.pass)
predRngr <- predict(modelRngr, newdata = valid.mice.data[, -c(1, 7)], na.action = na.pass)
cmRngr <- confusionMatrix(predRngr, valid.data$Party)

# 0.6111
modelRngrp <- train(Party ~ ., data = train.proc[, -1], method = "ranger", trControl = trCtrl, na.action = na.pass)
predRngrp <- predict(modelRngrp, newdata = valid.proc[, -c(1, 224)])
cmRngrp <- confusionMatrix(predRngrp, valid.data$Party)
cmRngrp

# 0.6118
modelLDA <- train(Party ~ ., data = train.mice.data[, -1], method = "lda", trControl = trCtrl)
predLDA <- predict(modelLDA, newdata = valid.mice.data[, -c(1, 7)], na.action = na.pass)
cmLDA <- confusionMatrix(predLDA, valid.data$Party)

# 0.6168
modelSVM <- train(Party ~ ., data = train.mice.data[, -1], method = "svmRadial", trControl = trCtrl)
predSVM <- predict(modelSVM, newdata = valid.mice.data[, -c(1, 7)], na.action = na.pass)
cmSVM <- confusionMatrix(predSVM, valid.data$Party)


# modelRF <- train(Party ~ ., data = train.data[, -1], method = "rf", trControl = trCtrl, na.action = na.pass)

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
models <- caretList(Party ~ ., data = train.mice.data[, -1], trControl = trCtrl,
                    methodList = c("C5.0", "ranger", "svmRadial"))

stack <- caretStack(models, method = "glm")

predStack <- predict(stack, newdata = valid.mice.data[, -c(1, 7)])
cmStack <- confusionMatrix(predStack, valid.data$Party)


predDF <- data.frame(predRF, predGBM, predLDA, Party = valid.comp$Party)
modStack <- train(Party ~ ., data = predDF, method = "rf")
predStack <- predict(modStack, newdata = valid.comp)
cmStack <- confusionMatrix(predStack, valid.comp$Party)

# test data --------------------------------------------

# testRF <- predict(modelRF, newdata = test.proc)
# testGBM <- predict(modelGBM, newdata = test.proc)
# testLDA <- predict(modelLDA, newdata = test.proc)

# predTest <- predict(stack, newdata = test.proc)
predTest <- predict(modelC50, newdata = test.set, na.action = na.pass)

test.submit <- data_frame(USER_ID = testing$USER_ID, Predictions = predTest)

mod <- str_replace_all(as.character(Sys.time()), "-|:| ", "")
write_csv(test.submit, paste0("submission_", mod, ".csv"))
