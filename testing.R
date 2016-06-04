# testing

source("2-model_tests.R")

# library(mda)
# library(earth)
# train2 <- train.dv[, -c(1, hc.dv)]
# train2$Party <- train.party
#
# names(train2) <- make.names(names(train2))
# valid <- valid.dv[, -c(1, hc.dv)]
# names(valid) <- make.names(names(valid))
#
# # mdl <- mda(Party ~ ., data = train2, subclasses = 3)
# mdl <- fda(Party ~ ., data = train2, method = earth)
# summary(mdl$fit)
# pred <- predict(mdl, newdata = valid)
# confusionMatrix(pred, valid.party)
#
# mdl <- train(Party ~ ., data = train2, method = "fda")
# mdl3 <- train(x = train.dv[, -c(1, hc.dv)], y = train.party, method = "fda")
# pred3 <- predict(mdl3, newdata = valid.dv[, -c(1, hc.dv)])
# confusionMatrix(pred3, valid.party)
#
# vi <- varImp(mdl)
# vars <- vi$importance %>% mutate(vars = rownames(vi$importance)) %>% filter(Overall > 0)
#
# train3 <- train2[, vars$vars]
# train3$Party <- train.party
# valid2 <- valid[, vars$vars]
#
# mdl2 <- train(Party ~ ., data = train3, method = "fda")
#
# pred2 <- predict(mdl2, newdata = valid2)
# confusionMatrix(pred2, valid.party)

# library(xgboost)
#
# party <- as.numeric(train.party == "Republican")
# xgb.data <- xgb.DMatrix(as.matrix(train.dv), label = party)
# xgb.valid <- xgb.DMatrix(as.matrix(valid.dv), label = as.numeric(valid.party == "Republican"))
#
# xgb <- xgboost(data = xgb.data, nround = 25,
#                objective = "binary:logistic", eta = 0.1, max.depth = 15)
#
# pred <- predict(xgb, xgb.valid)
# predXGB <- ifelse(pred >= 0.5, "Republican", "Democrat")
# confusionMatrix(predXGB, valid.party)
#
# model <- xgb.dump(xgb, with.stats = TRUE)
# model[1:10]
# mdl.names <- names(train.dv)
#
# import <- xgb.importance(mdl.names, model = xgb)
# xgb.plot.importance(import[1:20, ])
#
# chisq.test(train.dv$Q109244.Yes, party)
# chisq.test(train.dv$Q115611.Yes, party)
# chisq.test(train.dv$Gender.Male, party)
# chisq.test(train.dv$Q121011.No, party)
# chisq.test(train.dv$Q98197.Yes, party)
# chisq.test(train.dv$EducationLevel.NA, party)
# chisq.test(train.dv$Q122120.Yes, party)
#
# keep <- import[1:14, ]$Feature
#
# xgb.keep <- xgb.DMatrix(as.matrix(train.dv[, keep]), label = party)
# xgb <- xgboost(data = xgb.keep, nround = 25,
#                objective = "binary:logistic", eta = 0.1, max.depth = 15)
# pred <- predict(xgb, xgb.valid)
# predXGB <- ifelse(pred >= 0.5, "Republican", "Democrat")
# confusionMatrix(predXGB, valid.party)
#
# # xgboost fitting with arbitrary parameters
# xgb_params_1 = list(
#     objective = "binary:logistic",                                               # binary classification
#     eta = 0.01,                                                                  # learning rate
#     max.depth = 3,                                                               # max tree depth
#     eval_metric = "auc"                                                          # evaluation/loss metric
# )
#
# xgb_1 = xgboost(data = as.matrix(train.dv),
#                 label = as.numeric(train.party == "Republican"),
#                 params = xgb_params_1,
#                 nrounds = 100,                                                 # max number of trees to build
#                 verbose = TRUE,
#                 print.every.n = 1,
#                 early.stop.round = 10                                          # stop if no improvement within 10 trees
# )
#
# # cross-validate xgboost to get the accurate measure of error
# xgb_cv_1 = xgb.cv(params = xgb_params_1,
#                   data = as.matrix(train.dv),
#                   label = as.numeric(train.party == "Republican"),
#                   nrounds = 100,
#                   nfold = 5,                                                   # number of folds in K-fold
#                   prediction = TRUE,                                           # return the prediction using the final model
#                   showsd = TRUE,                                               # standard deviation of loss across folds
#                   stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
#                   verbose = TRUE,
#                   print.every.n = 1,
#                   early.stop.round = 10
# )
#
# library(tidyr)
# # plot the AUC for the training and testing samples
# xgb_cv_1$dt %>%
#     select(-contains("std")) %>%
#     mutate(IterationNum = 1:n()) %>%
#     gather(TestOrTrain, AUC, -IterationNum) %>%
#     ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) +
#     geom_line() +
#     theme_bw()
#
# # set up the cross-validated hyper-parameter search
# xgb_grid_1 = expand.grid(
#     nrounds = 1000,
#     eta = c(0.01, 0.001, 0.0001),
#     max_depth = c(2, 4, 6, 8, 10),
#     gamma = c(0.1, 0.5, 1),
#     colsample_bytree = c(0.1, 0.5, 1),
#     min_child_weight = c(0.1, 0.5, 1)
# )
#
# # pack the training control parameters
# xgb_trcontrol_1 = trainControl(
#     method = "cv",
#     number = 5,
#     verboseIter = TRUE,
#     returnData = FALSE,
#     returnResamp = "all",                                                        # save losses across all models
#     classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
#     summaryFunction = twoClassSummary,
#     allowParallel = TRUE
# )
#
# xgb_train_1 = train(
#     x = as.matrix(train.dv),
#     y = train.party,
#     trControl = xgb_trcontrol_1,
#     tuneGrid = xgb_grid_1,
#     method = "xgbTree"
# )
#
# pred.xbg <- predict(xgb_train_1, newdata = as.matrix(valid.dv))
# confusionMatrix(pred.xbg, valid.party)

# c50Grid <- expand.grid(
#     .winnow = c(TRUE, FALSE),
#     .trials = c(1:9, 1:10 * 10),
#     .model = c("tree", "rules")
# )
#
# c50 <- train(
#     Party ~ .,
#     data = train.data,
#     # y = train.party,
#     method = "C5.0",
#     trControl = trCtrl,
#     na.action = na.pass,
#     metric = "ROC",
#     tuneGrid = c50Grid
# )

# best C5.0: trials = 20, model = rules, winnow = TRUE

train.dvp <- train.dv %>%
    select(-USER_ID, -hc.dv) %>%
    mutate(Party = train.party)

fda <- train(
    Party ~ .,
    data = train.dvp,
    method = "fda",
    trControl = trCtrl,
    metric = "ROC",
    tuneGrid = expand.grid(.degree = 1:2, .nprune = 2:20)
)

ctrl <- trainControl(
    method = "repeatedcv",
    seeds = seeds,
    classProbs = TRUE,
    returnResamp = "final",
    summaryFunction = twoClassSummary,
    savePredictions = "final"
)

# library(mda)
# set.seed(1056)
# mdl <- fda(Party ~ ., train.dvp, method = mars)
# pred <- predict(mdl, newdata = valid.dv[, -c(1, hc.dv)])
# confusionMatrix(pred, valid.party)

set.seed(1056)
mdl <- train(
    Party ~ .,
    data = train.dvp,
    method = "stepLDA",
    trControl = trCtrl,
    metric = "ROC"
)
