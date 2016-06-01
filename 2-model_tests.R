# model tests

source("1-data_sets.R")

# use parallel cores
library(doParallel)
registerDoParallel()

# set seeds
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

seeds[[51]] <- sample.int(1000, 1)

# set train control
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds,
                       classProbs = TRUE, returnResamp = "final",
                       summaryFunction = twoClassSummary,
                       index = createMultiFolds(train.party, 10, 5),
                       savePredictions = "final")


# run_pred <- function(model, data) {
#     predict(model, newdata = data[, -1], na.action = na.pass)
# }
#
# run_cm <- function(pred) {
#     confusionMatrix(pred, valid.party)
# }
#
# mods <- list(train.dv, train.hc)
# mthd <- list("lda", "gbm")
# prep <- c("knnImpute", "nzv")
#
# mod.cross <- list(x = mods, method = mthd, preProcess = prep)
# cross.df <- cross_d(mod.cross)
#
# test <- invoke("train", mods, y = train.party, method = "gbm", trControl = ctrl, preProcess = "nzv")
#
# ctrl <- trainControl(method = "none", number = 1, repeats = 1)
#
# tries <- map(mods, ~ train(x = .x[, -1], y = train.party, trControl = ctrl, method = "lda", preProcess = "knnImpute"))
# preds <- map(tries, predict)
# glm1 <- train(x = train.set[, -1], y = train.party, method = "glm", trControl = trCtrl, metric = "ROC",
#               preProcess = "knnImpute")
#
# try <- train(x = train.dv[, -1], y = train.party, trainControl = ctrl, method = "lda", preProcess = "knnImpute")

library(caretEnsemble)

model <- "glm"
prep <- list("knnImpute", c("nzv", "knnImpute"), c("nzv", "knnImpute", "pca"))
specs <- vector("list", length(prep))

for (i in seq_along(specs)) {
    specs[[i]] <- caretModelSpec(model, preProcess = prep[[i]])
}

names(specs) <- paste0("m", seq_along(specs))

data <- train.set[, -1]
cl <- caretList(x = data, y = train.party, metric = "ROC", trControl = trCtrl,
                tuneList = specs)

modelCor(resamples(cl))
preds <- predict(cl, newdata = valid.set[, -1])
predResp <- map(cl, predict, newdata = valid.set[, -1]) %>% as_data_frame()
