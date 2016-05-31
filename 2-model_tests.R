# model tests

source("1-data_sets.R")

# set seeds
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

seeds[[51]] <- sample.int(1000, 1)

# set train control
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds,
                       classProbs = TRUE, returnResamp = "final",
                       summaryFunction = twoClassSummary)

# use parallel cores
library(doParallel)
registerDoParallel()


run_pred <- function(model, data) {
    predict(model, newdata = data[, -1], na.action = na.pass)
}

run_cm <- function(pred) {
    confusionMatrix(pred, valid.party)
}

mods <- list(train.dv, train.hc)

ctrl <- trainControl(number = 1, repeats = 1)

tries <- map(mods, ~ train(x = .x[, -1], y = train.party, trainControl = ctrl,
                           method = "glm", preProcess = "knnImpute"))
