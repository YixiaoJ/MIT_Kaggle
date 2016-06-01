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

# function to run multiple sets of models
run_models <- function(model, data, valid, prep = NULL, ctrl = trCtrl, ...) {
    require(caretEnsemble)
    specs <- vector("list", length(prep))

    for (i in seq_along(specs)) {
        specs[[i]] <- caretModelSpec(model, preProcess = prep[[i]])
    }

    names(specs) <- paste0(model, seq_along(specs))

    cl <- caretList(x = data, y = train.party, metric = "ROC",
                    trControl = ctrl, tuneList = specs,
                    continue_on_fail = TRUE, ...)

    cm <- map(cl, predict, newdata = valid) %>%
        map(confusionMatrix, valid.party)

    list(cl = cl, cm = cm)
}


# modelCor(resamples(cl))
# preds <- predict(cl, newdata = valid.set[, -1])
# predResp <- map(cl, predict, newdata = valid.set[, -1]) %>% as_data_frame()

