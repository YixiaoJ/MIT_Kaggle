# model tests

source("1-data_sets.R")

# use parallel cores
library(doParallel)
registerDoParallel()

# set seeds
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 40)

seeds[[51]] <- sample.int(1000, 1)

# set train control
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds,
                       classProbs = TRUE, returnResamp = "final",
                       summaryFunction = twoClassSummary,
                       index = createMultiFolds(train.party, 10, 5),
                       savePredictions = "final")

# function to run multiple sets of models
#' @param data A data frame with training data
#' @param valid A data frame with validation data
#' @param vars A numeric vector of variables to be removed from data and valid
#' @param models A list of models to be run
#' @param tune A list of either tuneLength or tuneGrid to be applied to each model
#' @param prep A list of preProcessing to be applied to each model
#' @param ctrl A trainControl object
#' @param ... Additional parameters to pass on to train
#'
#' @return A list of lists; a list of train models, and a list of confusion matrices
run_models <- function(data, valid, vars, models, tune = list(NULL), prep = list(NULL), ctrl = trCtrl, ...) {
    require(caretEnsemble)

    if (!is.null(tune[[1]])) {
        if (length(models) != length(tune)) stop("model list and tune list must be the same length")
    }

    models <- at_depth(models, 1, setNames, "method")
    if (!is.null(prep[[1]])) prep <- at_depth(prep, 1, setNames, "preProcess")

    param <- map2(models, tune, list) %>%
        map(flatten) %>%
        cross2(prep) %>%
        map(flatten)

    specs <- invoke_map(list(caretModelSpec), param)

    set.seed(1056)
    cl <- caretList(x = data[, -c(1, vars)], y = train.party, metric = "ROC",
                    trControl = ctrl, tuneList = specs,
                    continue_on_fail = TRUE, ...)

    cm <- map(cl, predict, newdata = valid[, -c(1, vars)]) %>%
        map(confusionMatrix, valid.party)

    list(cl = cl, cm = cm)
}

# mods <- list("glm", "lda", "pls") %>% at_depth(1, setNames, "method")
# prep <- list(list(c("nzv", "center", "scale")), list(c("nzv", "pca"))) %>% at_depth(1, setNames, "preProcess")
# tune <- list(list(NULL), list(tuneLength = 10), list(tuneGrid = expand.grid(.ncomp = 1:10)))
# tune <- list(NULL)
# prep <- list(NULL) %>% at_depth(1, setNames, "preProcess")
# mod.list <- map2(mods, tune, list) %>% map(flatten) %>% cross2(prep) %>% map(flatten)


# modelCor(resamples(cl))
# preds <- predict(cl, newdata = valid.set[, -1])
# predResp <- map(cl, predict, newdata = valid.set[, -1]) %>% as_data_frame()

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
