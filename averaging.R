# averaging

source("2-model_tests.R")
# library(readr)
# library(dplyr)
library(tibble)

pScore <- function(x, y) {
    numX <- length(unique(x))
    if (numX > 2) {
        out <- t.test(x ~ y)$p.value
    } else {
        out <- fisher.test(factor(x), y)$p.value
    }
    out
}

scores <- apply(X = train.data.n[, -c(1, 7)], MARGIN = 2, FUN = pScore, y = train.data.n$Party)
scores2 <- apply(X = train.dv[, -c(1, 331)], MARGIN = 2, FUN = pScore, y = train.dv$Party)
pval <- sort(scores[scores < 0.05])
pval.dv <- sort(scores2[scores2 < 0.05])

library(pls)
pls.dv <- readRDS("models/pls_dv.Rds")
valid.pls.dv <- predict(pls.dv, valid.dv[, -c(1, hc.dv)])
test.pls.dv <- predict(pls.dv, test.dv[, -c(1, hc.dv)])

pls.pval.dv <- readRDS("models/pls_pval_dv.Rds")
valid.pls.pval.dv <- predict(pls.pval.dv, valid.dv[, names(pval.dv)])
test.pls.pval.dv <- predict(pls.pval.dv, test.dv[, names(pval.dv)])

library(glmnet)
glmnet.dv <- readRDS("models/glmnet_dv.Rds")
valid.glmnet.dv <- predict(glmnet.dv, valid.dv[, -c(1, hc.dv)])
test.glmnet.dv <- predict(glmnet.dv, test.dv[, -c(1, hc.dv)])

library(pamr)
nsc.dv <- readRDS("models/nsc_dv.Rds")
valid.nsc.dv <- predict(nsc.dv, valid.dv[, -c(1, hc.dv)])
test.nsc.dv <- predict(nsc.dv, test.dv[, -c(1, hc.dv)])

library(earth)
mars.dv <- readRDS("models/mars_dv.Rds")
valid.mars.dv <- predict(mars.dv, valid.dv[, -1])
test.mars.dv <- predict(mars.dv, test.dv[, -1])

library(kernlab)
svm.dv <- readRDS("models/svm_dv.Rds")
valid.svm.dv <- predict(svm.dv, valid.dv[, -1])
test.svm.dv <- predict(svm.dv, test.dv[, -1])

svm.pval.dv <- readRDS("models/svm_pval_dv.Rds")
valid.svm.pval.dv <- predict(svm.pval.dv, valid.dv[, names(pval.dv)])
test.svm.pval.dv <- predict(svm.pval.dv, test.dv[, names(pval.dv)])

nnet.dv <- readRDS("models/nnet_dv.Rds")
valid.nnet.dv <- predict(nnet.dv, valid.dv[, -1])
test.nnet.dv <- predict(nnet.dv, test.dv[, -1])

library(randomForest)
rf.set <- readRDS("models/rf_set.Rds")
valid.rf.set <- predict(rf.set, valid.set[, -1])
test.rf.set <- predict(rf.set, test.set[, -1])

rf.dv <- readRDS("models/rf_dv.Rds")
valid.rf.dv <- predict(rf.dv, valid.dv[, -1])
test.rf.dv <- predict(rf.dv, test.dv[, -1])

library(ranger)
rf2.set <- readRDS("models/rf2_set.Rds")
valid.rf2.set <- predict(rf2.set, valid.set[, -1])
test.rf2.set <- predict(rf2.set, test.set[, -1])

rf2.dv <- readRDS("models/rf2_dv.Rds")
valid.rf2.dv <- predict(rf2.dv, valid.dv[, -1])
test.rf2.dv <- predict(rf2.dv, test.dv[, -1])

rngr.pval.dv <- readRDS("models/ranger_pval_dv.Rds")
valid.rngr.pval.dv <- predict(rngr.pval.dv, valid.dv[, names(pval.dv)])
test.rngr.pval.dv <- predict(rngr.pval.dv, test.dv[, names(pval.dv)])

library(C50)
c50.data <- readRDS("models/c50_data.Rds")
valid.c50.data <- predict(c50.data, valid.data[, -1], na.action = na.pass)
test.c50.data <- predict(c50.data, test.data[, -1], na.action = na.pass)

c50.set <- readRDS("models/c50_set.Rds")
valid.c50.set <- predict(c50.set, valid.set[, -1])
test.c50.set <- predict(c50.set, test.set[, -1])

c50.dv <- readRDS("models/c50_dv.Rds")
valid.c50.dv <- predict(c50.dv, valid.dv[, -1])
test.c50.dv <- predict(c50.dv, test.dv[, -1])

c50.pval.dv <- readRDS("models/c50_pval_dv.Rds")
valid.pval.c50.dv <- predict(c50.pval.dv, valid.dv[, names(pval.dv)])
test.pval.c50.dv <- predict(c50.pval.dv, test.dv[, names(pval.dv)])

library(xgboost)
xgb.dv <- readRDS("models/xgb_dv.Rds")
valid.xgb.dv <- predict(xgb.dv, valid.dv[, -1])
test.xgb.dv <- predict(xgb.dv, test.dv[, -1])

library(gbm)
gbm.set <- readRDS("models/gbm_set.Rds")
valid.gbm.set <- predict(gbm.set, valid.set[, -1])
test.gbm.set <- predict(gbm.set, test.set[, -1])

gbm.dv <- readRDS("models/gbm_dv.Rds")
valid.gbm.dv <- predict(gbm.dv, valid.dv[, -1])
test.gbm.dv <- predict(gbm.dv, test.dv[, -1])

valid.avg <- data_frame(pls.dv = valid.pls.dv,
                        pls.pval.dv = valid.pls.pval.dv,
                        glmnet.dv = valid.glmnet.dv,
                        nsc.dv = valid.nsc.dv,
                        mars.dv = valid.mars.dv,
                        svm.dv = valid.svm.dv,
                        svm.pval.dv = valid.svm.pval.dv,
                        nnet.dv = valid.nnet.dv,
                        rf.set = valid.rf.set,
                        rf.dv = valid.rf.dv,
                        rf2.set = valid.rf2.set,
                        rf2.dv = valid.rf2.dv,
                        rngr.pval.dv = valid.rngr.pval.dv,
                        c50.data = valid.c50.data,
                        c50.dv = valid.c50.dv,
                        pval.c50.dv = valid.pval.c50.dv,
                        xgb.dv = valid.xgb.dv,
                        gbm.set = valid.gbm.set,
                        gbm.dv = valid.gbm.dv)

mod.avg <- train(x = valid.avg, y = valid.party, method = "rf")
pred.avg.valid <- predict(mod.avg, valid.avg)
confusionMatrix(pred.avg.valid, valid.party)

test.avg <- data_frame(pls.dv = test.pls.dv,
                       pls.pval.dv = test.pls.pval.dv,
                       glmnet.dv = test.glmnet.dv,
                       nsc.dv = test.nsc.dv,
                       mars.dv = test.mars.dv,
                       svm.dv = test.svm.dv,
                       svm.pval.dv = test.svm.pval.dv,
                       nnet.dv = test.nnet.dv,
                       rf.set = test.rf.set,
                       rf.dv = test.rf.dv,
                       rf2.set = test.rf2.set,
                       rf2.dv = test.rf2.dv,
                       rngr.pval.dv = test.rngr.pval.dv,
                       c50.data = test.c50.data,
                       c50.dv = test.c50.dv,
                       pval.c50.dv = test.pval.c50.dv,
                       xgb.dv = test.xgb.dv,
                       gbm.set = test.gbm.set,
                       gbm.dv = test.gbm.dv)

pred.avg.test <- predict(mod.avg, test.avg)


# valid.vote <- valid.avg == "Republican"
valid.vote <- valid.avg %>%
    mutate_each(funs(ifelse(. == "Republican", TRUE, FALSE)))
valid.vote$vote <- rowSums(valid.vote)
pred.valid.vote <- ifelse(valid.vote$vote >= 10, "Republican", "Democrat")
confusionMatrix(pred.valid.vote, valid.party)

valid.vote$pred <- factor(pred.valid.vote)
valid.vote$party <- valid.party

vote.wrong <- filter(valid.vote, pred != party) %>%
    mutate_each(funs(ifelse(. == TRUE, "Republican", "Democrat")), -(vote:party)) %>%
    mutate_each(funs(factor(., levels = c("Democrat", "Republican"))), -(vote:party)) %>%
    mutate_each(funs(. == party), -(vote:party)) %>%
    select(-(vote:party)) %>%
    summarize_each(funs(sum(.) / 397))

vote.correct <- filter(valid.vote, pred == party) %>%
    mutate_each(funs(ifelse(. == TRUE, "Republican", "Democrat")), -(vote:party)) %>%
    mutate_each(funs(factor(., levels = c("Democrat", "Republican"))), -(vote:party)) %>%
    mutate_each(funs(. == party), -(vote:party)) %>%
    select(-(vote:party)) %>%
    summarize_each(funs(sum(.) / 716))

test.vote <- test.avg %>%
    mutate_each(funs(ifelse(. == "Republican", TRUE, FALSE)))
test.vote$vote <- rowSums(test.vote)
pred.test.vote <- ifelse(test.vote$vote >= 10, "Republican", "Democrat")

test.submit <- data_frame(USER_ID = testing$USER_ID, Predictions = pred.test.vote)

mod <- str_replace_all(as.character(Sys.time()), "-|:| ", "")
write_csv(test.submit, paste0("submission_", mod, ".csv"))

