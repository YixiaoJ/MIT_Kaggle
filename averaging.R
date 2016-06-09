# averaging

library(readr)

library(pls)
pls.dv <- readRDS("models/pls_dv.Rds")
test.pls.dv <- predict(pls.dv, test.dv[, -c(1, hc.dv)])

library(glmnet)
glmnet.dv <- readRDS("models/glmnet_dv.Rds")
test.glmnet.dv <- predict(glmnet.dv, test.dv[, -c(1, hc.dv)])

library(pamr)
nsc.dv <- readRDS("models/nsc_dv.Rds")
test.nsc.dv <- predict(nsc.dv, test.dv[, -c(1, hc.dv)])

library(earth)
mars.dv <- readRDS("models/mars_dv.Rds")

library(kernlab)
svm.dv <- readRDS("models/svm_dv.Rds")

nnet.dv <- readRDS("models/nnet_dv.Rds")

library(randomForest)
rf.set <- readRDS("models/rf_set.Rds")
rf.dv <- readRDS("models/rf_dv.Rds")

library(ranger)
rf2.set <- readRDS("models/rf2_set.Rds")
rf2.dv <- readRDS("models/rf2_dv.Rds")

library(C50)
c50.data <- readRDS("models/c50_data.Rds")
c50.set <- readRDS("models/c50_set.Rds")
c50.dv <- readRDS("models/c50_dv.Rds")

library(xgboost)
xgb.dv <- readRDS("models/xgb_dv.Rds")

library(gbm)
gbm.set <- readRDS("models/gbm_set.Rds")
gbm.dv <- readRDS("models/gbm_dv.Rds")

