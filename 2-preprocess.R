# 2-preprocess

source("1-data_sets.R")

library(tibble)

# dummy vars -------------------------------------------

# use unordered levels
tmp <- train.data %>%
    mutate(Income = factor(Income, ordered = FALSE),
           EducationLevel = factor(EducationLevel, ordered = FALSE))

dv <- dummyVars(~ ., data = tmp[, -7])
train.dv <- predict(dv, newdata = tmp[, -7]) %>% as_data_frame()
train.dv$Party <- train.party

tmp <- valid.data %>%
    mutate(Income = factor(Income, ordered = FALSE),
           EducationLevel = factor(EducationLevel, ordered = FALSE))

valid.dv <- predict(dv, newdata = tmp) %>% as_data_frame()

rm(tmp)

# near zero variance -----------------------------------

# nz <- nearZeroVar(train.dv)
# nz2 <- nearZeroVar(train.set)
#
# train.nz <- train.set[, -nz]
# valid.nz <- valid.set[, -nz]

# high correlation -------------------------------------
hcor <- cor(train.dv[, -226], use = "na.or.complete")
hc <- findCorrelation(hcor)
train.hc <- train.dv[, -hc]
valid.hc <- valid.dv[, -hc]

# set seeds --------------------------------------------
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

seeds[[51]] <- sample.int(1000, 1)

# train control ----------------------------------------
trCtrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds,
                       classProbs = TRUE, returnResamp = "all")

# GLM --------------------------------------------------
modelGLM <- train(Party ~ ., data = train.dv[, -1], method = "glm", trControl = trCtrl,
                  preProcess = c("nzv", "BoxCox", "knnImpute"))
predGLM <- predict(modelGLM, newdata = valid.dv, na.action = na.pass)
cmGLM <- confusionMatrix(predGLM, valid.party)
cmGLM
modelGLM

modelGLM <- train(Party ~ ., data = train.hc[, -1], method = "glm", trControl = trCtrl,
                  preProcess = c("nzv", "BoxCox", "knnImpute"))
predGLM <- predict(modelGLM, newdata = valid.dv, na.action = na.pass)
cmGLM <- confusionMatrix(predGLM, valid.party)
cmGLM
modelGLM

