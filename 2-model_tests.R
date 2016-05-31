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
