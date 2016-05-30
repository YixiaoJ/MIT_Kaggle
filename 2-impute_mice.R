# impute_mice

source("1-data_sets.R")

library(mice)

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

# impute factor data
train.mice <- get_impute("train_mice_fact.Rds", train.set)
train.data.m <- complete(train.mice)

valid.mice <- get_impute("valid_mice_fact.Rds", valid.set, train.mice$method)
valid.data.m <- complete(valid.mice)

test.mice <- get_impute("test_mice_fact.Rds", test.set, train.mice$method)
test.data.m <- complete(test.mice)

# impute numeric data
train.mice2 <- get_impute("train_mice_num.Rds", train.set)
train.set.m <- complete(train.mice2)

valid.mice2 <- get_impute("valid_mice_num.Rds", valid.set, train.mice$method)
valid.set.m <- complete(valid.mice2)

test.mice2 <- get_impute("test_mice_num.Rds", test.set, train.mice$method)
test.set.m <- complete(test.mice2)
