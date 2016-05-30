# 2-preprocess

source("1-data_sets.R")

library(tibble)

# dummy vars -------------------------------------------
tmp <- train.data %>%
    mutate(Income = factor(Income, ordered = FALSE),
           EducationLevel = factor(EducationLevel, ordered = FALSE))

dv <- dummyVars(~ ., data = tmp[, -7])
train.dv <- predict(dv, newdata = tmp[, -7]) %>% as_data_frame()

rm(tmp)
