# 1-data_sets

library(readr)
library(plyr)
library(dplyr)
library(tibble)
library(stringr)
library(caret)
library(purrr)
library(mice)

# use saved imputed data set if it exists, else do imputing (slow)
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

# set ordered factor levels
income <- c("under \\$25,000" = "u25",
            "\\$25,001 - \\$50,000" = "25_49",
            "\\$50,000 - \\$74,999" = "50_74",
            "\\$75,000 - \\$100,000" = "75_99",
            "\\$100,001 - \\$150,000" = "100_150",
            "over \\$150,000" = "o150")

household <- c("Married \\(w/kids\\)" = "married_kids",
               "Domestic Partners \\(w/kids\\)" = "partners_kids",
               "Single \\(no kids\\)" = "single",
               "Married \\(no kids\\)" = "married",
               "Domestic Partners \\(no kids\\)" = "partners",
               "Single \\(w/kids\\)" = "single_kids")

education <- c("Current K-12" = "in_school",
               "High School Diploma" = "hs_diploma",
               "Current Undergraduate" = "in_college",
               "Associate's Degree" = "aa",
               "Bachelor's Degree" = "bs",
               "Master's Degree" = "ms",
               "Doctoral Degree" = "dr")

# convert levels to valid names
# keep ordered levels in train.set; unordered for dummyVars

# get training and testing data
training <- read_csv("train2016.csv") %>%
    mutate(Income = str_replace_all(Income, income),
           HouseholdStatus = str_replace_all(HouseholdStatus, household),
           EducationLevel = str_replace_all(EducationLevel, education))

testing <- read_csv("test2016.csv") %>%
    mutate(Income = str_replace_all(Income, income),
           HouseholdStatus = str_replace_all(HouseholdStatus, household),
           EducationLevel = str_replace_all(EducationLevel, education))

# train / validate sets --------------------------------

set.seed(123)
inTrain <- createDataPartition(y = training$Party, p = 0.8, list = FALSE)
train.data <- training[inTrain, ]
valid.data <- training[-inTrain, ]

# remove invalid YOB
train.data$YOB[train.data$YOB > 2005 | train.data$YOB < 1910] <- NA
valid.data$YOB[valid.data$YOB > 2005 | valid.data$YOB < 1910] <- NA
testing$YOB[testing$YOB > 2005 | testing$YOB < 1910] <- NA

# feature processing -------------------------------------------

# create an "NA" level for factor vars before converting to dummy vars
train.set <- select(train.data, -Party) %>%
    mutate_each(funs(str_replace_na), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

# make factors for original data set (keeps NA's)
train.data <- train.data %>%
    mutate(Income = ordered(Income, levels = income),
           EducationLevel = ordered(EducationLevel, levels = education)) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

# make outcome vectors
train.party <- train.data$Party

# impute missing data (should only be YOB variable)
train.mice <- get_impute("train_mice.Rds", train.set)
train.imp <- complete(train.mice)

# replace the YOB variable with imputed values (done this way to avoid
# warnings about dropped contrasts)
train.set$YOB <- train.imp$YOB

# convert factors to numeric
train.data.n <- dmap_if(train.data, is.factor, as.numeric)
train.set.n <- dmap_if(train.set, is.factor, as.numeric)

# make dummy variables for factor data
dv <- dummyVars(~ ., data = train.set)
train.dv <- predict(dv, newdata = train.set) %>% as_data_frame()

train.set <- train.set %>%
    mutate(Income = ordered(Income, levels = c("NA", income)),
           EducationLevel = ordered(EducationLevel, levels = c("NA", education)))

# repeat for validation set
valid.set <- select(valid.data, -Party) %>%
    mutate_each(funs(str_replace_na), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

valid.data <- valid.data %>%
    mutate(Income = ordered(Income, levels = income),
           EducationLevel = ordered(EducationLevel, levels = education)) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

valid.party <- valid.data$Party

valid.mice <- get_impute("valid_mice.Rds", valid.set, train.mice$method)
valid.imp <- complete(valid.mice)

valid.set$YOB <- valid.imp$YOB

valid.data.n <- dmap_if(valid.data, is.factor, as.numeric)
valid.set.n <- dmap_if(valid.set, is.factor, as.numeric)

valid.dv <- predict(dv, newdata = valid.set) %>% as_data_frame()

valid.set <- valid.set %>%
    mutate(Income = ordered(Income, levels = c("NA", income)),
           EducationLevel = ordered(EducationLevel, levels = c("NA", education)))

# repeat for test set
test.set <- testing %>%
    mutate_each(funs(str_replace_na), -USER_ID, -YOB) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

test.data <- testing %>%
    mutate(Income = ordered(Income, levels = income),
           EducationLevel = ordered(EducationLevel, levels = education)) %>%
    mutate_each(funs(as.factor), -USER_ID, -YOB)

test.mice <- get_impute("test_mice.Rds", test.set, train.mice$method)
test.imp <- complete(test.mice)

test.set$YOB <- test.imp$YOB

test.data.n <- dmap_if(test.data, is.factor, as.numeric)
test.set.n <- dmap_if(test.set, is.factor, as.numeric)

test.dv <- predict(dv, newdata = test.set) %>% as_data_frame()

test.set <- test.set %>%
    mutate(Income = ordered(Income, levels = c("NA", income)),
           EducationLevel = ordered(EducationLevel, levels = c("NA", education)))

# high correlation -------------------------------------

hcor.set <- cor(train.set.n, use = "na.or.complete")
hc.set <- findCorrelation(hcor.set)

hcor.dv <- cor(train.dv, use = "na.or.complete")
hc.dv <- findCorrelation(hcor.dv)

# party ------------------------------------------------

# add party back to training data sets for formula use
train.dv$Party <- train.party
train.set$Party <- train.party
train.set.n$Party <- train.party

# new features -----------------------------------------

train.ftr <- train.data %>%
    by_row(function(x) sum(is.na(x)), .to = "num.na", .collate = "cols") %>%
    select(USER_ID, Party, num.na)

ica <- preProcess(train.dv[, -c(1, 331)], c("ica", "nzv"), n.comp = 10)
train.ica <- predict(ica, train.dv)
valid.ica <- predict(ica, valid.dv)
test.ica <- predict(ica, test.dv)

# linear_combo <- findLinearCombos()
# centroids <- classDist(train.data[, 3], train.party)
# dist <- predict(centroids, valid.data[, 2])

# cats = apply(train.data[, -c(1, 2, 7)], 2, function(x) nlevels(as.factor(x)))

# library(FactoMineR)
# mca1 <- MCA(train.data, quanti.sup = c(1, 2), quali.sup = 7)
#
# train.mca <- predict(mca1, valid.data)

library(tibble)
# mca1_vars <- data.frame(mca1$var$coord)
# mca1_obs <- data.frame(mca1$ind$coord)

library(MASS)
mca2 <- mca(train.data[, -c(1, 2, 7)], nf = 10)
train.mca <- predict(mca2, train.data[, -c(1, 2, 7)]) %>% as_data_frame()
valid.mca <- predict(mca2, valid.data[, -c(1, 2, 7)]) %>% as_data_frame()
test.mca <- predict(mca2, test.data[, -c(1, 2)]) %>% as_data_frame()

names(train.mca) <- paste0("mca", names(train.mca))
names(valid.mca) <- paste0("mca", names(valid.mca))
names(test.mca) <- paste0("mca", names(test.mca))

train.mca <- bind_cols(train.data[, c("USER_ID", "Party")], train.mca)
