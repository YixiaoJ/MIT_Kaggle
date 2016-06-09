# predictions

library(tibble)
model <- readRDS("models/stack_pls_c50_rngr_pval_dv.Rds")
test <- test.dv[, names(pval.dv)]

pred.test <- predict(model, newdata = test, na.action = na.pass)

test.submit <- data_frame(USER_ID = testing$USER_ID, Predictions = pred.test)

mod <- str_replace_all(as.character(Sys.time()), "-|:| ", "")
write_csv(test.submit, paste0("submission_", mod, ".csv"))
