
library(caret)

my_mod <- getModelInfo("C5.0", regex = FALSE)[[1]]
names(my_mod)

my_mod$parameters <- data.frame(
    parameter = c("trials", "model", "winnow", "fuzzy", "cutoff"),
    class = c("numeric", "character", "logical", "logical", "numeric"),
    label = c("# Boosting Iterations", "Model Type", "Winnow", "Fuzzy Thresholding", "Probability Cutoff"))

my_mod$grid <- function(x, y, len = NULL){
    # values of "trials" to try
    c5seq <- if(len==1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
    # values of "cutoff" to try
    cutoff_seq <- seq(.01, .99, length=len)

    # produce the grid
    expand.grid(trials = c5seq,
                model = c("tree", "rules"),
                winnow = c(TRUE, FALSE),
                fuzzy = c(TRUE, FALSE),
                cutoff = cutoff_seq)
}

my_mod$fit <- function(x, y, wts, param, lev, last, classProbs, ...){

    # throw an error if there are more than 2 classes in the target variable
    if(length(levels(y)) != 2){
        stop("This only works for 2-class problems!")
    }

    # grab any arguments passed from train through ...
    theDots <- list(...)

    # if one of the arguments is a C5.0Control object,
    if(any(names(theDots) == "control")){
        # change its value of winnow to match the value given in param
        theDots$control$winnow <- param$winnow
        # change its value of fuzzyThreshold to match the value given in param
        theDots$control$fuzzyThreshold <- param$fuzzy
    }else{
        # if a C5.0Control object is not given, create one
        theDots$control <- C5.0Control(winnow = param$winnow,
                                       fuzzyThreshold = param$fuzzy)
    }

    # put all of the model fitting arguments into one list
    argList <- list(x = x, y = y, weights = wts, trials = param$trials,
                    rules = param$model == "rules")
    # append anything left in ...
    argList <- c(argList, theDots)

    # fit a C5.0 model using the argumets in argList
    do.call("C5.0.default", argList)
}

my_mod$loop <- function(grid){

    # for each model/winnow/fuzzy combination, choose the model with
    # the largest trials and cutoff to be the "main" model
    loop <- ddply(grid, c("model", "winnow", "fuzzy"),
                  function(x) c(trials = max(x$trials), cutoff = max(x$cutoff)))

    # create a list with one element for each "main" model
    submodels <- vector(mode = "list", length = nrow(loop))

    # for each "main" model, make a grid of all sub-models
    for(i in 1:nrow(loop)){
        index <- which(grid$model == loop$model[i] &
                           grid$winnow == loop$winnow[i] &
                           grid$fuzzy == loop$fuzzy[i] &
                           (grid$trials < loop$trials[i] | grid$cutoff < loop$cutoff[i]))

        submodels[[i]] <- grid[index, c('trials', 'cutoff')]
    }

    # return the main models and the sub-models separately
    list(loop = loop, submodels = submodels)
}

my_mod$predict <- function(modelFit, newdata, submodels = NULL){

    # generate probability of membership in first class
    class1Prob <- predict(modelFit, newdata, type='prob')[, modelFit$obsLevels[1]]

    # now assign observations to a class using the cutoff value provided
    out <- ifelse(class1Prob >= modelFit$tuneValue$cutoff,
                  modelFit$obsLevels[1],
                  modelFit$obsLevels[2])

    # now make predictions for any sub-models
    if(!is.null(submodels)){
        tmp <- out # hold this for a second plz
        # create a list with an element for each sub-model plus one for the main model
        out <- vector(mode = "list", length = nrow(submodels) + 1)
        out[[1]] <- tmp # the first element is the main model

        for(j in 1:nrow(submodels)){
            class1Prob_again <- predict(modelFit, newdata, type='prob', trial=submodels$trials[j])[, modelFit$obsLevels[1]]
            out[[j+1]] <- ifelse(class1Prob_again >= submodels$cutoff[j],
                                 modelFit$obsLevels[1],
                                 modelFit$obsLevels[2])
        }
    }
    out
}

fourStats <- function(data, lev = levels(data$obs), model = NULL){

    out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))

    # The best possible model has sensitivity of 1 and specificity of 1.
    # How far are we from that value?
    coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2, byrow = TRUE)
    colnames(coords) <- c("Spec", "Sens")
    rownames(coords) <- c("Best", "Current")
    c(out, Dist = dist(coords)[1])
}
