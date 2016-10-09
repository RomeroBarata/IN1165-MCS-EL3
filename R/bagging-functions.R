#' Generate bootstrap samples.
#' 
#' \code{generateBootstrapSamples} returns a list containing the indices of 
#' bootstrap samples.
#' 
#' Given a number of examples \code{m} and a number of bootstrap samples 
#' \code{b}, the function returns a list of \code{b} elements where each of 
#' them is a vector of length \code{m} containing the examples' indices.
#' 
#' @param m Number of examples in the data set.
#' @param b Number of bootstrap samples.
#' @return A list with \code{b} elements, each containing the indices of a 
#'  bootstrap sample.

generateBootstrapSamples <- function(m, b){
  replicate(b, sample(m, replace = TRUE), simplify = FALSE)
}

#' Generate an ensemble of trees through the bagging algorithm.
#' 
#' \code{bagging} generates a tree ensemble using the bagging strategy, where 
#' each tree in the ensemble is trained with a boostrap sample from the 
#' original data set.
#' 
#' The base decision tree is the one available in the \code{rpart} package.
#' 
#' @param data A data frame containing the predictors and the outcome.
#' @param L Number of trees in the ensemble.
#' @param cores Number of cores for parallel processing. If running on Windows 
#'  \code{cores} must be left as 1.
#' @return A list containing the trained trees.

bagging <- function(data, L = 100, ds_rule = NULL, cores = 1){
  # In order to use the formula interface of the rpart function
  names(data)[ncol(data)] <- "Class"
  
  bootstrap_samples <- generateBootstrapSamples(nrow(data), L)
  
  decisionTree <- function(bootstrap_sample){
    rpart::rpart(Class ~ ., data = data, subset = bootstrap_sample,
                 method = "class", parms = list(split = "information"))
  }
  bagging_models <- parallel::mclapply(bootstrap_samples,
                                       decisionTree, mc.cores = cores)
  names(bagging_models) <- paste("Tree", 1:L, sep = "-")
  
  if (is.null(ds_rule)) class <- "bagging"
  else if (ds_rule == "ola") class <- "bagging_ola"
  else if (ds_rule == "lca") class <- "bagging_lca"
  else if (ds_rule == "dsknn") class <- "bagging_dsknn"
  
  structure(bagging_models, class = class)
}

predict.bagging <- function(object, newdata, ...){
  predictions <- lapply(object, predict, newdata, type = "class")
  predictions <- matrix(unlist(predictions, use.names = FALSE), 
                        ncol = length(object))
  colnames(predictions) <- paste("Tree", 1:length(object), sep = "-")
  predictions
}