createStratifiedPartition <- function(y, folds = 10){
  if (is.data.frame(y)) y <- unlist(y, use.names = FALSE)
  classes_dist <- table(y)
  
  partition <- vector(mode = "numeric", length = length(y))
  for(i in seq_along(classes_dist)){
    if (length(folds) == 1){
      max_sample <- ceiling(classes_dist[i] / folds) * folds
      folds_idx <- rep_len(1:folds, length.out = max_sample)
    } else{
      offset <- classes_dist[i] %% 10
      max_sample <- classes_dist[i] - offset + 10
      folds_idx <- rep_len(rep.int(seq_along(folds), times = folds),
                           length.out = max_sample)
    }
    class_partition <- sample(folds_idx)[1:classes_dist[i]]
    class_id <- names(classes_dist)[i]
    partition[y == class_id] <- class_partition
  }
  partition
}

cvTrain <- function(data, method, method_args = list(), 
                    folds, repeats, cores, seed = NULL, ...){
  if (!is.null(seed)) set.seed(seed)
  partitions <- replicate(repeats, 
                          createStratifiedPartition(data[[ncol(data)]], folds), 
                          simplify = FALSE)
  
  results <- parallel::mcMap(train, 
                             data = list(data), 
                             method = list(method), 
                             method_args = list(method_args), 
                             partition = partitions, 
                             cores = list(cores), 
                             mc.cores = cores)
}

train <- function(data, method, method_args = list(), partition, cores, ...){
  folds <- length(unique(partition))
  for (i in seq_len(folds)){
    # Test set
    testing_idx <- partition == (((i - 1) %% folds) + 1)
    testing <- data[testing_idx, -ncol(data)]
    y <- unlist(data[testing_idx, ncol(data)], use.names = FALSE)
    
    # Validation
    validation_idx <- partition == ((i %% folds) + 1)
    validation <- data[validation_idx, ]
    
    # Training
    training <- data[!(testing_idx | validation_idx), ]
    model <- do.call(method, c(list(data = training), method_args))
    
    # 
    preds <- predict(model, testing, validation)
    if (class(model) == "bagging")
      preds <- apply(preds, 1, function(x) names(which.max(table(x))))
    
    acc <- mean(preds == y)
    
    # Assemble results
    if (i == 1){
      results <- rbind(c(Accuracy = acc))
    } else{
      results <- rbind(results, c(Accuracy = acc))
    }
  }
  results
}