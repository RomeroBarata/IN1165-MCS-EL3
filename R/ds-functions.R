predict.bagging_ola <- function(object, newdata, valdata, ...){
  names(valdata)[ncol(valdata)] <- "Class"
  val_preds <- predict.bagging(object, valdata[, -ncol(valdata)])
  
  knn_idx <- kknn::kknn(Class ~ ., 
                        train = valdata, 
                        test = newdata, 
                        k = 5, 
                        kernel = "rectangular")$C
  
  final_preds <- vector("logical", length = nrow(newdata))
  for (i in seq_len(nrow(newdata))){
    nns_idx <- knn_idx[i, ]
    nns_preds <- val_preds[nns_idx, ]
    nns_class <- unlist(valdata[nns_idx, "Class"], use.names = FALSE)
    overall_acc <- colMeans(nns_preds == nns_class)
    best_classifier <- which.max(overall_acc)
    final_preds[i] <- predict(object[[best_classifier]], newdata[i, ], 
                             type = "class")
  }
  
  levels(unlist(valdata[, ncol(valdata)], use.names = FALSE))[final_preds]
}

predict.bagging_lca <- function(object, newdata, valdata, ...){
  names(valdata)[ncol(valdata)] <- "Class"
  val_preds <- predict.bagging(object, valdata[, -ncol(valdata)])
  test_preds <- predict.bagging(object, newdata)
  
  knn_idx <- kknn::kknn(Class ~ ., 
                        train = valdata, 
                        test = newdata, 
                        k = 5, 
                        kernel = "rectangular")$C
  
  final_preds <- vector("logical", length = nrow(newdata))
  for (i in seq_len(nrow(newdata))){
    nns_idx <- knn_idx[i, ]
    nns_preds <- val_preds[nns_idx, ]
    nns_class <- unlist(valdata[nns_idx, "Class"], use.names = FALSE)
    
    f <- function(j){
      current_pred <- test_preds[i, j]
      same_class <- current_pred == nns_class
      if (!any(same_class)) return(0)
      mean(nns_preds[same_class, j] == nns_class[same_class])
    }
    local_acc <- vapply(seq_len(ncol(test_preds)), f, numeric(1))
    best_classifier <- which.max(local_acc)
    final_preds[i] <- predict(object[[best_classifier]], newdata[i, ], 
                              type = "class")
  }
  
  levels(unlist(valdata[, ncol(valdata)], use.names = FALSE))[final_preds]
}

predict.bagging_dsknn <- function(object, newdata, valdata, ...){
  names(valdata)[ncol(valdata)] <- "Class"
  val_preds <- predict.bagging(object, valdata[, -ncol(valdata)])
  test_preds <- predict.bagging(object, newdata)
  
  knn_idx <- kknn::kknn(Class ~ ., 
                        train = valdata, 
                        test = newdata, 
                        k = 5, 
                        kernel = "rectangular")$C
  
  final_preds <- vector("logical", length = nrow(newdata))
  for (i in seq_len(nrow(newdata))){
    nns_idx <- knn_idx[i, ]
    nns_preds <- val_preds[nns_idx, ]
    nns_class <- unlist(valdata[nns_idx, "Class"], use.names = FALSE)
    
    f <- function(j){
      current_pred <- test_preds[i, j]
      same_class <- current_pred == nns_class
      if (!any(same_class)) return(0)
      mean(nns_preds[same_class, j] == nns_class[same_class])
    }
    local_acc <- vapply(seq_len(ncol(test_preds)), f, numeric(1))
    # Highest accuracies
    best_classifiers <- order(local_acc, decreasing = TRUE)[1:5]
    
    #
    diversity_matrix <- diversityMatrix(val_preds[nns_idx, best_classifiers], 
                                        valdata[nns_idx, ncol(valdata)], 
                                        "doubleFault")
    best_idx <- order(colMeans(diversity_matrix))[1:3]
    best_classifiers <- best_classifiers[best_idx]
    
    preds <- predict.bagging(object[best_classifiers], newdata[i, ])
    final_preds[i] <- names(which.max(table(preds)))
  }
  
  final_preds
}