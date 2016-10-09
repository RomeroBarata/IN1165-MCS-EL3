oracleMatrix <- function(predictions, ground_truth){
  d1 <- predictions[, 1] == ground_truth
  d2 <- predictions[, 2] == ground_truth
  
  a <- sum(d1 & d2)
  b <- sum(d1 & !d2)
  c <- sum(!d1 & d2)
  d <- sum(!(d1 | d2))
  
  matrix(c(a, b, c, d), byrow = TRUE, nrow = 2, ncol = 2) / nrow(predictions)
}

correlation <- function(a, b, c, d){
  (a * d - b * c) / sqrt((a + b) * (c + d) * (a + c) * (b + d))
}

doubleFaultMeasure <- function(d){
  d
}

pairwiseMeasures <- function(oracle_matrix){
  a <- oracle_matrix[1, 1]
  b <- oracle_matrix[1, 2]
  c <- oracle_matrix[2, 1]
  d <- oracle_matrix[2, 2]
  
  c(correlation = correlation(a, b, c, d), doubleFault = doubleFaultMeasure(d))
}

diversityMatrix <- function(predictions, ground_truth, measure){
  num_classifiers <- ncol(predictions)
  pairwise_combs <- combn(num_classifiers, 2, simplify = FALSE)
  f <- function(cols){
    oracle_matrix <- oracleMatrix(predictions[, cols], ground_truth)
    pairwiseMeasures(oracle_matrix)[measure]
  }
  pairwise_measures <- vapply(pairwise_combs, f, numeric(1))
  
  diversity_matrix <- matrix(0, nrow = num_classifiers, ncol = num_classifiers)
  for (i in seq_along(pairwise_measures)){
    idx <- matrix(pairwise_combs[[i]], nrow = 1)
    diversity_matrix[idx] <- pairwise_measures[[i]]
  }
  diversity_matrix + t(diversity_matrix)
}