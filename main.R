## Load packages ---------------------------------------------------------------
if (!require(needs)) install.packages("needs")
needs::needs(readr)

## Constants -------------------------------------------------------------------
DATA_PATH <- "data"
R_PATH <- "R"
CORES <- 1  # For parallel processing
SEED <- 14563  # For a fair comparison between models
FOLDS <- 5
REPEATS <- 2

## Source files ----------------------------------------------------------------
source(file.path(R_PATH, "bagging-functions.R"))
source(file.path(R_PATH, "cv-functions.R"))
source(file.path(R_PATH, "diversity-functions.R"))
source(file.path(R_PATH, "ds-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## Read data into the workspace ------------------------------------------------
glass1 <- read(DATA_PATH, "glass1.csv")
haberman <- read(DATA_PATH, "haberman.csv")
pima <- read(DATA_PATH, "pima.csv")

## Cross-validation process ----------------------------------------------------
f <- function(data, rule){
  cvTrain(data, 
          method = "bagging", 
          method_args = list(L = 15, ds_rule = rule, cores = CORES), 
          folds = FOLDS, repeats = REPEATS, cores = CORES, seed = SEED)
}
data_sets <- list(glass1 = glass1, haberman = haberman, pima = pima)
ds_rules <- list(bagging = NULL, ola = "ola", lca = "lca", dsknn = "dsknn")
results <- parallel::mcMap(f, 
                           data = data_sets, 
                           rule = rep(ds_rules, each = length(data_sets)), 
                           mc.cores = CORES)
names(results) <- paste(names(data_sets), 
                        rep(names(ds_rules), each = length(data_sets)), 
                        sep = "-")

## Assemble the results for Q1 -------------------------------------------------
results <- lapply(results, rbindList)
results_mean_sd <- vapply(results, 
                          function(x) c(Mean = mean(x[, 1]), SD = sd(x[, 1])), 
                          numeric(2))
mean_matrix <- matrix(results_mean_sd[1, ], nrow = 3, ncol = 4, 
                      dimnames = list(names(data_sets), 
                                      c("bagging", ds_rules[-1])))
sd_matrix <- matrix(results_mean_sd[2, ], nrow = 3, ncol = 4, 
                    dimnames = list(names(data_sets), 
                                    c("bagging", ds_rules[-1])))

## Q3 --------------------------------------------------------------------------
reps <- 100
diversity_matrix <- matrix(0, nrow = length(ds_rules), ncol = length(ds_rules))
set.seed(SEED)
for (i in seq_len(reps)){
  partition <- createStratifiedPartition(pima$Class,
                                         folds = c(2, 2, 6))
  testing <- pima[partition == 1, -ncol(pima)]
  testing_class <- unlist(pima[partition == 1, ncol(pima)],
                          use.names = FALSE)
  validation <- pima[partition == 2, ]
  training <- pima[partition == 3, ]
  
  models <- lapply(ds_rules, bagging, data = training, L = 15, cores = CORES)
  preds <- vapply(models[-1], predict, character(nrow(testing)), 
                  newdata = testing, valdata = validation)
  bagging_preds <- predict(models[[1]], testing)
  bagging_preds <- apply(bagging_preds, 1, function(x) names(which.max(table(x))))
  preds <- cbind(bagging = bagging_preds, preds)
  diversity_matrix <- diversity_matrix + 
    diversityMatrix(preds, testing_class, "correlation")
}
diversity_matrix <- diversity_matrix / reps
dimnames(diversity_matrix) <- list(c("bagging", ds_rules[-1]), 
                                   c("bagging", ds_rules[-1]))
