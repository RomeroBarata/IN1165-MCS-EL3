# IN1165 MCS EL3
Third exercise list for **IN1165 Multiple Classifier Systems**.

## How to run it?
Running the script `main.R` should do all the work. It can be done from the `R` console with the command `source("main.R")` (this directory should be your working directory). Script configuration is also in `main.R`. You can change the cross-validation parameters and the number of cores for parallel processing. If you are a Windows user please change the number of cores to 1. The final results will be stored in the variables `mean_matrix` (table, Q1), `sd_matrix` (table, Q1), and `diversity_matrix` (table, Q3).

## Organisation
The data sets utilised during the experimentation are in the `data` folder. All the implemented functions are logically organised into scripts inside the `R` folder. A report containing my answers for the third exercise list is in the file `report.html`.
