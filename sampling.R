###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           January 7th, 2016                                                               #
# Description:    Data preprocessing: sampling for evaluation purpose.                            #
###################################################################################################


# CONSTANTS DEFINITIONS
pathname <- "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/vitis_vinifera_cleaned.txt"
pathname_metadata <- "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/vitis_vinifera_metadata_cleaned.txt"
pathname_new <- "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/testing/vitis_vinifera_cleaned_nan_filled.txt"
nan_limit_genes_inf <- 0
nan_limit_genes_sup <- 666
nan_limit_experiments_inf <- 31
nan_limit_experiments_sup <- 18190

# Load the datasets in a data frame
dataset <- read.delim(pathname, na.strings="NaN", skip=6)
dataset_metadata <- read.delim(pathname_metadata, na.strings="NaN", header=FALSE)

# VARIABLES DEFINITIONS
nan_distr_genes <- numeric()            # vector that stores the number of m.v. for each gene
nan_distr_genes_new <- numeric()
nan_distr_experiments <- numeric()      # vector that stores the number of m.v. for each experiment
nan_distr_experiments_new <- numeric()
row_indexes <- numeric()                # vector that stores the row indexes to set to NaN
col_indexes <- numeric()                # vector that stores the col indexes to set to NaN
col_condition <- FALSE                  # flag used to reduce the computational complexity of the nan condition

# Store a vector of number of missing values (for the genes)
for (r in 1:nrow(dataset)) {
  nan_counter_genes <- sum(is.na(dataset[r,]))
  nan_distr_genes <- c(nan_distr_genes, nan_counter_genes)
}

# Store a vector of number of missing values (for the experiments)
for (c in 1:ncol(dataset)) {
  nan_counter_experiments <- sum(is.na(dataset[,c]))
  nan_distr_experiments <- c(nan_distr_experiments, nan_counter_experiments)
}

# Store the row/col indexes of elements that meet the condition
for (g in 1:length(nan_distr_experiments)) {
  if (((nan_distr_experiments[g]) > nan_limit_experiments_inf) & ((nan_distr_experiments[g]) < nan_limit_experiments_sup)) {
    col_condition <- TRUE
  }
  for (e in 1:length(nan_distr_genes)) {
    if (col_condition==TRUE) {
      if (((nan_distr_genes[e]) > nan_limit_genes_inf) && ((nan_distr_genes[e]) < nan_limit_genes_sup)) {
        row_indexes <- c(row_indexes, e)
        col_indexes <- c(col_indexes, g)
      }
    }
  }
  col_condition <- FALSE
}

# Remove the first 3 columns to obtain a fully numeric dataframe
dataset <- dataset[,4:ncol(dataset)]

# Create a flag matrix for storing the flags (0: training, 1: test, 2: validation)
data_flags <- data.frame(matrix(0, ncol=1367, nrow=28044))

# Fill with the validation flag (2) the matrix cells that meet the condition imposed
for (i in 1:length(row_indexes)) {
  data_flags[row_indexes[i],col_indexes[i]] <- 2      # flag them as validation data
}

# Fill with the test flag (1) some matrix cells according to the NaN distribution
for (i in 1000:14500) {
  for (j in 200:700) {
    if (data_flags[i,j] != 2) {
      data_flags[i,j] <- 1                              # flag them as test data
    }
  }
}

# Create the test set to be filled by NaN (i.e. the validation set to set apart)
data_test_nan <- dataset

for (i in 1:nrow(data_flags)) {
  for (j in 1:ncol(data_flags)) {
    if (data_flags[i,j] == 2) {     # check if the cell is a validation cell
      data_test_nan[i,j] <- NA      # if yes, remove it
    }
  }
}

# GENERAL INFORMATION
# data_test_nan   to be used for testing
# dataset         to be used for validation