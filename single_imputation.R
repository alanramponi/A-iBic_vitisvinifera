###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           November 13th, 2015                                                             #
# Description:    Data preprocessing: single imputation.                                          #
###################################################################################################


# IMPORT THE NEEDED LIBRARIES
library("mice")

# CONSTANTS DEFINITIONS
pathname <- "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/testing/vitis_vinifera_cleaned_nan_filled.txt"

# Load the datasets in a data frame
dataset <- read.delim(pathname, na.strings="NaN", skip=6)
dataset_metadata <- read.delim(pathname, na.strings="NaN", nrows=7, header=FALSE)

# Create a new transposed data frame from the original one, removing the header lines
dataset_t <- t(dataset[,4:ncol(dataset)])

# Create a new file for the mean-imputed dataset and write the header in it
write.table(dataset_metadata, "~/Scrivania/BIO_DATA/PROJECT/data/imputed/vitis_vinifera_si_mean.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="")

split_len <- 200                                # number of cols splitting
splits <- ncol(dataset_t) %/% split_len         # only the integer + the remaining
last_split_len <- ncol(dataset_t) %% split_len  # number of cols of the last split
end <- 0                                        # the ending index of the current split
start <- end+1                                  # the starting index of the current split

# Impute the NaN values for each split
for(i in 1:splits) {
  end <- split_len*i                            # update the end index
  
  print(paste0("Mean imputation of split ", i, "..."))
  
  mean_imputation <- mice(dataset_t[,start:end], method="mean", m=1, maxit=1)
  dataset_imputed <- complete(mean_imputation)
  dataset_imputed <- t(dataset_imputed)
  dataset_imputed <- data.frame(Geneid.Contrast_id = dataset[start:end,3], dataset_imputed)
  dataset_imputed <- data.frame(Gene.name = dataset[start:end,2], dataset_imputed)
  dataset_imputed <- data.frame(LocusTag = dataset[start:end,1], dataset_imputed)
  
  # append to a file
  write.table(dataset_imputed, "~/Scrivania/BIO_DATA/PROJECT/data/imputed/vitis_vinifera_si_mean.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN", append=TRUE)
  
  cat("Results successful appended to the file.\n\n")
  
  start <- end+1                                # update the start index
  
  # delete temporary structures
  rm(mean_imputation)
  rm(dataset_imputed)
}

# Impute the NaN values for the last split
if (last_split_len != 0) {
  end <- end + last_split_len                   # update the end index
  
  print("Mean imputation of the last split")
  
  mean_imputation <- mice(dataset_t[,start:end], method="mean", m=1, maxit=1)
  dataset_imputed <- complete(mean_imputation)
  dataset_imputed <- t(dataset_imputed)
  dataset_imputed <- data.frame(Geneid.Contrast_id = dataset[start:end,3], dataset_imputed)
  dataset_imputed <- data.frame(Gene.name = dataset[start:end,2], dataset_imputed)
  dataset_imputed <- data.frame(LocusTag = dataset[start:end,1], dataset_imputed)
  
  # append to a file
  write.table(dataset_imputed, "~/Scrivania/BIO_DATA/PROJECT/data/imputed/vitis_vinifera_si_mean.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN", append=TRUE)
  
  cat("Results successful appended to the file.\n\n")
  
  # delete temporary structures
  rm(mean_imputation)
  rm(dataset_imputed)
}

# Plot the density of a given variable
#densityplot(mean_imputation, ~V1|.imp)