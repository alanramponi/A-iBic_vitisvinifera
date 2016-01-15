###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           January 8th, 2015                                                               #
# Description:    Data preprocessing: biclustering substep.                                       #
###################################################################################################


# IMPORT THE NEEDED LIBRARIES
library(biclust)

# VARIABLES DEFINITIONS
min_value <- min(dataset, na.rm=TRUE)   # -12.747
max_value <- max(dataset, na.rm=TRUE)   # 10.208
min_range <- -13.00
max_range <- 11.00
nan_count <- sum(is.na(dataset))        # 11485632

data_validation <- dataset

# Generate a vector of random number in the defined range and fill the matrix of the dataset
random_number <- runif(nan_count, min_range, max_range)
data_validation[is.na(data_validation)] <- random_number
dataset_mat <- data.matrix(data_validation)

# Discretize the dataframe in order to choose in a faster way where to split
dataset_discretized <- discretize(dataset_mat, nof=10)

# Perform the xMotifs biclustering algorithm
dataset_biclustered <- biclust(dataset_discretized, method=BCXmotifs(), ns=150, nd=50, sd=5, alpha=0.00001, number=20)

# Store the indexes of the original matrix for each bicluster
indexes <- biclusternumber(dataset_biclustered)

# Save them into vectors
rows_bic1 <-indexes$Bicluster2$Rows
cols_bic1 <- indexes$Bicluster2$Cols
rows_bic2 <-indexes$Bicluster8$Rows
cols_bic2 <- indexes$Bicluster8$Cols
rows_bic3 <-indexes$Bicluster6$Rows
cols_bic3 <- indexes$Bicluster6$Cols
rows_bic4 <-indexes$Bicluster7$Rows
cols_bic4 <- indexes$Bicluster7$Cols

# Create a matrix for storing the values from the indexes of the rows and cols
dataset_clust1 <- matrix(NA, nrow=length(rows_bic1), ncol=length(cols_bic1))
dataset_clust2 <- matrix(NA, nrow=length(rows_bic2), ncol=length(cols_bic2))
dataset_clust3 <- matrix(NA, nrow=length(rows_bic3), ncol=length(cols_bic3))
dataset_clust4 <- matrix(NA, nrow=length(rows_bic4), ncol=length(cols_bic4))

# Create a matrix for storing the corresponding flags
flags_clust1 <- matrix(NA, nrow=length(rows_bic1), ncol=length(cols_bic1))
flags_clust2 <- matrix(NA, nrow=length(rows_bic2), ncol=length(cols_bic2))
flags_clust3 <- matrix(NA, nrow=length(rows_bic3), ncol=length(cols_bic3))
flags_clust4 <- matrix(NA, nrow=length(rows_bic4), ncol=length(cols_bic4))

# Populate the first bicluster with the real value of the original dataset
for (i in 1:nrow(dataset_clust1)) {
  for (j in 1:ncol(dataset_clust1)) {
    dataset_clust1[i,j] <- dataset[rows_bic1[i], cols_bic1[j]]
  }
}
# Populate the flag matrix of the first bicluster
for (i in 1:nrow(flags_clust1)) {
  for (j in 1:ncol(flags_clust1)) {
    flags_clust1[i,j] <- data_flags[rows_bic1[i], cols_bic1[j]]
  }
}

# Populate the second bicluster with the real value of the original dataset
for (i in 1:nrow(dataset_clust2)) {
  for (j in 1:ncol(dataset_clust2)) {
    dataset_clust2[i,j] <- dataset[rows_bic2[i], cols_bic2[j]]
  }
}
# Populate the flag matrix of the second bicluster
for (i in 1:nrow(flags_clust2)) {
  for (j in 1:ncol(flags_clust2)) {
    flags_clust2[i,j] <- data_flags[rows_bic2[i], cols_bic2[j]]
  }
}

# Populate the third bicluster with the real value of the original dataset
for (i in 1:nrow(dataset_clust3)) {
  for (j in 1:ncol(dataset_clust3)) {
    dataset_clust3[i,j] <- dataset[rows_bic3[i], cols_bic3[j]]
  }
}
# Populate the flag matrix of the third bicluster
for (i in 1:nrow(flags_clust3)) {
  for (j in 1:ncol(flags_clust3)) {
    flags_clust3[i,j] <- data_flags[rows_bic3[i], cols_bic3[j]]
  }
}

# Populate the fourth bicluster with the real value of the original dataset
for (i in 1:nrow(dataset_clust4)) {
  for (j in 1:ncol(dataset_clust4)) {
    dataset_clust4[i,j] <- dataset[rows_bic4[i], cols_bic4[j]]
  }
}
# Populate the flag matrix of the fourth bicluster
for (i in 1:nrow(flags_clust4)) {
  for (j in 1:ncol(flags_clust4)) {
    flags_clust4[i,j] <- data_flags[rows_bic4[i], cols_bic4[j]]
  }
}