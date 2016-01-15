###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           January 10th, 2015                                                              #
# Description:    Data preprocessing: imputing substep and model assessment.                      #
###################################################################################################


# IMPORT THE NEEDED LIBRARIES
library("Metrics")
library(imputation)

# Backup the original cluster
cluster_4_test <- dataset_clust4

# Delete test data to predict using the flag matrix as a guide
for (i in 1:nrow(cluster_4_test)) {
  for (j in 1:ncol(cluster_4_test)) {
    if (flags_clust4[i,j] == 2)             # if it is a validation entry
      cluster_4_test[i,j] <- NA             # delete it to predict it
  }
}

cluster_4_test <- t(cluster_4_test)
flags_clust4 <- t(flags_clust4)
dataset_clust4 <- t(dataset_clust4)


###################################################################################################
# MEAN COMPUTATION                                                                                #
###################################################################################################

# Create a list to save the MSE performances
mean_mse <- numeric()

# Compute the MSE of the mean
dataset_imputed <- meanImpute(cluster_4_test)
dataset_imputed <- data.frame(dataset_imputed)
values_exp <- numeric()
values_pred <- numeric()

for (i in 1:nrow(flags_clust4)) {
  for (j in 1:ncol(flags_clust4)) {
    if(flags_clust4[i,j] == 2) {
      if(!is.na(dataset_imputed[i,j])) {
        values_exp <- c(values_exp, dataset_clust4[i,j])
        values_pred <- c(values_pred, dataset_imputed[i,j])
      }
    }
  }
}

# Print the results
print(mse(values_exp, values_pred))

# Store the results
mean_mse <- c(mean_mse, mse(values_exp, values_pred))

print("The best value is:")
print(mean_mse[mean_mse == min(mean_mse)])
print("at index:")
print(which(mean_mse == min(mean_mse)))


###################################################################################################
# SVT, aSVT, WkNN, SVD COMPUTATION                                                                #
###################################################################################################

# Create a list to save the MSE performances with different parameters
svt_mse <- numeric()
# asvt_mse <- numeric()
# knn_mse <- numeric()
# svd_mse <- numeric()

# Compute the MSE of the method each 10 steps (for SVT) and each 1 step (for the others)
for(n in 1:50) {
  # n <- n*10
  
  dataset_imputed <- SVTImpute(cluster_4_test, n, verbose=F)
  # dataset_imputed <- SVTApproxImpute(cluster_4_test, n, verbose=F)
  # dataset_imputed <- kNNImpute(cluster_4_test, n, verbose=F)
  # dataset_imputed <- SVDImpute(cluster_4_test, n, verbose=F)
  
  dataset_imputed <- data.frame(dataset_imputed)
  values_exp <- numeric()
  values_pred <- numeric()
  
  for (i in 1:nrow(flags_clust4)) {
    for (j in 1:ncol(flags_clust4)) {
      if(flags_clust4[i,j] == 2) {
        if(!is.na(dataset_imputed[i,j])) {
          values_exp <- c(values_exp, dataset_clust4[i,j])
          values_pred <- c(values_pred, dataset_imputed[i,j])
        }
      }
    }
  }
  
  # Print the results
  cat("it.", n, "\n")
  print(mse(values_exp, values_pred))
  
  # Store the results
  svt_mse <- c(svt_mse, mse(values_exp, values_pred))
  # asvt_mse <- c(asvt_mse, mse(values_exp, values_pred))
  # knn_mse <- c(knn_mse, mse(values_exp, values_pred))
  # svd_mse <- c(svd_mse, mse(values_exp, values_pred))
  
  n <- n/10
}

print("The best value is:")
print(svt_mse[svt_mse == min(svt_mse)])
# print(asvt_mse[asvt_mse == min(asvt_mse)])
# print(knn_mse[knn_mse == min(knn_mse)])
# print(svd_mse[svd_mse == min(svd_mse)])
print("at index:")
print(which(svt_mse == min(svt_mse)))
# print(which(asvt_mse == min(asvt_mse)))
# print(which(knn_mse == min(knn_mse)))
# print(which(svd_mse == min(svd_mse)))