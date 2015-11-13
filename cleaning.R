###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           October 26th, 2015                                                              #
# Description:    Data preprocessing: dimensionality reduction.                                   #
###################################################################################################


# IMPORT THE NEEDED LIBRARIES
library(ggplot2)

# CONSTANTS DEFINITIONS
pathname <- "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_without_metadata.txt"
pathname_metadata <- "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_metadata.txt"
nan_limit_genes <- 863            # max number of m.v. for the samples
nan_limit_experiments <- 18190    # max number of m.v. for the features

# Load the datasets in a data frame
dataset <- read.delim(pathname, na.strings="NaN")
dataset_metadata <- read.delim(pathname_metadata, na.strings="NaN", header=FALSE)

# VARIABLES DEFINITIONS
observations <- dim(dataset)[1]   # 29090 samples
features <- dim(dataset)[2]       #  1567 features
nan_distr_genes <- numeric()      # vector that stores the number of m.v. for each gene
nan_distr_genes_ord <- numeric()  # vector (ord ASC) that stores the number of m.v. for each gene 
nan_distr_genes_cleaned <- numeric()
nan_distr_genes_cleaned_ord <- numeric()
nan_distr_genes_cleaned_final <- numeric()
nan_distr_genes_cleaned_final_ord <- numeric()
nan_distr_features <- numeric()   # vector that stores the number of m.v. for each experiment
nan_distr_features_ord <- numeric()
nan_distr_features_after <- numeric()
nan_distr_features_after_ord <- numeric()
nan_distr_features_cleaned <- numeric()
nan_distr_features_cleaned_ord <- numeric()
genes_to_keep <- logical()        # vector that stores the bool result of a condition for each gene
experiments_to_keep <- logical()

###################################################################################################


###################################################################################################
# PLOT #1: Distribution of m.v. for the genes                                                     #
###################################################################################################

# For each sample (row), iterate each feature (col) and count the amount of missing values
for (r in 1:observations) {
  nan_counter_genes <- sum(is.na(dataset[r,]))                      # update a NaN counter
  nan_distr_genes <- c(nan_distr_genes, nan_counter_genes)          # append it into a distr vector
}

# Order (ascending) the distribution vector (for the samples) 
nan_distr_genes_ord <- sort(nan_distr_genes, decreasing = FALSE)

# Prepare the data to plot
samples_mv_plot <- ggplot(
  data.frame(nan_distr_genes_ord), aes(seq_along(nan_distr_genes_ord), nan_distr_genes_ord)
)

# Customize the plot
samples_mv_plot <- samples_mv_plot +
  labs(x="samples (genes)", y="missing values (NaN condition contrasts)", title="Amount of missing values of the dataset") +
  annotate("text", x=21500, y=1300, label="Less informative samples\n(1046 samples, ~3.6%)", colour="#C0392B", fontface="bold", size=5) +
  annotate("text", x=5000, y=640, label="mean: 595 (~37.9%)", colour="#e67e22", fontface="bold", size=4) +
  scale_x_continuous(limits = c(0, observations), breaks=seq(0, observations, by=observations)) +
  scale_y_continuous(limits = c(0, 1600), breaks=seq(0, 1600, by=100)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
samples_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_genes_ord>nan_limit_genes, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_genes_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


###################################################################################################
# PLOT #2: Distribution of m.v. for the genes (without those that are less informative)           #
###################################################################################################

# For each sample (row), iterate each feature (col) and mark FALSE the samples that are less informative
for (r in 1:observations) {
  nan_counter_genes <- sum(is.na(dataset[r,]))
  genes_to_keep <- c(genes_to_keep, ifelse(nan_counter_genes<=nan_limit_genes, TRUE, FALSE))
}

# Store the dataset cleaned (i.e. without less informative samples) in a new data frame
dataset_cleaned_rows <- dataset[genes_to_keep,]

# (NEW) VARIABLES DEFINITIONS
observations_cleaned <- dim(dataset_cleaned_rows)[1]           # store the (new) samples dimension

# For each sample (row), iterate each feature (col) and count the amount of missing values
for (r in 1:observations_cleaned) {
  nan_counter_genes_cleaned <- sum(is.na(dataset_cleaned_rows[r,]))
  nan_distr_genes_cleaned <- c(nan_distr_genes_cleaned, nan_counter_genes_cleaned)
}

# Order (ascending) the distribution vector (for the samples) 
nan_distr_genes_cleaned_ord <- sort(nan_distr_genes_cleaned, decreasing = FALSE)

# Prepare the data to plot
samples_cleaned_mv_plot <- ggplot(
  data.frame(nan_distr_genes_cleaned_ord), aes(seq_along(nan_distr_genes_cleaned_ord), nan_distr_genes_cleaned_ord)
)

# Customize the plot
samples_cleaned_mv_plot <- samples_cleaned_mv_plot +
  labs(x="samples (genes)", y="missing values (NaN condition contrasts)", title="Amount of missing values of the dataset") +
  annotate("text", x=5000, y=609, label="mean: 564 (~35.9%)", colour="#e67e22", fontface="bold", size=4) +
  scale_x_continuous(limits = c(0, observations), breaks=seq(0, observations, by=observations)) +
  scale_y_continuous(limits = c(0, 1600), breaks=seq(0, 1600, by=100)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
samples_cleaned_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_genes_cleaned_ord>nan_limit_genes, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_genes_cleaned_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


###################################################################################################
# PLOT #3: Distribution of m.v. for the experiments (before the samples cleaning)                 #
###################################################################################################

# For each feature (col), iterate each sample (row) and count the amount of missing values
for (c in 1:features) {
  nan_counter_features <- sum(is.na(dataset[,c]))
  nan_distr_features <- c(nan_distr_features, nan_counter_features)
}

# Order (ascending) the distribution vector (for the features) 
nan_distr_features_ord <- sort(nan_distr_features, decreasing = FALSE)

# Prepare the data to plot
features_mv_plot <- ggplot(
  data.frame(nan_distr_features_ord), aes(seq_along(nan_distr_features_ord), nan_distr_features_ord)
)

# Customize the plot
features_mv_plot <- features_mv_plot +
  labs(x="features (condition contrasts)", y="missing values (NaN samples)", title="Amount of missing values of the dataset") +
  annotate("text", x=350, y=12000, label="mean: 11037 (~37.9%)", colour="#e67e22", fontface="bold", size=4) +
  scale_y_continuous(limits = c(0, 30000), breaks=seq(0, 30000, by=2000)) +
  scale_x_continuous(limits = c(0, 1600), breaks=seq(0, features, by=features)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
features_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_features_ord<0, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_features_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


###################################################################################################
# PLOT #4: Distribution of m.v. for the experiments (after the samples cleaning)                  #
###################################################################################################

# For each feature (col), iterate each sample (row) and count the amount of missing values
for (c in 1:features) {
  nan_counter_features_after <- sum(is.na(dataset_cleaned_rows[,c]))
  nan_distr_features_after <- c(nan_distr_features_after, nan_counter_features_after)
}

# Order (ascending) the distribution vector (for the features) 
nan_distr_features_after_ord <- sort(nan_distr_features_after, decreasing = FALSE)

# Prepare the data to plot
features_mv_after_plot <- ggplot(
  data.frame(nan_distr_features_after_ord), aes(seq_along(nan_distr_features_after_ord), nan_distr_features_after_ord)
)

# Customize the plot
features_mv_after_plot <- features_mv_after_plot +
  labs(x="features (condition contrasts)", y="missing values (NaN samples)", title="Amount of missing values of the dataset") +
  annotate("text", x=1150, y=26000, label="Less informative features\n(197 features, ~12.6%)", colour="#C0392B", fontface="bold", size=5) +
  annotate("text", x=350, y=11052, label="mean: 10091 (~34.7%)", colour="#e67e22", fontface="bold", size=4) +
  scale_y_continuous(limits = c(0, 30000), breaks=seq(0, 30000, by=2000)) +
  scale_x_continuous(limits = c(0, 1600), breaks=seq(0, features, by=features)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
features_mv_after_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_features_after_ord>18190, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_features_after_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


###################################################################################################
# PLOT #5: Distribution of m.v. for the experiments (without those that are less informative)     #
###################################################################################################

# For each feature (col), iterate each sample (row) and mark FALSE the features that are less informative
for (c in 1:features) {
  nan_counter_experiments <- sum(is.na(dataset_cleaned_rows[,c]))
  experiments_to_keep <- c(experiments_to_keep, ifelse(nan_counter_experiments<=nan_limit_experiments, TRUE, FALSE))
}

# Store the dataset cleaned (i.e. without less informative samples and features too) in a new data frame
dataset_cleaned_rows_cols <- dataset_cleaned_rows[,experiments_to_keep]
dataset_metadata_cleaned <- dataset_metadata[,experiments_to_keep]

# (NEW) VARIABLES DEFINITIONS
features_cleaned <- dim(dataset_cleaned_rows_cols)[2]           # store the (new) features dimension

# For each feature (col), iterate each sample (row) and count the amount of missing values
for (c in 1:features_cleaned) {
  nan_counter_features_cleaned <- sum(is.na(dataset_cleaned_rows_cols[,c]))
  nan_distr_features_cleaned <- c(nan_distr_features_cleaned, nan_counter_features_cleaned)
}

# Order (ascending) the distribution vector (for the features) 
nan_distr_features_cleaned_ord <- sort(nan_distr_features_cleaned, decreasing = FALSE)

# Prepare the data to plot
features_cleaned_mv_plot <- ggplot(
  data.frame(nan_distr_features_cleaned_ord), aes(seq_along(nan_distr_features_cleaned_ord), nan_distr_features_cleaned_ord)
)

# Customize the plot
features_cleaned_mv_plot <- features_cleaned_mv_plot +
  labs(x="features (condition contrasts)", y="missing values (NaN samples)", title="Amount of missing values of the dataset") +
  annotate("text", x=350, y=9345, label="mean: 8384 (~28.8%)", colour="#e67e22", fontface="bold", size=4) +
  scale_y_continuous(limits = c(0, 30000), breaks=seq(0, 30000, by=2000)) +
  scale_x_continuous(limits = c(0, 1600), breaks=seq(0, features, by=features)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
features_cleaned_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_features_cleaned_ord>18190, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_features_cleaned_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


###################################################################################################
# PLOT #6: OVERALL distribution of m.v. for the samples (without less informative sampl/feat)     #
###################################################################################################

# For each sample (row), iterate each feature (col) and count the amount of missing values
for (r in 1:observations_cleaned) {
  nan_counter_genes_cleaned_final <- sum(is.na(dataset_cleaned_rows_cols[r,]))
  nan_distr_genes_cleaned_final <- c(nan_distr_genes_cleaned_final, nan_counter_genes_cleaned_final)
}

# Order (ascending) the distribution vector (for the samples) 
nan_distr_genes_cleaned_final_ord <- sort(nan_distr_genes_cleaned_final, decreasing = FALSE)

# Prepare the data to plot
samples_cleaned_final_mv_plot <- ggplot(
  data.frame(nan_distr_genes_cleaned_final_ord), aes(seq_along(nan_distr_genes_cleaned_final_ord), nan_distr_genes_cleaned_final_ord)
)

# Customize the plot
samples_cleaned_final_mv_plot <- samples_cleaned_final_mv_plot +
  labs(x="samples (genes)", y="missing values (NaN condition contrasts)", title="Amount of missing values of the dataset") +
  annotate("text", x=5000, y=455, label="mean: 410 (~26.1%)", colour="#e67e22", fontface="bold", size=4) +
  scale_x_continuous(limits = c(0, observations), breaks=seq(0, observations, by=observations)) +
  scale_y_continuous(limits = c(0, 1600), breaks=seq(0, 1600, by=100)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
samples_cleaned_final_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_genes_cleaned_ord>nan_limit_genes, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_genes_cleaned_final_ord)), colour="#e67e22", linetype="dashed")

###################################################################################################


# Create the cleaned dataset (without metadata) in a new file
write.table(dataset_cleaned_rows_cols, "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_without_metadata_cleaned.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN")

# Create the cleaned dataset (metadata) in a new file
write.table(dataset_metadata_cleaned, "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_metadata_cleaned.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN")

# Create the whole cleaned dataset in a new file
write.table(dataset_metadata_cleaned, "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_cleaned.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN")
write.table(dataset_cleaned_rows_cols, "~/Scrivania/BIO_DATA/PROJECT/data/vitis_vinifera_cleaned.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN", append=TRUE)
