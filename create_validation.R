###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           October 31st, 2015                                                              #
# Description:    Data preprocessing: prepare a validation set by filling NaN.                    #
###################################################################################################


# IMPORT THE NEEDED LIBRARIES
library(ggplot2)

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
        # cat("row", e, ", col", g, ":\t", dataset[e,g], "\n")
      }
    }
  }
  col_condition <- FALSE
}

# Fill with NaN the matrix cells that meet the condition imposed
for (i in 1:length(row_indexes)) {
  dataset[row_indexes[i],col_indexes[i]] <- NaN
}

# Create the NaN filled dataset for validation purposes in a new file
write.table(dataset_metadata, "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/testing/vitis_vinifera_cleaned_nan_filled.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN")
write.table(dataset, "~/Scrivania/BIO_DATA/PROJECT/data/cleaned/testing/vitis_vinifera_cleaned_nan_filled.txt", sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, na="NaN", append=TRUE)

###################################################################################################


# Load the datasets in a data frame
dataset_new <- read.delim(pathname_new, na.strings="NaN", skip=6)

observations <- dim(dataset_new)[1]   # 28044 samples
features <- dim(dataset_new)[2]       #  1370 features

# Store a vector of number of missing values (for the genes)
for (r in 1:nrow(dataset_new)) {
  nan_counter_genes_new <- sum(is.na(dataset_new[r,]))
  nan_distr_genes_new <- c(nan_distr_genes_new, nan_counter_genes_new)
}

# Order (ascending) the distribution vector (for the samples) 
nan_distr_genes_ord <- sort(nan_distr_genes_new, decreasing = FALSE)

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
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_genes_ord>100000, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_genes_ord)), colour="#e67e22", linetype="dashed")


# Store a vector of number of missing values (for the genes)
for (c in 1:ncol(dataset_new)) {
  nan_counter_experiments_new <- sum(is.na(dataset_new[,c]))
  nan_distr_experiments_new <- c(nan_distr_experiments_new, nan_counter_experiments_new)
}

# Order (ascending) the distribution vector (for the samples) 
nan_distr_experiments_ord <- sort(nan_distr_experiments_new, decreasing = FALSE)

# Prepare the data to plot
experiments_mv_plot <- ggplot(
  data.frame(nan_distr_experiments_ord), aes(seq_along(nan_distr_experiments_ord), nan_distr_experiments_ord)
)

# Customize the plot
experiments_mv_plot <- experiments_mv_plot +
  labs(x="features (condition contrasts)", y="missing values (NaN samples)", title="Amount of missing values of the dataset") +
  annotate("text", x=350, y=9345, label="mean: 8384 (~28.8%)", colour="#e67e22", fontface="bold", size=4) +
  scale_y_continuous(limits = c(0, 30000), breaks=seq(0, 30000, by=2000)) +
  scale_x_continuous(limits = c(0, 1600), breaks=seq(0, features, by=features)) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank(), plot.title=element_text(size=rel(1.4), face="bold"))

# Print the plot
experiments_mv_plot + geom_bar(
  position=position_dodge(), stat="identity", width=1, fill=ifelse(nan_distr_experiments_ord>100000, rgb(192,57,43, maxColorValue=255), rgb(52,73,94, maxColorValue=255))
) + geom_hline(aes(yintercept=mean(nan_distr_experiments_ord)), colour="#e67e22", linetype="dashed")