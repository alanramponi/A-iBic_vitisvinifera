###################################################################################################
# Author:         Alan Ramponi                                                                    #
# Course:         Biological Data Mining Lab                                                      #
# Date:           January 13th, 2015                                                              #
# Description:    Data visualisation: model assessment.                                           #
###################################################################################################


# Define the vectors of performances
error_mean <- c(0.139,0.067,0.057,0.033)
error_svd <- c(0.139,0.066,0.039,0.012)
error_knn <- c(0.134,0.066,0.057,0.024)
error_svt <- c(0.132,0.064,0.039,0.012)
error_svt_appr <- c(0.134,0.066,0.048,0.020)
# order of testing: 3, 4, 1, 2.

# Calculate range from 0.01 to max value
g_range <- range(0.01, error_mean, error_knn)

# Graph the error of the mean
plot(error_mean, type="l", col="red", ylim=g_range, axes=FALSE, ann=FALSE)

# Make x axis using Biclusters labels
axis(1, at=1:4, lab=c("Bicluster 1","Bicluster 2","Bicluster 3", "Bicluster 4"))

# Make y axis with horizontal labels that display ticks at every 0.01 marks
axis(2, las=1, at=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.11,0.12,0.13,0.14))

# Create box around plot
box()

# Choose the color of the lines
lines(error_knn, type="l", pch=22, lty=1, col="blue")
lines(error_svd, type="l", pch=22, lty=1, col="orange")
lines(error_svt_appr, type="l", pch=22, lty=1, col="chartreuse3")
lines(error_svt, type="l", pch=22, lty=1, col="darkgreen")

# Create a title
title(main="Model assessment")

# Label the x and y axes
title(xlab="Biclusters of the original dataset")
title(ylab="Mean squared error (MSE)")

# Create a legend that uses the same line colors and points used by the actual plot 
legend(2.9, g_range[2], c("raw mean", "WkNN", "SVD", "aSVT", "SVT"), cex=0.7, 
       col=c("red","blue", "orange", "chartreuse3", "darkgreen"), lty=1)