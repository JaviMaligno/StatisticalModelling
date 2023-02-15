# principal components analysis examples

#---------------------------------------------------------------------------------------------------------------
# arrests
#---------------------------------------------------------------------------------------------------------------


# read details on the data set
?USArrests

# view data set
View(USArrests)

# correlation plot
install.packages("corrplot")
library(corrplot)

par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
corrplot(cor(USArrests), method="ellipse")

# perform PCA on the covariance matrix
arr_pca_cov <- prcomp(USArrests)

# view results
summary(arr_pca_cov)

# these are the sqrt of the eigenvalues
arr_pca_cov$sdev

# these are the eigenvectors
arr_pca_cov$rotation

# the first PC has been created to account for assault
# that is because assault has a much greater variance than the other variables
diag(cov(USArrests))

# best to use the correlation matrix instead
# this is equivalent to performing PCA on the covariance matrix 
# of the standardised variables
arr_pca_cor <- prcomp(apply(USArrests, 2, scale))

# view results
summary(arr_pca_cor)

# these are the sqrt of the eigenvalues
arr_pca_cor$sdev

# these are the eigenvectors
arr_pca_cor$rotation

# these are the scores
arr_pca_cor$x

# screeplot
par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
screeplot(arr_pca_cor)

install.packages("psych")
library(psych)
par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
biplot(arr_pca_cor, cex=0.5, col=c("black","red"), scale = 0, xlabs = row.names(USArrests))

#---------------------------------------------------------------------------------------------------------------
# air pollution
#---------------------------------------------------------------------------------------------------------------

data("USairpollution", package = "HSAUR2")

# view data set
View(USairpollution)

par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
corrplot(cor(USairpollution), method="ellipse")


USairpollution$temp <- USairpollution$temp*(-1)

# perform PCA on the correlation matrix
pol_pca_cor <- prcomp(apply(USairpollution, 2, scale))
pol_pca_cor

# view results
summary(pol_pca_cor)

# screeplot
par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
screeplot(pol_pca_cor)

library(psych)
par(mfrow=c(1,1),mar = c(4.1, 4.1, 1, 1), mgp=c(2.5,1,0))
biplot(pol_pca_cor, choices=c(1,2), cex=0.7, col=c("black","red"), scale = 0, xlabs = row.names(USairpollution))
biplot(pol_pca_cor, choices=c(1,3), cex=0.7, col=c("black","red"), scale = 0, xlabs = row.names(USairpollution))
biplot(pol_pca_cor, choices=c(2,3), cex=0.7, col=c("black","red"), scale = 0, xlabs = row.names(USairpollution))
