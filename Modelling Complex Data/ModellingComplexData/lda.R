# read in data
library(MASS)
data("Pima.tr")

# pairs plot
pairs(Pima.tr[,1:7], pch = c(3,2) [unclass(Pima.tr$type)], col = 1:2)


# perform lda
diab_lda1 <- lda(Pima.tr$type~., data = Pima.tr)
diab_lda1

plot(density)
library(lattice)
densityplot(~predict(diab_lda1)$x, groups=Pima.tr$type)

# plot lda results
library(klaR) # Classification and visualization package

partimat(x=Pima.tr[,1:7], grouping=as.factor(Pima.tr$type), method="lda", 
         col.mean=1, image.colors = c("white","lightgrey"), prec = 400)

# confusion matrix
table(Pima.tr[,8], predict(diab_lda1, Pima.tr)$class)


