rm(list=ls())
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
# FA examples
#---------------------------------------------------------------------------------------------------------------
# grades
#---------------------------------------------------------------------------------------------------------------

# read in the data set
grades <- read.table("grades.txt", header=T, sep="")

# perform factor analysis with one factor
fa_grades1 <- factanal(grades, 1)
fa_grades1

# we need more factors
fa_grades2 <- factanal(grades, 2, scores="regression")
fa_grades2

# two factors are enough, check results
biplot(fa_grades2$scores, loadings(fa_grades2), cex=0.5, col=c("black","red"), xlim = c(-3, 3))


plot(loadings(fa_grades2), xlim = c(0, 1), ylim = c(0, 1))
text(loadings(fa_grades2), labels = row.names(loadings(fa_grades2)), pos = 3)


fa.parallel(grades, fm = "mle")

# note that in this case, the conclusion on the number of factors is not clear and you will 
# may get different results when you run the code a few times


#---------------------------------------------------------------------------------------------------------------
# life expectancy
#---------------------------------------------------------------------------------------------------------------

life <- read.table("life.txt", header = F)
row.names(life) <- life[,1]

# 1-factor model
life_fa1 <- factanal(life[,-1], 1, scores="regression")
life_fa1

# 2-factor model
life_fa2 <- factanal(life[,-1], 2, scores="regression")
life_fa2

# 3-factor model
life_fa3 <- factanal(life[,-1], 3, scores="regression")
life_fa3

# plot the scores

scores <- factanal(life[,-1], factors = 3, method = "mle", scores = "regression")$scores

plot(scores[,1], scores[,2], type = "n", xlab = "Factor 1", ylab = "Factor 2")
text(scores[,1], scores[,2], abbreviate(rownames(life), 5), cex = 0.7)

plot(scores[,1], scores[,3], type = "n", xlab = "Factor 1", ylab = "Factor 3")
text(scores[,1], scores[,3], abbreviate(rownames(life), 5), cex = 0.7)

plot(scores[,2], scores[,3], type = "n", xlab = "Factor 2", ylab = "Factor 3")
text(scores[,2], scores[,3], abbreviate(rownames(life), 5), cex = 0.7)




