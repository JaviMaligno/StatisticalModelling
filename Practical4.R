############################################################ 
##              Statistical Modelling 2022                ##
##                  Practical 4                           ##
##                                                        ##
##                Dr.Davide Pigoli                        ##
############################################################

# In this practical we discuss model selection 


# The data are in the file Stackloss.txt on KEATS and you must first download them in your working directory. 
# Read the data in again
setwd("Z:\\GitHub\\StatisticalModelling")

Data<-read.table("Stackloss.txt",header=TRUE)
head(Data)
attach(Data)


# 1) Partial F-tests

# Let's start with the full model:

ModelFull <- lm(Stkloss~Air+Temp+Acid)
summary(ModelFull)


# and compare it with the model with only Temp as predictor

ModelRed<-lm(Stkloss~Temp)

# We can carry out the partial F-test to compare this two models
# using the anova() command

anova(ModelRed,ModelFull)

# Discussion point 1: 
# What can you conclude from this result?



# 2) Comparison of non-nested models:

# Partial F-test cannot be used to compare non nested models.

# Consider the model:

Model2<-lm(Stkloss~Air+Acid)
summary(Model2)

# Discussion point 2:

# Look at the summary of the two models, which one is
# better based on the adjusted R2?

# Let's now see other ways to compare the two models:

# AIC can be computed with the function AIC()

AIC(ModelRed)
AIC(Model2)
AIC(ModelFull)

# Discussion point 3:
# Which is the best model based on AIC?

# Optional: Try listing all the possible models 
# based on this 3 predictors and choose the best one based on AIC.
ModelNull<-lm(Stkloss~1)
ModelAir <-lm(Stkloss~Air)
ModelAcid <-lm(Stkloss~Acid)
ModelTempAir <-lm(Stkloss~Temp+Air)
ModelTempAcid <-lm(Stkloss~Temp+Acid)

AIC(ModelNull)
AIC(ModelAir)
AIC(ModelAcid)
AIC(ModelTempAir)
AIC(ModelTempAcid)

# As seen in the previous practical, predictive R^2 is not
# easily available but needs to be computed.

# Since we want to do that for many models, we are going to define
# a FUNCTION to do it:

pred.R2<-function(model){
  X<-model.matrix(model) # to obtain the design matrix X of the model.
  H<-X%*%solve(t(X)%*%X)%*%t(X)  # this computes the projection matrix H
  h<-diag(H)                    # this extracts the diagonal of the matrix (elements h_ii)
  PRESS<-sum((model$residuals/(1-h))^2) # this computes the Predictive residuals 
  SST<-sum((model$model[,1]-mean(model$model[,1]))^2)                # this computes explicitly the total sum of squares  
  
  # And finally, the predictive R^2:
  Rp<-1-PRESS/SST
  Rp
}


pred.R2(ModelFull)
pred.R2(ModelRed)
pred.R2(Model2)
pred.R2(ModelTempAir)

# Discussion point 4: which one is better?



#3) Step-wise search:

# We now try a step-wise search based on the AIC:

step(ModelFull)

# you can read the final selected model at the end of the
# (long) output
# or save it:

Model.best<-step(ModelFull)
summary(Model.best)

plot(Model.best) 

# Do you see any issue with the model assumptions?

#the first plot shows a pattern that for possitive values the residual is greater an viceversa, so assumptions of normality, independence, homoscedasticity etc may be wrong, also the data set is small
# cook distance tells if some point has too much influence, in the lot there is one point that is way lower than the others