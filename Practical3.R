############################################################ 
##              Statistical Modelling 2022                ##
##                  Practical 3                           ##
##                                                        ##
##                Dr.Davide Pigoli                        ##
############################################################

# In this session we will explore further the R function for linear models, 
# including further inference and diagnostics plots.


# We will consider again the data set on the operation of an industrial 
# chemical process. Recall that the variable of interest is the parts per 
# thousand (i.e.\ 10 times the percentage) of ingoing ammonia that escapes; 
# we want this to be low for environmental reasons. We also have data on three 
# variables that might be related to the loss of ammonia: the air flow 
# rate at which the plant is run; the temperature of the cooling water; and 
# the acid concentration (in parts per thousand minus 500). 

# The data are in the file Stackloss.txt on KEATS and you must first download them in your working directory. 
# Read the data in again

Data<-read.table("Stackloss.txt",header=TRUE)
head(Data)
attach(Data)

# Let us fit again the model

ModelFull <- lm(Stkloss~Air+Temp+Acid)
summary(ModelFull)

# 1) Inference for the parameters

beta<-ModelFull$coefficients # extract the coefficients value

beta_sd<-summary(ModelFull)$coefficients[,2] # extract the standard errors of the coefficients

# (You could have also read these value from the summary table)

# Apply the formula for a 95% confidence interval for the coefficient in front of Air:

# Lower bound
beta[2]-qt(0.975,17)*beta_sd[2]
beta[2]+qt(0.975,17)*beta_sd[2]

# the function qt(0.975,17) gives us the 0.975 quantile of a student-t with 17 degrees of freedom.

# There is of course also a function to produce all the intervals automatically:

confint(ModelFull,level = 0.95)

# check that is giving you the same result!

# We cal also compute confidence intervals for the error variance sigma^2:

s2<-summary(ModelFull)$sigma^2


# Lower bound
(17*s2)/qchisq(0.975,17)

# Upper bound:
(17*s2)/qchisq(0.025,17)

# where again qchisq() is giving us the quantiles of a chi-squared distribution.


# 2) Confidence and prediction interval for the response 

# Confidence and prediction intervals for the response variables can be obtained
# using the function predict().


# First note that 

predict(ModelFull)

# only give you the fitted values, same as

ModelFull$fitted.values

# However, what you can do is to give new values of the predictor to the function:

New.Values <- data.frame(Air=60,Temp=20,Acid=59)
predict(ModelFull, New.Values)

# Using the parameter interval in the function, you can also produce
# confidence interval:

predict(ModelFull, New.Values, interval="confidence")

# and prediction intervals:

predict(ModelFull, New.Values, interval="prediction")

# The default level is 95%, but you can change it:

predict(ModelFull, New.Values, interval="prediction", level=0.99)

# Try doing the same for the confidence interval!



# 3) The ANOVA table  

# The function anova() allows us to compute ANOVA tables.
# However, simply typing 

anova(ModelFull)

# does something different than what we have seen in the lecture. 
# The reason is that this ANOVA table is doing a further decomposition
# of the sum-of-squares for a sequence of nested plots - we are going
# to see this in future weeks.

# To replicate the ANOVA table seen in the lecture we need to force R to compare
# the fitted model only with the so called null model, ie. the model with only the
# intercept:

ModelNull <- lm(Stkloss~1)
anova(ModelNull,ModelFull)


# Discussion point 1: interpret the output of this function, can you recognise all 
# the quantities in the ANOVA table seen in the lecture? Is anything missing?
# Is anything in a different order?


# Discussion point 2: what decision would you take on the global F-test for this
# model? What does it mean in practice?





# 4) Coefficients of determinations

# Have a look at the summary of the model

summary(ModelFull)

# and find out the values of the R^2 (called multiple R^2 in R) and the
# adjusted R^2. Is the model a good fit for the data based on these quantities?
# Which is the most appropriate to look at in this case?


# Unfortunately, R does not provides us with the predictive R^2 (at least that I know of)
# We need to compute it manually:

X<-model.matrix(ModelFull) # to obtain the design matrix X of the model.
H<-X%*%solve(t(X)%*%X)%*%t(X)  # this computes the projection matrix H
h<-diag(H)                    # this extracts the diagonal of the matrix (elements h_ii)
PRESS<-sum((ModelFull$residuals/(1-h))^2) # this computes the Predictive residuals 
SST<-sum((Stkloss-mean(Stkloss))^2)                # this computes explicitly the total sum of squares  

# And finally, the predictive R^2:
Rp<-1-PRESS/SST
Rp

# Does this support the use of this model for prediction?