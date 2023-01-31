# Today, we will learn how to fit linear models in R and interpret the parts of the
# output related to estimation and inference about the parameters.
# We will work with a data set on the operation of an industrial chemical process.
# The variable of interest is the parts per thousand (i.e. 10 times the percentage) of
# ingoing ammonia that escapes (Stkloss); we want this to be low for environmental reasons.
# We also have data on three variables that might be related to the loss of ammonia:
#   the air flow rate at which the plant is run (Air); the temperature of the cooling water(Temp);
# and the acid concentration (Acid,in parts per thousand minus 500).


# The data are in the file Stackloss.txt on Keats. Download the file
# in your current working directory (ask for help with this if needed!)

Data<-read.table("Stackloss.txt",header=TRUE)
head(Data)
attach(Data)

# Let's start with some exploratory plot:

plot(Data)

# You can explore individual scatterplot, like:

plot(Air,Stkloss)
plot(Temp,Stkloss)
plot(Acid,Stkloss)

# Discussion point 1: 
# From the scatterplots, which variable(s) you expect will explain most of
# the variation in Stkloss?


# We can fit the simple linear regression of ammonia loss on air flow using the
# command:

lm(Stkloss~Air)

# This gives the estimates of the parameters beta_0 and beta_1, labelled in an obvious way.
# However, having calculated and displayed these, R does not keep them. It is much
# more useful to type:

Model1 <- lm(Stkloss~Air)

# So far we have just seen the parameter estimates. R can give us a lot more information
# about the fitted linear model. For now, the most useful command is

summary(Model1)

# The output gives you a table where for each coefficient in the model
# you are given:
# Estimate
# Standard error (i.e. the estimated standard deviation of the estimator for beta_j)
# t-value: the test statistics T_j for the test H0: beta_j=0 vs H1: beta_j != 0
# Pr(>|t|): p-value of the above test.


# Discussion point 2:

# What conclusion will you draw about the test for the coefficient 
# in front of Air in Model1 and what does that mean for the relationship between 
# Air and Stkloss?


# We can now fit a multiple linear regression model with all the variables in the dataset:

ModelFull <- lm(Stkloss~Air+Temp+Acid)
summary(ModelFull)

# Discussion point 3:

# On the basis of this fitted model, what would you suggest to decrease Stkloss?


# Finally, we may want to include some basis functions/ variable transformations:

ModelPoly <- lm(Stkloss~Air+Temp+I(Temp^2)+Acid) # https://stackoverflow.com/questions/27050431/what-is-the-difference-between-x2-and-ix2-in-r
summary(ModelPoly)

# Discussion point 4:

# We have not talked about model comparison yest, but have a look at the output
# of model ModelPoly and ModelFull and discuss which one looks better and why.
