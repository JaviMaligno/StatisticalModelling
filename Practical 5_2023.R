
############################################################ 
##              Statistical Modelling 2023                ##
##                  Practical 5                           ##
##                                                        ##
##                Dr. Davide Pigoli/Yu Luo                ##
############################################################


# 1) TWO-WAY ANOVA

# This is an example of model with only
# categorical variables as predictors.

# We are going to use the ToothGrowth data available in R.

data("ToothGrowth")

attach(ToothGrowth)

# Let's start with exploratory data analysis and
# look at interaction plot and mean effect plots. 

# Mean effect plots


sup.name<-unique(supp) # define the levels of the factor
mean.effects<-c(mean(len[supp==sup.name[1]]),mean(len[supp==sup.name[2]])) # compute the mean of the response for each level
#values of the mean effect for supplement
mean.effects

# plot of the mean effect for supplement
plot(c(-1,1),mean.effects,xaxt='n',type='b', xlab="Supplement type",ylab="Mean length", main = "Mean effect of supplement type")
axis(2,at=c(15,16,17,18,19,20,21))
axis(1,at=c(-1,1),labels=sup.name)

# same for the dose:
dose.name<-unique(dose)
mean.effects<-c(mean(len[dose==dose.name[1]]),mean(len[dose==dose.name[2]]),mean(len[dose==dose.name[3]]))
mean.effects
plot(dose.name,mean.effects,type='b',xlab="dose",ylab="Mean length",main="Main effect of the dose")


# intercation plot:

interaction.plot(dose,supp,len)

# Discussion point 1:

# simply looking at this visual exploration, how would you describe
# the effect of the dose and the supplement on the tooth length and their interaction?


# Now, fit a linear regression model to confirm this:

# This will treat dose as a continuous variable:

model_ANOVA<-lm(len~dose*supp)
summary(model_ANOVA)

# If the dose should be treated as categorical in you model,
# you need to force it to be factor:

Dose<-factor(dose)

model_TWO_WAY_ANOVA<-lm(len~Dose*supp)
summary(model_TWO_WAY_ANOVA)


# Which approach to be used depend on the practical problem at hand,
# i.e. if there is an interest in a continuous response from the dose or not.


# Warning: it is always good practice to check that the variable tha you want to treat as
# categorical are coded as factor in R:

is.factor(dose)
is.factor(Dose)

# since this is not always immediately obvious looking at columns in a dataset.

# Discussion point 2:

# Interpret the two models that you have fitted. Do they confirm
# your impressions from the exploratory data analysis? Is there any
# difference in interpretation between the two (Hint: look at the interaction terms)?


# 2) Optimisation

# Let us use some simulated data:

set.seed(100) # this is to have the same simulate data, but you can try changing it!
x1<-c(-1,-0.5,0.5,1,-1,-0.5,0.5,1,0.3,-0.3,0.7,-0.7)
x2<-sample(x1,length(x1)) # x2 is a reshuffled version of x1
y<-(x1-0.5)^2+(x2+0.7)^2+x1*x2+rnorm(length(x1),0,0.1)
data<-data.frame(y,x1,x2)
plot(data)

# Goal: find the values of x1 and x2 that minimise y.

# Let's fit a quadratic model:

model_opt<-lm(y~x1*x2+I(x1^2)+I(x2^2)) 
summary(model_opt)

# In this case we know this is the right model, in a
# real application we may need to do remove some of the terms.

# Let's extract the vector b and matrix B needed to compute the stationary point:

beta<-model_opt$coefficients
b<-beta[2:3]
B<-matrix(0,2,2)
B[1,1]<-beta[4]
B[2,2]<-beta[5]
B[1,2]<-B[2,1]<-0.5*beta[6]

x_opt<--0.5*solve(B)%*%b
x_opt

# Now we need to check if it a maximum, a minimum or neither.

eigen(B)$values # eigenvalues of B

# Remember: if they are all positive, it is a minimum
#           if they are all negative, it is a maximum
#           else, it is neither.

# Second thing to check is that it is in the range where we have observations:

range(x1)
range(x2)

# Discussion point 3: Is it within the observation range?

# If it is not, the "correct" answer (apart for collecting more data), it 
# is to choose the closest point in the range of the observations.


# Optional: try to define a new grid x1 and x2 centered on the value you found,
#  simulate new data and repeat the exercise.



# 3) Generalised linear models:

# Poisson regression

# The data in the file insurance.txt (on the KEATS page) show the number of claims during
# the last financial year and the
# ages of 35 customers for a particular health insurance plan.
# The insurance company wants to investigate how the number of claims
# is related to a customer's age.
# Read the data into R:

insdat <- read.table("insurance.txt", header=TRUE)
attach(insdat)

# We can fit a Poisson regression model using the glm command:

i1.glm <- glm(nclaims ~ age, family=poisson(link="log"))

# Let's have a look at the summary:

summary(i1.glm)

# Discussion point 4: 

# how would you describe the relationship between age and the number of insurance claims?

# We can do model selection using the step() function again:

i2.glm<-step(i1.glm)

# In this case the only option would be to remove the age as predictor,
# and the AIC suggests to keep it in the model!

# and we can plot the relationship between age and the expected number of insurance claim:

age_x<-seq(min(age),max(age))
beta<-i2.glm$coefficients
plot(age,nclaims)
points(age_x,exp(beta[1]+beta[2]*age_x),type='l',lwd=2)

# and check the diagnostics plots:
plot(i2.glm)

# Binary (logistic) regression

# Data are collected for 40 patients receiving
# a new surgery technique. The variable surv takes the value 1
# if the patient show any  negative side effect in the 30 days after surgery and is 0 otherwise.
# The age (years) of the patient is also recorded. 
# 
# This are binary data, where the response is 0/1, and 
# we need to use a Bernoulli regression, also called LOGISTIC regression (from the name of the link function).
# The question of interest is to see how the probability $p$ of having negative side effects
#  depends on age.

# Import data and visual exploration:

surg <- read.table("surgery.txt",header=T)
attach(surg)
head(surg)
plot(Age, surv)

# Fit a logistic regression/ Bernoulli regression model:

surg1.glm <- glm(surv ~ Age, binomial, data=surg)
summary(surg1.glm)

# Again, we can use the step() function to see if we should keep Age in the model:

step(surg1.glm)

# What do you think?

# Let's also plot the probability of having side effect 
# (i.e. the mean of our Bernoulli response) given age:

plot(Age, surv, pch=20)
coefs = surg1.glm$coefficients
Ages = seq(from=49, to=73, length.out=1000)
g_mu = coefs[1] + Ages*coefs[2]
probs = exp(g_mu)/(1 + exp(g_mu)) # inverse of the link function
points(Ages, probs, type="l")

# What would you conclude from this graph?

