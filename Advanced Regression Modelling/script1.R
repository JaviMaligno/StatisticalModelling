set.seed(01032023)

sample_size <- 100

age <- runif(sample_size, min = 18, max = 75)

sex <- sample(c(0,1), size = sample_size, replace = TRUE)

#age <- floor(age) # alternatively use sample(18:75, size = sample_size, replace = TRUE)

beta0 <- 1
beta1 <- 2
beta2 <- 4
error <- rnorm(sample_size, mean = 0, sd = 6)

income <- beta0 + 
          beta1 * age + 
          beta2 * sex +
          error

plot(age, income)

library(ggplot2)
ggplot() + geom_point(aes(x = age, y = income, color = factor(sex)))
                      
lm(income~age)

y <- income
X <- cbind(1, age, sex) 
beta_hat <- solve(inv(t(X)%*%X)%*%t(X)%*%y)
