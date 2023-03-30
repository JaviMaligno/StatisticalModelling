N <- 100
P <- seq(0.0001, 0.9999, length.out = 1000)
prior <- dbeta(P, 1,1, log = FALSE) # very non informative, gives little weight to every possible value
plot(P,prior, type = "l")

dice <- 1:6
result <- sample(dice, size= N, replace = TRUE)
n <- sum(result==1)
evidence <- P^n*(1-P)^(N-n)
plot(P, evidence, type = "l")

posterior <- prior *  evidence
plot(P, posterior, type = "l")

result <- sample(dice, size= N, replace = TRUE)
n <- sum(result==1)
evidence <- P^n*(1-P)^(N-n)

posterior <- posterior *  evidence #update belief
plot(P, posterior, type = "l")

N <- 100
P <- seq(0.0001, 0.9999, length.out = 1000)
prior2 <- dbeta(P, 5,20, log = FALSE) # more informative
plot(P,prior2, type = "l")
evidence <- P^n*(1-P)^(N-n)
plot(P, evidence, type = "l")

posterior2 <- prior2 *  evidence
plot(P, posterior2, type = "l")

N <- 100
P <- seq(0.0001, 0.9999, length.out = 1000)

prior3 <- dbeta(P, 10, 10, log = FALSE) # more informative
plot(P,prior3, type = "l")
evidence <- P^n*(1-P)^(N-n)
plot(P, evidence, type = "l")

posterior3 <- prior3 *  evidence
plot(P, posterior3, type = "l")

posterior3up <- posterior3 *  evidence
plot(P, posterior3up, type = "l")


posterior <- posterior/sum(posterior) #normalizing to get probability distribution
cumulative <- cumsum(posterior)
plot(P, posterior, type = "l")

#0.95 confidence region
lower <- P[max(which(cumulative < 0.025))]
upper <- P[min(which(cumulative > 1- 0.025))]
plot(P, posterior, type = "l")
abline(v=lower, col = "red")
abline(v=upper, col = "red")


