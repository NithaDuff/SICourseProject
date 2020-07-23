#Exponential Distribution ~ Central Limit Theorem.
#Initialise variable
lambda <- 0.2
mean <- sd <-  1/lambda
n <- 40
nosim <- 1000
x <- rexp(n,lambda)

#Simulate nosim simulations
samples <- matrix(sample(x,n*nosim,replace = T),nosim,n)
means <- apply(samples,1,mean)

#means <- (means - mean(means))/sd(means)
mu0 <- mean(means)
print(c(mean,mu0))
#Plot results
hist(means, probability = TRUE)
curve(dnorm(x, mean=mean(means), sd=sd(means)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
abline(v = mean, col = "red", lwd = 2)
abline(v = mu0,col = "green", lwd = 2)


vars <- apply(samples, 1, var)
print(c(sd^2,mean(vars)))
hist(vars, probability = TRUE)
curve(dnorm(x, mean=mean(vars), sd=sd(vars)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
abline(v = sd^2, col = "red", lwd = 2)
abline(v = mean(vars),col = "green", lwd = 2)

par(mfrow = c(1,2))
ux <- runif(1000)
hist(ux)

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)
curve(dnorm(mns, mean=mean(mns), sd=sd(mns)), col="cyan", lwd=2, add=TRUE, yaxt="n")

