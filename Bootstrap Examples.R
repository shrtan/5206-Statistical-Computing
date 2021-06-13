
# Simulate dist of max from unif(0,theta)
max_vec <- NULL
n <- 40
theta <-3
for (i in 1:1000) {
  X <- runif(n,0,theta)
  max_vec[i] <- max(X)
}
# not normal
hist(max_vec)

# bootstrap interval for maximum
set.seed(0)
# Single sample
n <- 40
X <- runif(n,0,theta)

boot_vec <- NULL
B <- 1000
for (b in 1:B) {
  boot.sample <- sample(X,n,replace=T)
  boot_vec[b] <- max(boot.sample)
}
quantile_vec <- quantile(boot_vec,probs=c(0.025,0.975))
boot.interval <- c(LL=2*max(X)-quantile_vec[2],UL=2*max(X)-quantile_vec[1])
boot.interval
# I think that the true max is between 2.976 3.256.


# When to use bootstrap in regression?
# simulate normal model
set.seed(0)
n <- 40
x <- runif(n,0,2)
Y <- x + rnorm(n,sd=.25)
model <- lm(Y~x)
plot(x,Y)
abline(model,col="blue")
# qqplot for normal errors (looks good)
qqnorm(rstudent(model))
qqline(rstudent(model))

# simulate non-normal model
set.seed(0)
n <- 40
x <- runif(n,0,4)
Y <- x + rexp(n)
model <- lm(Y~x)
plot(x,Y)
abline(model,col="blue")

# qqplot
qqnorm(rstudent(model))
qqline(rstudent(model))

# Use a bootstrap so that I don't have to assume 
# the normal distribution on the errors
# Recall that the t-test and t-interval requires normality.




