#Define f(x)
f <- function(x) {
  return(ifelse((x < 0 | x > 2*pi), 0, (sin(x)+2)/(4*pi)))
}

x <- seq(0, 2*pi, by = 0.01)
plot(x, f(x), type = "l", ylab = "f(x)", ylim=c(0, 0.3))

#Find x where f(x) is max
#Solve f'(x) = 0
x_max = pi/2
abline(v=pi/2, col = "purple")
f_max <- f(x_max)

#Define envelope term e(x)
e <- function(x) {
  return(ifelse((x < 0 | x > 2*pi), Inf, f_max))
}
lines(x, e(x), col = "pink")


n_samps <- 1000      #num of desired samples
n <- 0               #counter for num of samples in loop

samps <- numeric(n_samps)   #stores actual samples

while(n < n_samps) {
  y <- runif(1, min=0, max=2*pi)  #random draw from g
  u <- runif(1)
  if(u < f(y)/e(y)) {
    n <- n + 1      #add to sample counter
    samps[n] <- y   #stores sample for f(x)
  }
}

hist(samps, prob = T, ylab = "f(x)", xlab = "x")
lines(x, f(x), type = "l", ylab = "f(x)", ylim=c(0, 0.3), col="blue")




