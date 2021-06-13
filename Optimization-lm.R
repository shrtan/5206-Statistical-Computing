setwd("C:/Users/Shreya/Documents/GR5206/Week 6")

data <- read.table("Kutner_6_9.txt", header = T)

Q <- function(beta=c(mean(data$Y), 0, 0,0), data=data) {
  beta0 <- beta[1]
  beta1 <- beta[2]
  beta2 <- beta[3]
  beta3 <- beta[4]
  
  resid <- data$Y - 
    (beta0 + beta1*data$X1 + beta2*data$X2 + beta3*data$X3)
  return(sum(resid^2))
}

Q(beta = c(mean(data$Y), 0, 0, 0), data=data)

#feed in func I want to optimize, then use a starting point, then pass
#the original 
nlm(Q, p=c(mean(data$Y), 0, 0, 0), data=data)

lm(Y~X1+X2+X3, data=data)


neg.reg.ll <- function(params=c(mean(data$Y), 0, 0, 0, var(data$Y)), data=data) {
  beta0 <- params[1]
  beta1 <- params[2]
  beta2 <- params[3]
  beta3 <- params[4]
  true_var <- params[5]
  
  mean_Y <- beta0 + beta1*data$X1 + beta2*data$X2 + beta3*data$X3
  
  return(-1*sum(dnorm(data$Y, mean=mean_Y, sd=sqrt(true_var), log=T)))
}

neg.reg.ll(params=c(mean(data$Y), 0, 0, 0, var(data$Y)), data=data)

nlm(neg.reg.ll, p = c(mean(data$Y), 0, 0, 0, var(data$Y)), data=data)

model <- lm(Y~X1+X2+X3, data=data)
sigma_2_MLE <- sum(residuals(model)^2)/nrow(data)
sigma_2_MLE


beta.plot <- seq(0, 1, length=500)
plot(beta.plot, dbeta(beta.plot, shape1=10, shape2=10), type="l")

