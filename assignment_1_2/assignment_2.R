# Read data
data <- read.csv("assignment_1_2/tritium.csv")
attach(data)

# Question 1: Nadaraya-Watson regression
x=pressure
y=tritium

NW_CV <- function(x,y,h){
  n <- length(x)
  mcv <- rep(0,n)
  for(i in 1:n){
    mcv[i] <- ksmooth(x[-i], y[-i], kernel="normal", bandwidth=h, x.points=x[i])$y
  }
  cv <- mean((y-mcv)^2)
  return(cv)
}

min_h <- optim(par = 0.3, fn=function(h) NW_CV(x,y,h), method = "BFGS")
(h_cv <- min_h$par)

x_val <- seq(from=min(x), to=max(x), length.out = 1000)
NWe <- ksmooth(x, y, kernel="normal", bandwidth=h_cv, x.points=x_val)
str(NWe)

plot(x, y, xlab = "Pressure", ylab = "Tritium", col="gray40")
points(NWe$x, NWe$y, type = "l", col = 2, lwd = 2) 

# Question 2: 95% confidence intervals

NWe1 <- ksmooth(x, y, kernel="normal", bandwidth=h_cv, x.points=x)
s2 <- mean((y[order(x)]-NWe1$y)^2)
K2 <- 2*qnorm(0.75)/sqrt(pi)
fx = density(x, from=min(x), to=max(x), n=1000)
NWe2 <- ksmooth(x, y, kernel="normal", bandwidth=h_cv, x.points = fx$x)
CI_up <- NWe2$y + qnorm(0.975)*sqrt(s2*K2)/sqrt(n*h_cv*fx$y)
CI_lo <- NWe2$y - qnorm(0.975)*sqrt(s2*K2)/sqrt(n*h_cv*fx$y)

plot(x,y, xlab = "Pressure", ylab = "Tritium", col="gray50")
points(NWe$x,NWe$y,type = "l",col=2,lwd=2)
points(NWe$x,CI_up,type = "l",col=2,lwd=2,lty=2)
points(NWe$x,CI_lo,type = "l",col=2,lwd=2,lty=2)



               
# Question 4: Local quadratic regression using leave-one-out-cross-validation for h              
               
h_seq <- seq(0.05, 1, by = 0.01)
cv_errors <- numeric(length(h_seq))

for (j in seq_along(h_seq)) {
  h <- h_seq[j]
  errors <- numeric(length(x))
  
  for (i in 1:length(x)) {
    x_train <- x[-i]
    y_train <- y[-i]
    
    z <- x_train - x[i]
    w <- dnorm(z / h)
    
    fit <- lm(y_train ~ z + I(z^2), weights = w)
    y_pred <- coef(fit)[1]
    errors[i] <- (y[i] - y_pred)^2
  }
    cv_errors[j] <- mean(errors)
}
optimal_h <- h_seq[which.min(cv_errors)]


xval <- seq(min(x), max(x), length.out = 802)
lqest <- rep(NA, length(xval)) 
               
for (i in 1:length(xval)) {
  z <- x - xval[i]
  w <- dnorm(z / optimal_h)  
  wls <- lm(y ~ z + I(z^2), weights = w)
  lqest[i] <- wls$coef[1]
}

plot(x, y, pch = 16, col = "gray60",
     xlab = "Pressure", ylab = "Tritium", 
     main = "Local Quadratic Regression (p = 2)")
lines(xval, lqest, col = 2, lwd = 2)

# Question 5: Local quadratic regression with first and second derivative

xval <- seq(min(x), max(x), length.out = 802)

lqest <- rep(NA, length(xval))       
lqest_prime <- rep(NA, length(xval)) 
lqest_double_prime <- rep(NA, length(xval)) 

for (i in 1:length(xval)) {
  z <- x - xval[i]
  w <- dnorm(z / optimal_h)
  wls <- lm(y ~ z + I(z^2), weights = w)  
  coefs <- coef(wls)
  
  lqest[i] <- coefs[1]                   
  lqest_prime[i] <- coefs[2]               
  lqest_double_prime[i] <- 2 * coefs[3]     
}

plot(x, y, pch = 16, col = "gray60",
     xlab = "Pressure", ylab = "Tritium", 
     main = "Local Quadratic Regression with Derivatives")
lines(xval, lqest, col = "blue", lwd = 2)
lines(xval, lqest_prime, col = "red", lwd = 2)
lines(xval, lqest_double_prime, col = "green", lwd = 2)
legend("topright", legend = c("Regression", "First Derivative", "Second Derivative"),
       col = c("blue", "red", "green"), lwd = 2)

               
