# Read data
data <- read.csv("assignment_1/tritium.csv")
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