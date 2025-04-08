# Read data
data <- read.csv("assignment_1/tritium.csv")
attach(data)

# Plot data
plot(longitude, latitude, col=2, pch=20, cex=0.5, ylim=c(0,60), xlim=c(-100,10))
library(rworldmap)
plot(getMap(), asp = 1, add=T, col="gray60")

# Question 1: Kernel density estimator using Gaussian kernel
n = nrow(data)
n

h <- sd(tritium)*(4/(3*n))^(1/5)
h

d <- density(tritium, bw=h, kernel = "gaussian")
d

plot(d$x, d$y, type="l", xlab="x", ylab="kernel density")
points(tritium, rep(0,n), pch = 3)

# Question 2: Confidence interval

K22 <- 1/(2*sqrt(pi))

ci_up <- d$y + qnorm(0.975)*sqrt(d$y*K22)/sqrt(n*h)
ci_lo <- d$y - qnorm(0.975)*sqrt(d$y*K22)/sqrt(n*h)

plot(d$x, d$y, type="l", xlab="x", ylab="density", ylim=c(0, max(ci_up)))
points(tritium, rep(0,n), pch = 3)
points(d$x, ci_up, type="l", lty=2)
points(d$x, ci_lo, type="l", lty=2)
points(d$x, dnorm(d$x), type="l", col=2, lwd=2)

# Question 3: Kernel density estimator using Epanechnikov kernel
