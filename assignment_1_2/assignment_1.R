# Read data
data <- read.csv("DS methods/assignment_1/tritium.csv")
attach(data)

# Plot data
plot(longitude, latitude, col = 2, pch = 20, cex = 0.5, ylim = c(0, 60), xlim = c(-100, 10))
library(rworldmap)
plot(getMap(), asp = 1, add = T, col = "gray60")

# Question 1: Kernel density estimator using Gaussian kernel
n <- nrow(data)
n

h <- sd(tritium) * (4 / (3 * n))^(1 / 5)
h

d <- density(tritium, bw = h, kernel = "gaussian")
d

plot(d$x, d$y, type = "l", xlab = "x", ylab = "kernel density")
points(tritium, rep(0, n), pch = 3)

# Question 2: Confidence interval
K22 <- 1 / (2 * sqrt(pi))

ci_up <- d$y + qnorm(0.975) * sqrt(d$y * K22) / sqrt(n * h)
ci_lo <- d$y - qnorm(0.975) * sqrt(d$y * K22) / sqrt(n * h)

plot(d$x, d$y, type = "l", xlab = "x", ylab = "density", ylim = c(0, max(ci_up)))
points(tritium, rep(0, n), pch = 3)
points(d$x, ci_up, type = "l", lty = 2)
points(d$x, ci_lo, type = "l", lty = 2)

# Question 3: Kernel density estimator using Epanechnikov kernel
h_epan <- sd(tritium) * (4 / (3 * n))^(1 / 5)  # same h by Silverman's rule
mu2_epan <- 1 / 5
h_epan_adjusted <- h_epan * sqrt(mu2_epan)
h_epan_adjusted

d_epan <- density(tritium, bw = h_epan_adjusted, kernel = "epanechnikov")
d_epan

K22_epan <- 3 / 5 

ci_up_epan <- d_epan$y + qnorm(0.975) * sqrt(d_epan$y * K22_epan) / sqrt(n * h_epan)
ci_lo_epan <- d_epan$y - qnorm(0.975) * sqrt(d_epan$y * K22_epan) / sqrt(n * h_epan)

plot(d_epan$x, d_epan$y, type = "l", col = "red", ylim = c(0, max(ci_up_epan)))
points(tritium, rep(0, n), pch = 3)
points(d_epan$x, ci_up_epan, type = "l", lty = 2)
points(d_epan$x, ci_lo_epan, type = "l", lty = 2)

# Question 4: Comparing the estimators with Epanechnikov and Gaussian kernels

plot(d$x, d$y, type = "l", xlab = "x", ylab = "density", ylim = c(0, max(ci_up_epan)))
lines(d_epan$x, d_epan$y, type = "l", col = "red")

points(tritium, rep(0, n), pch = 3)
legend("topright", legend = c("Gaussian", "Epanechnikov"), col = c("black", "red"), lty = c(1, 1))
