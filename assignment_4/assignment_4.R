library(mgcv)


# Read data
data <- read.csv("assignment_4/red_wine.csv")
attach(data)

# Question 1 - parametric logistic regression
x <- alcohol
y <- quality
est_par <- gam(y~x,family = binomial)

# Make predictions and plot
x_grid <- seq(min(x), max(x), length.out = 100)
pred <- predict(est_par, newdata = data.frame(x = x_grid), type = "response")
plot(x, y, pch = 16, col = "gray", main = "Logistic Regression", xlab = "Alcohol", ylab = "Conditional probability y=1")
lines(x_grid, pred, col = "blue", lwd = 2)

# Question 2


# Question 3