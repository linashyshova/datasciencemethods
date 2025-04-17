library(splines)

data <- read.csv("assignment_1_2/tritium.csv")
attach(data)

n=550
x=pressure[1:n]
y=tritium[1:n]

# Question 1: Regression spline model based on a cubic B-spline basis

est <- lm(y ~ bs(x, knots=c(-0.2,0.2), degree=3))

xval <- seq(min(x),max(x),length.out = 1000)
yhat <- predict(est, newdata = data.frame(x=xval))

plot(x, y, col="gray40")
points(xval, yhat, type = "l", col=2, lwd=2)

# Question 2: Select knots

# Set AIC to maximum possible integer value to later compare with it
AIC = .Machine$integer.max
best_k = 0

for (k in 1:10) {
  knots <- quantile(x, probs = seq(0, 1, length.out = k + 2))[2:(k + 1)]
  model <- lm(y ~ bs(x, knots=knots, degree=3))
  aic_val <- AIC(model)
  
  if (aic_val < AIC) {
    AIC = aic_val
    best_k = k
    best_model = model
  }
}

cat("Optimal number of knots", best_k)
xval <- seq(min(x),max(x),length.out = 1000)
yhat <- predict(best_model, newdata = data.frame(x=xval))

plot(x, y, col="gray40")
points(xval, yhat, type = "l", col=2, lwd=2)
