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

# Question 4: Predicting tritium level curve with contour plot

# Variables
y <- tritium
x1 <- longitude
x2 <- latitude

# Load the mgcv package
library(mgcv)

# Fits GAM with thin plate spline over (longitude, latitude)
est_gam <- gam(y ~ s(x1, x2))

# Grid of values for prediction
x1val <- seq(min(x1), max(x1), length.out = 100)
x2val <- seq(min(x2), max(x2), length.out = 100)
xval <- expand.grid(x1 = x1val, x2 = x2val)

# Predicting on the grid
yhat <- predict(est_gam, newdata = xval)
ymat <- matrix(yhat, 100, 100)

# Contour plot
contour(x = x1val, y = x2val, z = ymat,
        xlab = "Longitude", ylab = "Latitude",
        xlim = range(x1), ylim = range(x2))
points(x1, x2, col = 2, pch = 20, cex = 0.5)
