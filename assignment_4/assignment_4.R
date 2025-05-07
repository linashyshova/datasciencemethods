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

# Question 2 - non-parametric logistic regression
est_s <- gam(y ~ s(x), family = binomial)

# Predicting on the grid
xval <- data.frame("x" = seq(min(x), max(x), length.out = 500))
pred <- predict(est_s, xval, se.fit = TRUE)

# Extract predicted log-odds and compute 95% confidence intervals
pred_val <- pred$fit
pred_up <- pred_val + qnorm(0.95) * pred$se.fit
pred_lo <- pred_val + qnorm(0.05) * pred$se.fit

# Logistic transformation to get probabilities from log-odds
prob_fit <- exp(pred_val) / (1 + exp(pred_val))
prob_up  <- exp(pred_up)  / (1 + exp(pred_up))
prob_lo  <- exp(pred_lo)  / (1 + exp(pred_lo))

plot(x, y, ylab = "Conditional probability y=1", xlab = "Alcohol", pch = 20, col = rgb(0,0,0,0.2))
points(xval$x, prob_fit, type = "l", col = 2, lwd = 2)
points(xval$x, prob_up,  type = "l", col = 2, lwd = 2, lty = 2)
points(xval$x, prob_lo,  type = "l", col = 2, lwd = 2, lty = 2)

# Question 3 - point prediction
pred_point <- predict(est_s, newdata = data.frame(x = 9.5), se.fit = TRUE)

# Extract predicted log-odds and compute 95% confidence intervals
pred_point_fit <- pred_point$fit
pred_point_se  <- pred_point$se.fit

pred_point_up <- pred_point_fit + qnorm(0.95) * pred_point_se
pred_point_lo <- pred_point_fit + qnorm(0.05) * pred_point_se

# Logistic transformation to get probabilities from log-odds
prob_point_fit <- exp(pred_point_fit) / (1 + exp(pred_point_fit))
prob_point_up  <- exp(pred_point_up)  / (1 + exp(pred_point_up))
prob_point_lo  <- exp(pred_point_lo)  / (1 + exp(pred_point_lo))
