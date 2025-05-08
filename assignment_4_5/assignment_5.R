library(mgcv)
library(glmnet)

# Read data
data <- read.csv("assignment_4_5/red_wine.csv")
head(data)

# Another subset of data
# data_train <- data[599:nrow(data), ]
# data_test <- data[1:599,]

data_train <- data[1:1000, ]
data_test <- data[1001:nrow(data),]

y <- data_train[, ncol(data_train)]
X <- as.matrix(data_train[, -ncol(data_train)])
n <- nrow(X)

y_test <- data_test[, ncol(data_test)]
X_test <- as.matrix(data_test[, -ncol(data_test)])

# Question 1 - parametric logistic regression on all regressors for first 1000 of rows
model <- gam(y ~ X, family = binomial)
summary(model)

# Question 2 - logistic regression model with L2 penalty (Ridge)
cv.ridge <- cv.glmnet(X, y, alpha = 0, nfolds=n, grouped=FALSE)
cv.ridge$lambda.min

ridge <- glmnet(X, y, alpha = 0, lambda = cv.ridge$lambda.min)
coef(ridge)

# Question 3 - logistic regression model with L1 penalty (Lasso)
cv.lasso <- cv.glmnet(X, y, alpha = 1, nfolds=n, grouped=FALSE)
cv.lasso$lambda.min

lasso <- glmnet(X, y, alpha = 1, lambda = cv.lasso$lambda.min)
coef(lasso)

# Question 4 - comparing the accuracy of the different methods
mxval <- matrix(X_test,nrow = nrow(X_test))
data_test_X <- data.frame(X = I(X_test))

y_pred_ridge_prob = predict(ridge,mxval)
y_pred_lasso_prob = predict(lasso,mxval)
y_pred_ml_prob <- predict(model, newdata = data_test_X, type = "response")

# Apply 0.5 threshold
y_pred_ridge <- ifelse(y_pred_ridge_prob > 0.5, 1, 0)
y_pred_lasso <- ifelse(y_pred_lasso_prob > 0.5, 1, 0)
y_pred_ml <- ifelse(y_pred_ml_prob > 0.5, 1, 0)

# Calculate average value of correct predictions (accuracy)
acc_ridge <- mean(y_pred_ridge == y_test)
acc_lasso <- mean(y_pred_lasso == y_test)
acc_ml <- mean(y_pred_ml == y_test)

cat("Accuracy (Ridge):", round(acc_ridge, 4), "\n")
cat("Accuracy (Lasso):", round(acc_lasso, 4), "\n")
cat("Accuracy (No shrinkage):", round(acc_ml, 4), "\n")
