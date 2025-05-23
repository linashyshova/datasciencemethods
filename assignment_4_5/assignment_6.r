library(nnet)

wine <- read.csv("red_wine.csv")

# Exercise 1: NN with 2 hidden units

data_subset <- wine[1:1000, ]

X <- as.matrix(data_subset[, -ncol(data_subset)])
y <- data_subset[, "quality"]


est_nn <- nnet(X,y, size=2, decay=0, linout=F, maxit=2000)

summary(est_nn)

# Exercise 2: NN with 50 hidden units

train_val_data <- wine[1:1000, ]
test_data <- wine[1001:1599, ]

# Within training+validation: training and validation sets for decay tuning
train_decay_data <- wine[1:800, ]
val_decay_data <- wine[801:1000, ]

# Extract predictors and targets for these sets
X_train_val <- as.matrix(train_val_data[, -ncol(train_val_data)])
y_train_val <- train_val_data[, "quality"]

X_test <- as.matrix(test_data[, -ncol(test_data)])
y_test <- test_data[, "quality"]

X_train_decay <- as.matrix(train_decay_data[, -ncol(train_decay_data)])
y_train_decay <- train_decay_data[, "quality"]

X_val_decay <- as.matrix(val_decay_data[, -ncol(val_decay_data)])
y_val_decay <- val_decay_data[, "quality"]

X_train_scaled <- scale(X_train_val)
X_test_scaled <- scale(X_test)
X_train_decay_scaled <- scale(X_train_decay)
X_val_decay_scaled <- X_val_decay

decay_values <- seq(0.001, 1, length.out = 20)
loglikelihood_vals <- rep(NA, length(decay_values))

# Loop over decay values to tune on validation set
for(i in seq_along(decay_values)) {
  nn_fit <- nnet(X_train_decay_scaled, y_train_decay, size = 50, decay = decay_values[i], linout = FALSE, maxit = 5000)
  preds <- predict(nn_fit, newdata = X_val_decay_scaled)
  loglikelihood_vals[i] <- mean(y_val_decay * log(preds) + (1 - y_val_decay) * log(1 - preds))
}

plot(decay_values, loglikelihood_vals, type = "l", xlab = "Decay Parameter", ylab = "Average Log-Likelihood")

best_decay <- decay_values[which.max(loglikelihood_vals)]
cat("Optimal decay parameter:", best_decay, "\n")

# Train final model on validation data with optimal decay
final_nn_model <- nnet(X_test, y_test, size = 50, decay = best_decay, linout = FALSE, maxit = 5000)

# Predict probabilities on test set
final_probs <- predict(final_nn_model, newdata = test_data, type = "raw")

accuracy_calc <- function(pred_probs, true_labels) {
  pred_labels <- ifelse(pred_probs > 0.5, 1, 0)
  mean(pred_labels == true_labels)
}

final_accuracy <- accuracy_calc(final_probs, y_test)
cat("Neural network accuracy on test data:", final_accuracy, "\n")

# Exercise 3: Additive NP logistic model

# Fit GAM using all regressors
est_gam <- gam(quality ~ s(fixed.acidity) + s(volatile.acidity) + s(citric.acid) +
                 s(residual.sugar) + s(chlorides) + s(free.sulfur.dioxide) +
                 s(total.sulfur.dioxide) + s(density) + s(pH) + s(sulphates) + s(alcohol),
               data = train_val_data, family = binomial)

# Predict probabilities on test set
final_probs_gam <- predict(est_gam, newdata = test_data, type = "raw")

final_accuracy_gam <- accuracy_calc(final_probs_gam, y_test)
cat("Additive NP logistic model accuracy on test data:", final_accuracy_gam, "\n")

misclassification_gam <- 1 - final_accuracy_gam
cat("Additive NP logistic model misclassification rate on test data:", misclassification_gam, "\n")

