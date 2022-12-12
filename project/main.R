options(error = function() traceback(3))

library(glmnet)
library(glmnetUtils)

npf <- read.csv("npf_train.csv")
npf_test <- read.csv("npf_test_hidden.csv")

# Remove columns "id", "date" and "partlybad" and set dates as rownames
rownames(npf) <- npf[, "date"]
npf <- npf[, -c((1:2), 4)]


# Some helpful functions
accuracy <- function(real, pred) {
    sum(as.numeric(real == pred)) / length(pred)
}

# Create a linear model
model <- glmnet(class4 ~ ., npf, family = "multinomial", alpha = 0, lambda = 0.01)

# Predict class4 on training data
predicted <- predict(model, newdata = npf, type = "response")[,,1]

predicted_class4 <- c()
predicted_probs <- c()
for (i in 1:length(predicted[, 1])) {
    pred_class <- colnames(predicted)[which.max(predicted[i, ])]
    predicted_class4 <- c(predicted_class4, pred_class)
    event_prob <- 1 - predicted[i, 4]
    predicted_probs <- c(predicted_probs, event_prob)
}

acc <- accuracy(npf$class4, predicted_class4)

cat("Multiclass accuracy: ")
cat(acc)
cat("\n")
