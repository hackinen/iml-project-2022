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

coefficients(model)

# Create variable class2 where the value is "event" if there was an NPF event
# and "nonevent" otherwise.
npf$class2 <- factor("event", levels = c("nonevent", "event"))
npf$class2[npf$class4 == "nonevent"] <- "nonevent"

# Predict class4 on training data
predicted <- predict(model, newdata = npf, type = "response")[,,1]

predicted_class4 <- c()
predicted_class2 <- c()
predicted_probs <- c()
for (i in 1:length(predicted[, 1])) {
    pred_class <- colnames(predicted)[which.max(predicted[i, ])]
    predicted_class4 <- c(predicted_class4, pred_class)
    event_prob <- 1 - predicted[i, 4]
    predicted_probs <- c(predicted_probs, event_prob)
    predicted_class2 <- c(predicted_class2, c("nonevent", "event")[1 + (event_prob > 0.5)])
}

acc <- accuracy(npf$class4, predicted_class4)

cat("Multiclass accuracy: ")
cat(acc)
cat("\n")


binary_acc <- accuracy(npf$class2, predicted_class2)

cat("Binary accuracy: ")
cat(binary_acc)
cat("\n")

# Predict class4 on test data
predicted <- predict(model, newdata = npf_test, type = "response")[,,1]

predicted_class4 <- c()
predicted_class2 <- c()
predicted_probs <- c()
for (i in 1:length(predicted[, 1])) {
    pred_class <- colnames(predicted)[which.max(predicted[i, ])]
    predicted_class4 <- c(predicted_class4, pred_class)
    event_prob <- 1 - predicted[i, 4]
    predicted_probs <- c(predicted_probs, event_prob)
    predicted_class2 <- c(predicted_class2, c("nonevent", "event")[1 + (event_prob > 0.5)])
}


write.csv(data.frame(class4=predicted_class4, p=predicted_probs), "answers.csv", row.names=F, quote=F)