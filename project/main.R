options(error = function() traceback(3))

library(glmnet)
library(glmnetUtils)

npf <- read.csv("npf_train.csv")
npf_test <- read.csv("npf_test_hidden.csv")

# Remove columns "id", "date" and "partlybad" and set dates as rownames
rownames(npf) <- npf[, "date"]
npf <- npf[, -c((1:2), 4)]

# Create variable class2 where the value is "event" if there was an NPF event
# and "nonevent" otherwise.
npf$class2 <- factor("event", levels = c("nonevent", "event"))
npf$class2[npf$class4 == "nonevent"] <- "nonevent"

npf_test$class2 <- factor("event", levels = c("nonevent", "event"))
npf_test$class2[npf_test$class4 == "nonevent"] <- "nonevent"

# Remove variable class4 because predicting will be done on variable class2
npf <- npf[, -1]

# Some helpful functions
accuracy <- function(real, pred) {
    sum(as.numeric(real == pred)) / length(pred)
}

# Create a linear model
model <- glmnet(class2 ~ ., npf, family = "binomial", alpha = 1)

# Predict class2 on test data
predicted <- predict(model, newdata = npf, type = "response")
predicted_classes <- c("nonevent", "event")[1 + (predicted > 0.5)]


acc <- accuracy(npf$class2, predicted_classes)


write.table(acc, "answers.csv", row.names=F, quote=F, col.names=F)