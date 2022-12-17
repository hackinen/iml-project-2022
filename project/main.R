options(error = function() traceback(3))

set.seed(100500)

library(glmnet)
library(glmnetUtils)

npf <- read.csv("npf_train.csv")
npf_test <- read.csv("npf_test_hidden.csv")

# Remove columns "id", "date" and "partlybad" and set dates as rownames
rownames(npf) <- npf[, "date"]
npf <- npf[, -c((1:2), 4)]


# Split training data to training and validation sets
idx <- sample.int(nrow(npf), nrow(npf) / 5)
npf_train <- npf[-idx, ]
npf_val <- npf[idx, ]


# Some helpful functions
accuracy <- function(real, pred) {
    sum(as.numeric(real == pred)) / length(pred)
}

pred <- function(mod, data) {
    predicted <- predict(mod, newdata = data, type = "response")[,,1]

    predicted_class4 <- c()

    for (i in 1:length(predicted[, 1])) {
        pred_class <- colnames(predicted)[which.max(predicted[i, ])]
        predicted_class4 <- c(predicted_class4, pred_class)
    }

    predicted_class4
}

# Create k data splits roughly equal size
kpart <- function(n, k) {
    rep_len(1:k, length.out = n)
}

crossval <- function(
               formula,
               data,
               model = lm,
               lambda = 1,
               n = nrow(data),
               k = 10, # number of cross-validation folds
               split = kpart(n, k),
               ## function to train a model on data
               train = function(data) model(formula, data = data, family = "multinomial", alpha = 0.5, lambda = lambda),
               ## function to make predictions on the trained model
               pred = function(model, data) predict(model, newdata = data, type = "response"))
{
    yhat <- NULL
    for (i in 1:k) {
        ## go through all folds, train on other folds, and make a prediction
        mod <- train(data[split != i, ])
        if (is.null(yhat)) {
            ## initialise yhat to something of correct data type,
            yhat <- pred(mod, data)
        } else {
            yhat[split == i] <- pred(mod, data[split == i, ])
        }
    }

    cv_pred <- yhat[, , 1]
    predicted_class4 <- c()
    for (i in 1:length(cv_pred[, 1])) {
        pred_class <- colnames(cv_pred)[which.max(cv_pred[i, ])]
        predicted_class4 <- c(predicted_class4, pred_class)
    }
    predicted_class4
}

# Create a linear model
models <- seq(from = 0.0045, to = 0.0175, length.out = 20)

res <- sapply(models, function(lambda) {
    model = glmnet(class4 ~ ., npf_train, family = "multinomial", alpha = 0.5, lambda = lambda)
    # Predict class4 on training data
    acc_train <- accuracy(npf_train$class4, pred(model, npf_train))

    # Predict class4 on validation data
    acc_val <- accuracy(npf_val$class4, pred(model, npf_val))

    # Let's examine our method using cross-validation
    cv_predicted <- crossval(class4 ~ ., npf, glmnet, k = 5, lambda = lambda)

    cv_acc <- accuracy(npf$class4, cv_predicted)


    c(
        lambda = lambda,
        acc_train = acc_train,
        acc_val = acc_val,
        cv_acc = cv_acc
    )
})

cat("Highest accuracy on validation data: ")
cat(max(res[3,]))
cat("\n")

cat("Highest CV accuracy: ")
cat(max(res[4,]))
cat("\n")

library(tcltk)
quartz()     #Use X11() or quartz() if on linux or mac.
plot(
    res[1,],
    res[3,],
    type = 'l',
    xlab = "Lambda",
    ylab = "Accuracy",
    ylim = c(
        min(c(res[3, ], res[4, ])),
        max(c(res[3, ], res[4, ]))
    ),
    col = "red"
)
lines(res[1,], res[4,], col = "blue")
legend(0.65, 0.715, legend=c("Training data", "Cross-validation"),
       col=c("red", "blue"), lty=c(1,1), cex=0.8)
prompt  <- "hit spacebar to close plots"
capture <- tk_messageBox(message = prompt)