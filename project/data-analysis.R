#Looking at the data and doing feature selcetion

npf <- read.csv("npf_train.csv")
npf_test <- read.csv("npf_test_hidden.csv")

# Remove columns "id", "date" and "partlybad" and set dates as rownames
rownames(npf) <- npf[, "date"]
npf <- npf[, -c((1:2), 4)]

means_of_means <- c()
means_of_sds <- c()

for (i in 2:length(npf[1,])) {
  if ((i %% 2) == 0) {
    means_of_means = c(means_of_means, mean(npf[,i]))
  } else {
    means_of_sds = c(means_of_sds, mean(npf[,i]))
  }
}

pairs((npf[, c(2,4,6,8)]))