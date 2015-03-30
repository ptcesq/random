y <- rbinom(n, 1, 1/2)
sample_1 <- sample(y, 10)
sample_2 <- sample(y, 100)

y <- sample(1:5, 1500000, replace=T)
sample_1 <- sample(y, 1)
sample_2 <- sample(y, 20)
sample_3 <- sample(y, 50)
hist(y)
