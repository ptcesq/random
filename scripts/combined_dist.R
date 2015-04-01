
# non-gussuain 

group_1 <- rbinom(500000, 1, 0.0001)
group_2 <- rbinom(100000, 1, 0.66)
group_3 <- rep(0, 400000)
group_4 <- rbinom(100000, 1, 0.85)
group_5 <- rbinom(400000, 1, 0.001)

y <- c(group_1, group_2, group_3, group_4, group_5)
actual_percent = sum(y)/length(y)

result <- data.frame(iteration= numeric(), 
                     err=numeric(),
                     group=numeric())
for (i in 1:2500) {
  sample_1 <- sample(y, i)
  sample_percent <- sum(sample_1)/i
  err <- abs(actual_percent - sample_percent)/actual_percent
  gp=1
  result <- rbind(result, c(i, err, gp))  
}
colnames(result) <- c("n", "err", "group")

# Non-Random Selection

counts <- seq(1,2500)

for (i in counts) {
  sample_1 <- y[1:i]
  sample_percent <- sum(sample_1)/i
  err <- abs(actual_percent - sample_percent)/actual_percent
  gp=4
  result <- rbind(result, c(i, err, gp))  
}

library(ggplot2)
p <- qplot(n,err, data=result, geom="smooth", color=gp, ylim=c(0.00, 1.0), 
           main="Relative Error for Ramdom Sample with Non-Gausian Distribution", 
           ylab="Error", xlab="Sample Size")  

p

