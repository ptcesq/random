
n <- 1500000

y <- rbinom(n, 1, 0.1007)
actual_percent = sum(y)/n

result <- data.frame(iteration= numeric(), 
                     err=numeric())
for (i in 1:2500) {
  sample_1 <- sample(y, i)
  sample_percent <- sum(sample_1)/i
  err <- abs(actual_percent - sample_percent)/actual_percent
  result <- rbind(result, c(i,err))  
}
colnames(result) <- c("n", "err")


library(ggplot2)
p <- qplot(n,err, data=result, geom="smooth", 
           main="Relative Error for Ramdom Sample with Gausian Distribution", 
           ylab="Error", xlab="Sample Size")  
     
p


