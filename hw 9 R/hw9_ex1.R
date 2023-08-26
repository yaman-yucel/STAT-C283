epsilon <- c(0,rnorm(100))
S <- c(50,rep(0,100))
DS <- rep(0,101)
for(i in(1:100)) {
  DS[i+1] <- (0.20/52)*S[i] + 0.25*sqrt(1/52)*S[i]*epsilon[i+1]
  S[i+1] = S[i] + DS[i+1]
}
x <- seq(0,100)
xx <- as.data.frame(cbind(x, epsilon, DS, S))
plot(x, S, type="l", xlab="Periods", ylab="Stock price")
points(x,S)


#Exercise 5

s1 <- read.csv("stockData.csv", sep=",", header=TRUE)
s1 <- s1[,3]
a <- s1[-(length(s1))]/s1[-1]
u <- log(a)
m <- mean(u)
s <- sqrt(var(u))
sigma_hat = sqrt(252)*s