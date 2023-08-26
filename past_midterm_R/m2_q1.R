#Midterm 2 2022, Q1

s1 <- seq(400,600,1)

p1 <- 24
p2 <- 68
p3 <- 115

E1 <- 450
E2 <- 500
E3 <- 550

profit_p1 <- ifelse(s1<E1, E1-s1-p1, -p1)
profit_p2 <- 2*ifelse(s1<E2, s1-E2+p2, p2)
profit_p3 <- ifelse(s1<E3, E3-s1-p3, -p3)


total <- profit_p1 + profit_p2 + profit_p3

plot(s1, total, type="l",ylim=c(-150,150),main = "Q1")
points(s1, profit_p1, type="l",ylim=c(-150,150),main = "Q1",col = "red")
points(s1, profit_p2, type="l",ylim=c(-150,150),main = "Q1",col = "green")
points(s1, profit_p3, type="l",ylim=c(-150,150),main = "Q1",col = "blue")
