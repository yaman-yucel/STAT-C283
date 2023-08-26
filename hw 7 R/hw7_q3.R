
#Q3

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the call:
c <- 4

#Exercise price:
E <- 50

#Profit from selling the call:
profit_call <- ifelse(s1>E, E-s1+c, c)

#Total profit:
total <- profit_call #+ profit_stock

#Plot of the profit from selling the call:
plot(s1, profit_call, type="l",ylim=c(-50,50),main = "Q3")

#Q4

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the put:
p <- 3

#Exercise price:
E <- 40

#Profit from selling the call:
profit_put <- ifelse(s1<E, E-s1-p, -p)

#Total profit:
total <- profit_put #+ profit_stock

#Plot of the profit from selling the call:
plot(s1, profit_put, type="l",ylim=c(-50,50),main = "Q4")


#Q5

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the put:
p <- 6
c <- 5
#Exercise price:
E <- 50

#Profit from selling the call:
profit_put1 <- ifelse(s1<E, E-s1-p, -p)
profit_put2 <- ifelse(s1<E, E-s1-p, -p)
profit_call <- ifelse(s1>E, s1-E - c, -c)

a <- profit_put1 + profit_put2
b <- profit_call
c <- a + b


#Plot of the profit from selling the call:
plot(s1, a, type="l",ylim=c(-100,100),col = "red",ylab= "profit",main = "Q5")
points(s1, b, type="l",ylim=c(-100,100),col = "blue")
points(s1, c, type="l",ylim=c(-100,100),col = "green")

legend("topright", 
       legend=c("a","b","c"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#Q6

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the call:
c1 <- 5
c2 <- 8
#Exercise price:
E1 <- 45
E2 <- 40
#Profit
profit_call1 <- 2 * ifelse(s1>E1, E1-s1+c1, c1)
profit_call2 <- ifelse(s1>E2, s1-E2-c2, -c2)
#Total profit:
total <- profit_call1 + profit_call2 #+ profit_stock

#Plot of the profit from selling the call:
plot(s1, profit_call1, type="l",ylim=c(-50,50),col = "red",ylab= "profit",main = "Q6")
points(s1, profit_call2, type="l",col = "blue")
points(s1,total,type = "l",col = "green")

legend("topright", 
       legend=c("Write 2 calls"," Buy 1 call","Total Profit"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#Q7


#Stock price at t=0:
s0 <- 40

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the put:
p <- 5

#Exercise price:
E <- 50

#Profit from buying the stock:
profit_stock <- s1-s0

#Profit from buy put:
profit_call <- ifelse(s1<E, E-s1-p, -p)
#Total profit:
total <- profit_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, profit_call, type="l", ylim=c(-40,40),col ="red",ylab = "profit",main = "Q7.1")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Long put profit","Long stock profit","Total Profit"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#payoff from buy put:
payoff_call <- ifelse(s1<E, E-s1,0)
#Total profit:
total <- payoff_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, payoff_call, type="l", ylim=c(-40,40),col ="red",ylab = "payoff",main = "Q7.1")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Long put payoff","Long stock payoff","Total payoff"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)



#Stock price at t=0:
s0 <- 40

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the put:
p <- 5

#Exercise price:
E <- 50

#Profit from shorting the stock:
profit_stock <- -(s1-s0)

#Profit from sell put:
profit_put <- ifelse(s1<E, s1-E+p, p)
#Total profit:
total <- profit_put + profit_stock
#Plot of the profit from selling the call:
plot(s1, profit_put, type="l", ylim=c(-40,40),col ="red",ylab = "profit",main = "Q7.2")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Short put profit","Short stock profit","Total Profit"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#payoff from buy put:
payoff_put <- ifelse(s1<E, s1-E,0)
#Total profit:
total <- payoff_put + profit_stock
#Plot of the profit from selling the call:
plot(s1, payoff_put, type="l", ylim=c(-40,40),col ="red",ylab = "payoff",main = "Q7.2")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Short put payoff","Short stock payoff","Total payoff"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)


#Stock price at t=0:
s0 <- 40

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the call:
c <- 5

#Exercise price:
E <- 50

#Profit from shorting the stock:
profit_stock <- -(s1-s0)

#Profit from buy call:
profit_call <- ifelse(s1>E, s1-E-c, -c)
#Total profit:
total <- profit_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, profit_call, type="l", ylim=c(-40,40),col ="red",ylab = "profit",main = "Q7.3")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Long call profit","Short stock profit","Total Profit"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#payoff from buy call:
payoff_call <- ifelse(s1>E, s1-E,0)
#Total profit:
total <- payoff_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, payoff_call, type="l", ylim=c(-40,40),col ="red",ylab = "payoff",main = "Q7.3")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Long call payoff","Short stock payoff","Total payoff"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#Stock price at t=0:
s0 <- 40

#Hypothetical prices of the stock at expiration:
s1 <- seq(1,100,1)

#Price of the call:
c <- 5

#Exercise price:
E <- 50

#Profit from long the stock:
profit_stock <- (s1-s0)

#Profit from sell call:
profit_call <- ifelse(s1>E, E-s1+c, c)
#Total profit:
total <- profit_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, profit_call, type="l", ylim=c(-40,40),col ="red",ylab = "profit",main = "Q7.4")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Short call profit","Long stock profit","Total Profit"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)

#payoff from buy call:
payoff_call <- ifelse(s1>E, E-s1,0)
#Total profit:
total <- payoff_call + profit_stock
#Plot of the profit from selling the call:
plot(s1, payoff_call, type="l", ylim=c(-40,40),col ="red",ylab = "payoff",main = "Q7.4")
#Add the profit from buying the stock:
points(s1, profit_stock,type="l", col="blue" )
#The total profit:
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Short call payoff","Long stock payoff","Total payoff"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)
#Q8
s1 <- seq(1,100,1)

E1 = 50
E2 = 60



payoff_buy_call_E1 <- ifelse(s1>E1, s1-E1,0)
payoff_sell_call_E2 <- ifelse(s1>E2, E2-s1,0)
payoff_bull = payoff_buy_call_E1 + payoff_sell_call_E2

payoff_buy_put_E2 <- ifelse(s1<E2, E2-s1,0)
payoff_sell_put_E1 <- ifelse(s1<E1, s1-E1,0)
payoff_bear = payoff_buy_put_E2 + payoff_sell_put_E1

plot(s1, payoff_bull, type="l", ylim=c(-40,40),col ="red",ylab = "payoff",main = "Q8")
points(s1, payoff_bear,type="l", col="blue" )

total = payoff_bull + payoff_bear
points(s1,total,type="l", col="green" )

legend("bottomright", 
       legend=c("Bull payoff","Bear payoff","Total payoff"),
       col=c("red","blue","green"),
       pch = 19,
       fill =c("red","blue","green"),
       cex=0.8)
#Q9
s1 <- seq(1,100,1)

E1 = 50
E2 = 60

payoff_buy_put_E2 <- ifelse(s1<E2, E2-s1,0)
payoff_sell_put_E1 <- ifelse(s1<E1, s1-E1,0)
payoff_bear = payoff_buy_put_E2 + payoff_sell_put_E1

plot(s1, payoff_bear, type="l", ylim=c(-40,40),col ="red",ylab = "payoff bear",main = "Q9")

p1 = 3
p2 = 5
profit_buy_put_E2 <- ifelse(s1<E2, E2-s1 -p1,-p1)
profit_sell_put_E1 <- ifelse(s1<E1, s1-E1+p2,p2)
profit_bear = profit_buy_put_E2 + profit_sell_put_E1

plot(s1, profit_bear, type="l", ylim=c(-40,40),col ="red",ylab = "profit bear",main = "Q9")

#Q10

s1 <- seq(1,100,1)

E1 = 50
E2 = 60

payoff_sell_call_E1 <- ifelse(s1>E1, E1-s1,0)
payoff_buy_call_E2 <- ifelse(s1>E2, s1-E2,0)
payoff_bear = payoff_sell_call_E1 + payoff_buy_call_E2

plot(s1, payoff_bear, type="l", ylim=c(-40,40),col ="red",ylab = "payoff bear",main = "Q10")

c1 = 5
c2 = 3

profit_sell_call_E1 <- ifelse(s1>E1, E1-s1 + c1,c1)
profit_buy_call_E2 <- ifelse(s1>E2, s1-E2-c2,-c2)
profit_bear = profit_sell_call_E1 + profit_buy_call_E2

plot(s1, profit_bear, type="l", ylim=c(-40,40),col ="red",ylab = "profit bear",main = "Q10")