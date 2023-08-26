#Read all data
a_all <- read.csv("stockData.csv", sep=",", header=TRUE) 

# Use 5 year data to train 
a <- a_all[1:60,]

#Convert adjusted close prices into returns:
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)] # return of stocks + market
#r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] # return of market
rrr <- r[,-1]

n_stocks= ncol(rrr)

#Compute the average correlation:
rho <- (sum(cor(rrr[1:n_stocks]))-n_stocks)/(n_stocks*(n_stocks-1))

#Initialize the vectors:
col1 <- rep(0,n_stocks)
col2 <- rep(0,n_stocks)
col3 <- rep(0,n_stocks)

#Initialize the var-covar matrix:
y <- rep(0,n_stocks*n_stocks)
mat <- matrix(y, ncol=n_stocks, nrow=n_stocks)

#Compute necessary quantities:
R_f = 0.002
Rbar <- colMeans(rrr[1:n_stocks])
Rbar_f <- Rbar-R_f
sigma <- ( diag(var(rrr[1:n_stocks])) )^0.5
Ratio <- Rbar_f/sigma

#Initial table:
xx <- (cbind(Rbar, Rbar_f, sigma, Ratio))

#Order the table based on the excess return to sigma ratio:
aaa <- xx[order(-Ratio),]

#Create the last 3 columns of the table:
for(i in(1:n_stocks)) {
  
  col1[i] <- rho/(1-rho+i*rho)
  
  col2[i] <- sum(aaa[,4][1:i])
}

#Compute the Ci:
for(i in (1:n_stocks)) {
  
  col3[i] <- col1[i]*col2[i]
  
}

#Create the entire table until now:
xxx <- cbind(aaa, col1, col2, col3)


#SHORT SALES ALLOWED:
#Compute the Zi:
z <- (1/((1-rho)*xxx[,3]))*(xxx[,4]-xxx[,7][nrow(xxx)])

#Compute the xi:
x <- z/sum(z)

#The final table:
aaaa <- cbind(xxx, z, x)

#SHORT SALES NOT ALLOWED:
#Find composition of optimum portfolio when short sales are not allowed:
aaaaa <- aaaa[1:which(aaaa[,7]==max(aaaa[,7])), ]
z_no <- (1/((1-rho)*aaaaa[,3]))*(aaaaa[,4]-aaaaa[,7][nrow(aaaaa)])
x_no <- z_no/sum(z_no)

#Final table:
a_no <- cbind(aaaaa, z_no, x_no)

#Var-covar matrix based on the constant correlation model:
for(i in 1:30){
  
  for(j in 1:30){
    
    if(i==j){
      mat[i,j]=aaaa[i,3]^2
    } else
    {
      mat[i,j]=rho*aaaa[i,3]*aaaa[j,3]
    }
  }
}


#Calculate the expected return and sd of the point of tangency 
#when short sales allowed
sd_p_opt <- (t(x) %*% mat %*% x)^.5
R_p_opt <- t(x) %*% aaaa[,1]


#Calculate the expected return and sd of the point of tangency 
#when short sales are not allowed
sd_p_opt_no <- (t(x_no) %*% mat[1:which(aaaa[,7]==max(aaaa[,7])),1:which(aaaa[,7]==max(aaaa[,7]))] %*% x_no)^.5
R_p_opt_no <- t(x_no) %*% aaaaa[,1]

#Trace out efficient portfolio
inv_covmat_const_corr= solve(mat)
ones = rep(1,n_stocks)

A = as.numeric(t(aaaa[,1]) %*% inv_covmat_const_corr %*% ones) 
B = as.numeric(t(aaaa[,1]) %*% inv_covmat_const_corr %*% aaaa[,1]) 
C = as.numeric(t(ones)  %*% inv_covmat_const_corr %*% ones) 
D = B*C - A^2

E <- seq(-0.2,0.2,.001)

sigmas <- sqrt(seq(1/C,0.03,.0001)) 
upper_part <- (A + sqrt(D*(C*sigmas^2 - 1)))*(1/C) 
lower_part <- (A - sqrt(D*(C*sigmas^2 - 1)))*(1/C) 

plot(sigmas, upper_part, lwd=5,type = "l",col = "green",xlim = c(0, 0.2), ylim= c(-0.2,0.2),ylab = "Return",xlab = "Sigma") 
lines(sigmas,lower_part, lwd=5,type = "l",col = "green")

points(sd_p_opt,R_p_opt, pch=19,lwd=1,col="blue")
points(sd_p_opt_no,R_p_opt_no, pch=19,lwd=1,col="gold")
points(aaaa[,3],aaaa[,1], pch=19,lwd=1,col="black")
points(sqrt(var(r[,1])),mean(r[,1]), pch=19,lwd=1,col="red")

legend("topright", 
       legend=c("Efficient Portfolio const corr","Short Sales Allowed", "Short Sales Not Allowed", "Stocks","Market"),
       col=c("green","blue","gold","black","red"),
       pch = 19,
       fill =c("green","blue","gold","black","red"),
       cex=0.8)
