#Part a
#Read all data
a_all <- read.csv("stockData.csv", sep=",", header=TRUE) 

# Use 5 year data to train 
a <- a_all[1:60,]

#Convert adjusted close prices into returns:
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)] # return of stocks + market
#r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] # return of market

n_stocks = 30

covmat <- var(r)
beta <- covmat[1,-1]/ covmat[1,1]

rrr <- r[,-c(1,which(beta<0)+1)] 


beta <- rep(0,ncol(rrr))
alpha <- rep(0,ncol(rrr))
mse <- rep(0,ncol(rrr))
Ribar <- rep(0,ncol(rrr))
Ratio <- rep(0,ncol(rrr))
stock <- rep(0,ncol(rrr))

rf <- 0.002


for(i in 1:ncol(rrr)){
  q <- lm(data=rrr, formula=rrr[,i] ~ r[,1])
  beta[i] <- q$coefficients[2] 
  alpha[i] <- q$coefficients[1] 
  mse[i] <- summary(q)$sigma^2
  Ribar[i] <- q$coefficients[1]+q$coefficients[2]*mean(r[,1])
  Ratio[i] <- (Ribar[i]-rf)/beta[i]
  stock[i] <- i
}

xx <- (cbind(stock,alpha, beta, Ribar, mse, Ratio))

A <- xx[order(-xx[,6]),]

col1 <- rep(0,nrow(A))
col2 <- rep(0,nrow(A))
col3 <- rep(0,nrow(A))
col4 <- rep(0,nrow(A))
col5 <- rep(0,nrow(A))

col1 <- (A[,4]-rf)*A[,3]/A[,5]
col3 <- A[,3]^2/A[,5]
for(i in(1:nrow(A))) {
  col2[i] <- sum(col1[1:i])
  col4[i] <- sum(col3[1:i])
}

#Compute the Ci (col5):
for(i in (1:nrow(A))) {
  col5[i] <- var(r[,1])*col2[i]/(1+var(r[,1])*col4[i])
}

#SHORT SALES ALLOWED:
#Compute the zi:
z_short <- (A[,3]/A[,5])*(A[,6]-col5[nrow(A)])
#Compute the xi:
x_short <- z_short/sum(z_short)

#The final table when short sales allowed:
Weights_with_short <- cbind(A, col1, col2, col3, col4, col5, z_short, x_short)


#SHORT SALES NOT ALLOWED:
#First create a matrix up to the maximum of col5:
table1 <- cbind(A, col1, col2, col3, col4, col5)
table2 <- table1[1:which(col5==max(col5)), ]

#Compute the zi:
z_no_short <- (table2[,3]/table2[,5])*(table2[,6]-max(col5))

#Compute the xi:
x_no_short <- z_no_short/sum(z_no_short)

#The final table when short sales are not allowed:
Weights_no_short <- cbind(table2, z_no_short, x_no_short)

#Part b


#find the return of the portfolio with short sales allowed

R_p_short <- Weights_with_short[,13] %*% Weights_with_short[,4]

covariance_matrix = matrix(0,n_stocks,n_stocks)
var_Rm = var(r[,1])
for (i in 1:n_stocks)
{
  for(j in 1:n_stocks){
    if(i == j)
    {
      covariance_matrix[i,j] = Weights_with_short[i,3] * Weights_with_short[i,3] * var_Rm + Weights_with_short[i,5]
    }else{
      covariance_matrix[i,j] = Weights_with_short[i,3] * Weights_with_short[j,3] * var_Rm 
    }
  }
}

#find the risk of the portfolio with short sales allowed

var_p_short <- Weights_with_short[,13] %*% covariance_matrix %*% Weights_with_short[,13]

#find the return of the portfolio with no short sales allowed
n_long = nrow(Weights_no_short)
R_p_no_short <- Weights_no_short[1:n_long,13] %*% Weights_no_short[1:n_long,4]
#find the risk of the portfolio with no short sales allowed
var_p_no_short <- Weights_no_short[1:n_long,13] %*% covariance_matrix[1:n_long,1:n_long]  %*% Weights_no_short[1:n_long,13]

#Trace out efficient portfolio
inv_covmat_single_index = solve(covariance_matrix)
ones = rep(1,n_stocks)

A = as.numeric(t(Weights_with_short[,4]) %*% inv_covmat_single_index %*% ones) 
B = as.numeric(t(Weights_with_short[,4]) %*% inv_covmat_single_index %*% Weights_with_short[,4]) 
C = as.numeric(t(ones)  %*% inv_covmat_single_index %*% ones) 
D = B*C - A^2

E <- seq(-0.2,0.2,.001)

sigmas <- sqrt(seq(1/C,0.03,.0001)) 
upper_part <- (A + sqrt(D*(C*sigmas^2 - 1)))*(1/C) 
lower_part <- (A - sqrt(D*(C*sigmas^2 - 1)))*(1/C) 

plot(sigmas, upper_part, lwd=5,type = "l",col = "green",xlim = c(0, 0.2), ylim= c(-0.2,0.2),ylab = "Return") 
lines(sigmas,lower_part, lwd=5,type = "l",col = "green")

points(sqrt(var_p_short),R_p_short, pch=19,lwd=1,col="blue")
points(sqrt(var_p_no_short),R_p_no_short, pch=19,lwd=1,col="gold")


points(sqrt(diag(covariance_matrix)),Weights_with_short[,4], pch=19,lwd=1,col="black")
points(sqrt(var(r[,1])),mean(r[,1]), pch=19,lwd=1,col="red")

legend("topright", 
       legend=c("Efficient Portfolio","Short Sales Allowed", "Short Sales Not Allowed", "Stocks","Market"),
       col=c("green","blue","gold","black","red"),
       pch = 19,
       fill =c("green","blue","gold","black","red"),
       cex=0.8)

#Part c


Rfr <- seq(-0.2,.007,0.001)

#Initialize the two vectors:
rbar_opt <- rep(0,length(Rfr))
risk_opt <- rep(0,length(Rfr))


for(l in 1:length(Rfr)){
  #Risk free asset:
  rf <- Rfr[l]
  #rf <- .002
  #Initialize
  beta <- rep(0,ncol(rrr))
  alpha <- rep(0,ncol(rrr))
  mse <- rep(0,ncol(rrr))
  Ribar <- rep(0,ncol(rrr))
  Ratio <- rep(0,ncol(rrr))
  stocknum <- rep(0,ncol(rrr))
  #stock <- names(rrr)
  
  #This for loop computes the required inputs:
  for(i in 1:ncol(rrr)){
    q <- lm(data=rrr, formula=rrr[,i] ~ r[,1])
    beta[i] <- q$coefficients[2] 
    alpha[i] <- q$coefficients[1] 
    mse[i] <- summary(q)$sigma^2
    Ribar[i] <- q$coefficients[1]+q$coefficients[2]*mean(r[,1])
    Ratio[i] <- (Ribar[i]-rf)/beta[i]
    stocknum[i] <- i
  }
  
  #So far we have this table:
  #xx <- (cbind(stock,alpha, beta, Ribar, mse, Ratio))
  xx <- (data.frame(stocknum,alpha, beta, Ribar, mse, Ratio))
  
  
  #Order the table based on the excess return to beta ratio:
  A <- xx[order(-xx[,6]),]
  
  col1 <- rep(0,nrow(A))
  col2 <- rep(0,nrow(A))
  col3 <- rep(0,nrow(A))
  col4 <- rep(0,nrow(A))
  col5 <- rep(0,nrow(A))
  
  
  #Create the last 5 columns of the table:
  col1 <- (A[,4]-rf)*A[,3]/A[,5]
  col3 <- A[,3]^2/A[,5]
  for(i in(1:nrow(A))) {
    col2[i] <- sum(col1[1:i])
    col4[i] <- sum(col3[1:i])
  }
  
  #So far we have:
  cbind(A, col1, col2, col3, col4)
  
  
  #Compute the Ci (col5):
  for(i in (1:nrow(A))) {
    col5[i] <- var(r[,1])*col2[i]/(1+var(r[,1])*col4[i])
  }
  
  
  #The final table when short sales allowed:
  B <- cbind(A, col1, col2, col3, col4, col5)
  rownames(B) <- NULL
  
  #SHORT SALES NOT ALLOWED:
  #First create a matrix up to the maximum of col5:
  #table1 <- cbind(A, col1, col2, col3, col4, col5)
  #table2 <- (B[1:which(col5==max(col5)), ], nrow=which(col5==max(col5)), ncol=ncol(B))
  table2 <- B[1:which(col5==max(col5)), ]
  
  #Compute the Zi:
  z_no_short <- (table2[,3]/table2[,5])*(table2[,6]-max(col5))
  
  #Compute the xi:
  x_no_short <- z_no_short/sum(z_no_short)
  
  #Compute the mean and variance for each portfolio when short sales not allowed:
  #First match the columns of the data with the composition of the portfolio:
  
  r1 <- data.frame(rrr[,table2[,1]])
  
  beta1 <- rep(0,ncol(r1))
  sigma_e1 <- rep(0,ncol(r1))
  alpha1 <- rep(0,ncol(r1))
  
  for(i in 1:ncol(r1)){
    q1<- lm(r1[,i] ~ r[,1])
    beta1[i] <- q1$coefficients[2] 
    sigma_e1[i] <- summary(q1)$sigma^2
    alpha1[i] <- q1$coefficients[1] 
  } 
  
  means1 <- colMeans(r1)
  #means1 <- alpha1 + beta1*mean(r[,1])
  
  
  #Construct the variance covariance matrix using SIM:
  xx <- rep(0,ncol(r1)*(ncol(r1)))             #Initialize
  varcovar <- matrix(xx,nrow=ncol(r1),ncol=ncol(r1))  #the variance covariance matrix 
  
  
  for (i in 1:ncol(r1)){
    for (j in 1:ncol(r1)){
      varcovar[i,j]=beta1[i]*beta1[j]*var(r[,1])
      if(i==j){varcovar[i,j]=beta1[i]^2*var(r[,1])+ sigma_e1[i]}
    }
  }
  
  
  rbar_opt[l] <- t(x_no_short) %*% means1
  risk_opt[l] <- ( t(x_no_short) %*% varcovar %*% x_no_short )^.5
}

plot(risk_opt, rbar_opt, type="l", main="Efficient frontier when short sales not allowed", ylab="Portfolio expected return", xlab="Portfolio standard deviation",col = "green",
     xlim = c(0,0.2),ylim = c(0,0.075))

points(sqrt(var_p_no_short),R_p_no_short, pch=19,lwd=1,col="gold")

points(sqrt(diag(covariance_matrix)),Weights_with_short[,4], pch=19,lwd=1,col="black")
points(sqrt(var(r[,1])),mean(r[,1]), pch=19,lwd=1,col="red")

legend("topright", 
       legend=c("Efficient Portfolio / No S.S", "Short Sales Not Allowed Rf = 0.002", "Stocks","Market"),
       col=c("green","gold","black","red"),
       pch = 19,
       fill =c("green","gold","black","red"),
       cex=0.8)
