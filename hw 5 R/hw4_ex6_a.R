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

C_star = (R_p_short - rf)*sum(Weights_with_short[,3]*Weights_with_short[,13]) * var_Rm / var_p_short
print(C_star)