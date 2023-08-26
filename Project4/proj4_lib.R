a_all <- read.csv("stockData.csv", sep=",", header=TRUE) 
# Use 5 year data to train 
a <- a_all[1:60,] 
#Convert adjusted close prices into returns:
r <- (a[-1,4:ncol(a)]-a[-nrow(a),4:ncol(a)])/a[-nrow(a),4:ncol(a)] # return of stocks
r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] # return of market
n_stocks = 30


mean_Rm = mean(r_m)
var_Rm <- var(r_m)
stdev_Rm <- var_Rm^.5

mean_Ri = colMeans(r)

betas = rep(0,n_stocks)
alphas = rep(0,n_stocks)
var_es = rep(0,n_stocks)
var_betas = rep(0,n_stocks)

for (i in 1:n_stocks){
  fit <- lm(r[,i] ~ r_m)
  betas[i] = fit$coefficients[2]
  alphas[i] = fit$coefficients[1]
  var_es[i] = sum(fit$residuals^2)/ (nrow(r) - 2)
  var_betas[i] = vcov(fit)[2,2]
}

#find beta_i
print("Betas are:")
print(betas)
print("Number of negative betas")
print(sum(which(betas < 0)))

#find alpha_i
print("Alphas are:")
print(alphas)

#Compute covariance matrix using single index model
covariance_matrix = matrix(0,n_stocks,n_stocks)
for (i in 1:n_stocks)
{
  for(j in 1:n_stocks){
    if(i == j)
    {
      covariance_matrix[i,j] = betas[i] * betas[i] * var_Rm + var_es[i]
    }else{
      covariance_matrix[i,j] = betas[i] * betas[j] * var_Rm
    }
  }
}
print("Single Index Model cov_mat")
print(covariance_matrix)


#Project 2 part e replicated:

n_stocks = 30 
means <- colMeans(r)
covmat <- cov(r)
cormat <- cor(r)
variances <- diag(covmat)
stdev <- diag(covmat)^.5
inv_covmat <- solve(covmat)
ones = rep(1,n_stocks)

A = as.numeric(t(means) %*% inv_covmat %*% ones) 
B = as.numeric(t(means) %*% inv_covmat %*% means) 
C = as.numeric(t(ones)  %*% inv_covmat %*% ones) 
D = B*C - A^2

E <- seq(-0.2,0.2,.001)

sigmas <- sqrt(seq(1/C,0.03,.0001)) 
upper_part <- (A + sqrt(D*(C*sigmas^2 - 1)))*(1/C) 
lower_part <- (A - sqrt(D*(C*sigmas^2 - 1)))*(1/C) 

plot(sigmas, upper_part, lwd=5,type = "l",ylab = 'E', xlab = expression(sigma),ylim = c(-0.15,0.15),main="Risk-Return Plot") 
lines(sigmas,lower_part, lwd=5,type = "l")

#Finding efficient frontier using the single index model:

mean_Ri = colMeans(r)
inv_covmat_single_index = solve(covariance_matrix)
ones = rep(1,n_stocks)

A = as.numeric(t(mean_Ri) %*% inv_covmat_single_index %*% ones) 
B = as.numeric(t(mean_Ri) %*% inv_covmat_single_index %*% mean_Ri) 
C = as.numeric(t(ones)  %*% inv_covmat_single_index %*% ones) 
D = B*C - A^2

E <- seq(-0.2,0.2,.001)

sigmas <- sqrt(seq(1/C,0.03,.0001)) 
upper_part <- (A + sqrt(D*(C*sigmas^2 - 1)))*(1/C) 
lower_part <- (A - sqrt(D*(C*sigmas^2 - 1)))*(1/C) 

lines(sigmas, upper_part, lwd=5,type = "l",col = "green") 
lines(sigmas,lower_part, lwd=5,type = "l",col = "green")
legend("topright", 
       legend=c("Historical Var-Cov", "Single index model var-cov"),
       col=c("black", "green"),
       pch = 19,
       fill =c("black", "green"),
       cex=0.8)