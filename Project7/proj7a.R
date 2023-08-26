
#Read all data
a_all <- read.csv("stockData.csv", sep=",", header=TRUE) 

# Use 5 year data to train 
a <- a_all[1:60,]

#Convert adjusted close prices into returns:
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)] # return of stocks + market
#r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] # return of market
rrr <- r[,-1]

n_industries = 5
n_stock_in_industry = 6

cor_30 = cor(rrr)
var_stocks = var(rrr)
std_i_j = sqrt(diag(var_stocks))
cor_group = matrix(rep(0,n_industries*n_industries),n_industries,n_industries)
for (i in 1:n_industries){
  for (j in 1:n_industries){
    rho = 0
    
    if( i == j){
      for(k in 1:n_stock_in_industry)
      {
        for(l in 1:n_stock_in_industry)
        {
          rho = rho + cor_30[(i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l]
         #print(c((i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l))
        }
      }
      #print("NEXT")
      rho = (rho - n_stock_in_industry)/(n_stock_in_industry*(n_stock_in_industry-1))
      cor_group[i,j] = rho

    } 
    else{
      for(k in 1:n_stock_in_industry)
      {
        for(l in 1:n_stock_in_industry)
        {
          rho = rho + cor_30[(i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l]
          #print(c((i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l))
        }
      }
      # print("NEXT")
      rho = (rho/(n_stock_in_industry*n_stock_in_industry))
      cor_group[i,j] = rho
    }
  }
}
#Construct covariance matrix using the rho's
cov_matrix_group = matrix(rep(0,30*30),30,30)
for (i in 1:n_industries){
  for (j in 1:n_industries){
    for(k in 1:n_stock_in_industry){
      for(l in 1:n_stock_in_industry){
        if(i== j && k == l){
          cov_matrix_group[(i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l] = std_i_j[k] * std_i_j[l]
          }
        else{
          cov_matrix_group[(i - 1)*n_stock_in_industry + k,( j - 1)*n_stock_in_industry + l] = cor_group[i,j] * std_i_j[k] * std_i_j[l]
        }
      }
    }
  }
}
# All stocks

#Compute mean vector:
means <- colMeans(r)
#Compute variance covariance matrix
covmat <- cov(r) 
#Compute correlation matrix:
cormat <- cor(r) 
#Compute the vector of variances:
variances <- diag(covmat)
#Compute the vector of standard deviations:
stdev <- diag(covmat)^.5
#Plot the 31 assets on the space expected return against standard deviation
plot(stdev, means, 
     main="Expected Return against Standard Deviation",
     xlab="Standard Deviation", 
     ylab="Expected Return",
     xlim = c(0, 0.3),
     ylim = c(-0.25, 0.25),
     col = "black",
     pch=19)

# Equal Allocation Portfolio

new_means <- colMeans(rrr)
new_covmat <- cov(rrr) 
new_cormat <- cor(rrr) 
new_variances <- diag(new_covmat)
new_stdev <- diag(new_covmat)^.5

number_of_stocks = 30
ones_vector <- rep(1, number_of_stocks)
equal_weight_vector <- ones_vector/number_of_stocks # COMPOSITION HERE

equal_varp <- t(equal_weight_vector) %*% new_covmat %*% equal_weight_vector
equal_sdp <- sqrt(equal_varp)
equal_Rp <- t(equal_weight_vector) %*% new_means
points(equal_sdp, equal_Rp, pch = 19, lwd=1, col="red")

# Minimum Risk Portfolio

ones_vector <- rep(1, number_of_stocks)
inverse_new_covmat <- solve(new_covmat)
min_risk_weight_vector <- inverse_new_covmat  %*% ones_vector / as.numeric(t(ones_vector)  %*% inverse_new_covmat  %*% ones_vector)
#COMPOSITION MIN RISK
min_risk_varp <- t(min_risk_weight_vector) %*% new_covmat %*% min_risk_weight_vector
min_risk_sdp <- sqrt(min_risk_varp)
min_risk_Rp <- t(min_risk_weight_vector) %*% new_means

points(min_risk_sdp, min_risk_Rp, pch=19, lwd=1, col="green")

# Efficient Frontier

n_stocks = 30 
#Compute mean vector:
means <- colMeans(rrr)
#Compute variance covariance matrix 
covmat <- cov(rrr)
#Compute correlation matrix: 
cormat <- cor(rrr)
#Compute the vector of variances: 
variances <- diag(covmat)
#Compute the vector of standard deviations: 
stdev <- diag(covmat)^.5
#Compute inverse of variance covariance matrix 
inv_covmat <- solve(covmat)
#ones vector 
ones = rep(1,n_stocks)

#Compute A,B,C,D
A = as.numeric(t(means) %*% inv_covmat %*% ones) 
B = as.numeric(t(means) %*% inv_covmat %*% means) 
C = as.numeric(t(ones)  %*% inv_covmat %*% ones) 
D = B*C - A^2

#Compute lambda1 and lambda2  
E = 0.025

l1 = (function(x) (C*x - A) / D) 
l2 = (function(x) (B - A*x) / D)

lambda_1 = l1(E) 
lambda_2 = l2(E)

#Composition of the efficient portfolio given the return E
investor_weight = inv_covmat %*% (l1(E) * means + l2(E) * ones) 

#Span values of E: 
E <- seq(-0.2,0.2,.001)

#Compute variance of efficient frontier portfolios (parabola) 
var_ef_p <- (C * E^2 - 2*A*E + B) / D

#Parabola: part d 
points(sqrt(var_ef_p), E, type="l", ylab = 'E',col = "blue") # Parabola

r_gpsc = (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] 
gspc_mean = mean(r_gpsc) 
gspc_var = var(r_gpsc) 
points(sqrt(gspc_var),gspc_mean, pch = 19, lwd=1, col = "brown")

#Tangent historical Rf = 0.002

R_f = 0.002
R = means - R_f

Z = inv_covmat %*% R
x_G_historic = Z / sum(Z)

print("Composition of tangent")
print(x_G_historic)


varg_historic <- t(x_G_historic) %*% covmat %*% x_G_historic 
Rg_historic <- t(x_G_historic) %*% means 
sigmag_historic <- sqrt(varg_historic)
points(sigmag_historic,Rg_historic, pch=19,lwd=1,col="darkgreen")
abline(a = R_f, b = (Rg_historic - R_f)/sigmag_historic , lwd = 1, col = "firebrick")

#Single Index Model

r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3]
n_stocks = 30
mean_Rm = mean(r_m)
var_Rm <- var(r_m)
stdev_Rm <- var_Rm^.5

mean_Ri = colMeans(rrr)

betas = rep(0,n_stocks)
alphas = rep(0,n_stocks)
var_es = rep(0,n_stocks)
var_betas = rep(0,n_stocks)

for (i in 1:n_stocks){
  fit <- lm(rrr[,i] ~ r_m)
  betas[i] = fit$coefficients[2]
  alphas[i] = fit$coefficients[1]
  var_es[i] = sum(fit$residuals^2)/ (nrow(rrr) - 2)
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
covariance_matrix_sim = matrix(0,n_stocks,n_stocks)
for (i in 1:n_stocks)
{
  for(j in 1:n_stocks){
    if(i == j)
    {
      covariance_matrix_sim[i,j] = betas[i] * betas[i] * var_Rm + var_es[i]
    }else{
      covariance_matrix_sim[i,j] = betas[i] * betas[j] * var_Rm
    }
  }
}

inv_covmat_single_index = solve(covariance_matrix_sim)
ones = rep(1,n_stocks)

A = as.numeric(t(means) %*% inv_covmat_single_index %*% ones) 
B = as.numeric(t(means) %*% inv_covmat_single_index %*% means) 
C = as.numeric(t(ones)  %*% inv_covmat_single_index %*% ones) 
D = B*C - A^2

E <- seq(-0.2,0.2,.001)

sigmas <- sqrt(seq(1/C,0.03,.0001)) 
upper_part <- (A + sqrt(D*(C*sigmas^2 - 1)))*(1/C) 
lower_part <- (A - sqrt(D*(C*sigmas^2 - 1)))*(1/C) 

lines(sigmas, upper_part, lwd=1,type = "l",col = "orange",xlim = c(0, 0.2), ylim= c(-0.2,0.2)) 
lines(sigmas,lower_part, lwd=1,type = "l",col = "orange")

R_f = 0.002
R = means - R_f

Z = inv_covmat_single_index %*% R
x_G_sim = Z / sum(Z)

print("Composition of tangent")
print(x_G_sim)


varg_sim <- t(x_G_sim) %*% covariance_matrix_sim %*% x_G_sim 
Rg_sim <- t(x_G_sim) %*% means 
sigmag_sim <- sqrt(varg_sim)
points(sigmag_sim,Rg_sim, pch=19,lwd=1,col="blue")
abline(a = R_f, b = (Rg_sim - R_f)/sigmag_sim , lwd = 1, col = "red")

#Short sales allowed SIM

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
print(Weights_with_short)

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
print(Weights_no_short)


#find the return of the portfolio with short sales allowed

R_p_short <- Weights_with_short[,13] %*% Weights_with_short[,4]

covariance_matrix_ss = matrix(0,n_stocks,n_stocks)
var_Rm = var(r[,1])
for (i in 1:n_stocks)
{
  for(j in 1:n_stocks){
    if(i == j)
    {
      covariance_matrix_ss[i,j] = Weights_with_short[i,3] * Weights_with_short[i,3] * var_Rm + Weights_with_short[i,5]
    }else{
      covariance_matrix_ss[i,j] = Weights_with_short[i,3] * Weights_with_short[j,3] * var_Rm 
    }
  }
}

#find the risk of the portfolio with short sales allowed

var_p_short <- Weights_with_short[,13] %*% covariance_matrix_ss %*% Weights_with_short[,13]

#find the return of the portfolio with no short sales allowed
n_long = nrow(Weights_no_short)
R_p_no_short <- Weights_no_short[1:n_long,13] %*% Weights_no_short[1:n_long,4]
#find the risk of the portfolio with no short sales allowed
var_p_no_short <- Weights_no_short[1:n_long,13] %*% covariance_matrix_ss[1:n_long,1:n_long]  %*% Weights_no_short[1:n_long,13]

points(sqrt(var_p_short),R_p_short, pch=19,lwd=1,col="dimgray")
points(sqrt(var_p_no_short),R_p_no_short, pch=19,lwd=1,col="gold")


#Const Corr Model

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
xx <- (cbind(seq(1,30),Rbar, Rbar_f, sigma, Ratio))

#Order the table based on the excess return to sigma ratio:
aaa <- xx[order(-Ratio),]

#Create the last 3 columns of the table:
for(i in(1:n_stocks)) {
  
  col1[i] <- rho/(1-rho+i*rho)
  
  col2[i] <- sum(aaa[,5][1:i])
}

#Compute the Ci:
for(i in (1:n_stocks)) {
  
  col3[i] <- col1[i]*col2[i]
  
}

#Create the entire table until now:
xxx <- cbind(aaa, col1, col2, col3)


#SHORT SALES ALLOWED:
#Compute the Zi:
z <- (1/((1-rho)*xxx[,4]))*(xxx[,5]-xxx[,8][nrow(xxx)])

#Compute the xi:
x <- z/sum(z)

#The final table:
aaaa <- cbind(xxx, z, x)
print(aaaa)
#SHORT SALES NOT ALLOWED:
#Find composition of optimum portfolio when short sales are not allowed:
aaaaa <- aaaa[1:which(aaaa[,8]==max(aaaa[,8])), ]
z_no <- (1/((1-rho)*aaaaa[,4]))*(aaaaa[,5]-aaaaa[,8][nrow(aaaaa)])
x_no <- z_no/sum(z_no)

#Final table:
a_no <- cbind(aaaaa, z_no, x_no)
print(a_no)

#Var-covar matrix based on the constant correlation model:
for(i in 1:30){
  
  for(j in 1:30){
    
    if(i==j){
      mat[i,j]=aaaa[i,4]^2
    } else
    {
      mat[i,j]=rho*aaaa[i,4]*aaaa[j,4]
    }
  }
}


#Calculate the expected return and sd of the point of tangency 
#when short sales allowed
sd_p_opt <- (t(x) %*% mat %*% x)^.5
R_p_opt <- t(x) %*% aaaa[,2]


#Calculate the expected return and sd of the point of tangency 
#when short sales are not allowed
sd_p_opt_no <- (t(x_no) %*% mat[1:which(aaaa[,8]==max(aaaa[,8])),1:which(aaaa[,8]==max(aaaa[,8]))] %*% x_no)^.5
R_p_opt_no <- t(x_no) %*% aaaaa[,2]

#Trace out efficient portfolio
inv_covmat_const_corr= solve(mat)
ones = rep(1,n_stocks)

points(sd_p_opt,R_p_opt, pch=19,lwd=1,col="darkorchid")
points(sd_p_opt_no,R_p_opt_no, pch=19,lwd=1,col="khaki3")

#MULTIGROUP MODEL

R_f = 0.002
R = means - R_f

Z = solve(cov_matrix_group) %*% R
x_G_group = Z / sum(Z)


varg_group <- t(x_G_group) %*% cov_matrix_group %*% x_G_group 
Rg_group <- t(x_G_group) %*% means 
sigmag_group <- sqrt(varg_group)
points(sigmag_group,Rg_group, pch=19,lwd=1,col="lightcoral")
abline(a = R_f, b = (Rg_group - R_f)/sigmag_group , lwd = 1, col = "hotpink4")

legend("topleft", 
       legend=c("Stocks", "Equal Allocation","Min Risk Portfolio","Efficient Frontier","S&P500", "Historical Tangent Rf = 0.002", "CAL Line Historical Cov RF = 0.002","SIM Efficient Frontier"," CAL SIM RF = 0.002","SIM SS","SIM NO SS","Const Cor SS","Const Cor No SS","Multigroup Rf = 0.002","CAL Multigroup"),
       col=c("black","red","green","blue","brown","darkgreen","firebrick","orange","red","dimgray","gold","darkorchid","khaki3","lightcoral","hotpink4"),
       pch = 19,
       fill =c("black","red","green","blue","brown","darkgreen","firebrick","orange","red","dimgray","gold","darkorchid","khaki3","lightcoral","hotpink4"),
       cex=0.45)
# Equal allocation portfolio
#equal_weight_vector, means, new_covmat, equal_Rp, equal_sdp

# Min risk portfolio
#min_risk_weight_vector, means, new_covmat, min_risk_Rp, min_risk_sdp

# Tangent historical Rf = 0.002
#x_G_historic, means, new_covmat,Rg_historic, sigmag_historic

# Short sales allowed SIM
# Weights_with_short[,13],Weights_with_short[,4], covariance_matrix_ss, sqrt(var_p_short),R_p_short
Weights_with_short_inv_sorted = Weights_with_short[order(Weights_with_short[,1]),]

# No short sales allowed SIM
# Weights_with_short[1:n_long,13],Weights_with_short[1:n_long,4], covariance_matrix_ss[1:n_long,1:n_long], sqrt(var_p_no_short),R_p_no_short
Weights_with_no_short_inv_sorted = rep(0,30)
for (i in 1:n_long){
  Weights_with_no_short_inv_sorted[Weights_no_short[i,1]] = Weights_no_short[i,13]
}

# Short sales allowed Constant Corr
#aaaa[,10],aaaa[,2],mat,sd_p_opt,R_p_opt
const_corr_ss_allowed_inv_sorted = aaaa[order(aaaa[,1]),]


# No short sales allowed Constant Corr
n_long_2 = nrow(aaaaa)
#aaaaa[,10],aaaaa[,2],mat[1:n_long_2,1:n_long_2],sd_p_opt_no,R_p_opt_no
const_corr_ss_not_allowed_inv_sorted = rep(0,30)
for (i in 1:n_long_2){
  const_corr_ss_not_allowed_inv_sorted[aaaaa[i,1]] = aaaaa[i,10]
}

# Multi Group
# x_G_group, means, cov_matrix_group, Rg_group, sigmag_group

# TIME PLOTS OF PERFORMANCE OF ALL PORTFOLIOS COMPARED TO MARKET
#Historical period 
a_all <- read.csv("stockData.csv", sep=",", header=TRUE)

#Testing period:
a1 <- a_all[1:60,]
a2 <- a_all[61:99,]

#Convert adjusted close prices into returns:
r1 <- (a1[-1,3:ncol(a1)]-a1[-nrow(a1),3:ncol(a1)])/a1[-nrow(a1),3:ncol(a1)]

r2 <- (a2[-1,3:ncol(a2)]-a2[-nrow(a2),3:ncol(a2)])/a2[-nrow(a2),3:ncol(a2)]

#Time plot of equal allocation portfolio

#Monthly return in period 2015-01-01 to 2018-05-01:
r22 <- as.matrix(r2)

#Market (S&P500) performance in period 2015-01-01 to 2018-05-01:
plot(cumprod(1+(r22[,1])), ylim=c(0.5,6.5), type="l",col="pink", lwd=5,main = "Time plot of portfolios and S&P500", ylab = "Portfolio Return",xlab="Time(Months)",) # pink sandp500

#Assume equal allocation:
x <- rep(1/30, 30)

#Compute montly returns in period 2015-01-01 to 2018-05-01:
r22 <- as.matrix(r2)

EquRet <-  r22[,-1] %*% x

lines(cumprod(1+EquRet), col="blue", lwd=2) #equal allocation


#Assume min risk allocation:
x <- min_risk_weight_vector

MinRet <-  r22[,-1] %*% x

lines(cumprod(1+MinRet), col="black", lwd=2) #min portfolio

#Assume tangent rf = 0.002 historical allocation:
x <- x_G_historic

tangentRet <-  r22[,-1] %*% x

lines(cumprod(1+tangentRet), col="green", lwd=2) #tangent portfolio

#Assume sim ss allowed Rf = 0.002  allocation:
x <- Weights_with_short_inv_sorted[,13]

simSSRet <-  r22[,-1] %*% x

lines(cumprod(1+simSSRet), col="gold", lwd=2) #tangent portfolio

#Assume sim ss not allowed Rf = 0.002  allocation:
x <- Weights_with_no_short_inv_sorted

simNoSSRet <-  r22[,-1] %*% x

lines(cumprod(1+simNoSSRet), col="red", lwd=2) #tangent portfolio

#Assume const corr ss allowed Rf = 0.002  allocation:
x <- const_corr_ss_allowed_inv_sorted[,10]

constcorrSSRet <-  r22[,-1] %*% x

lines(cumprod(1+constcorrSSRet), col="darkturquoise", lwd=2) #tangent portfolio

#Assume const corr ss not allowed Rf = 0.002  allocation:
x <- const_corr_ss_not_allowed_inv_sorted

constcorrNoSSRet <-  r22[,-1] %*% x

lines(cumprod(1+constcorrNoSSRet), col="darkolivegreen", lwd=2) #tangent portfolio

#Assume multi group model with Rf = 0.002
x <- x_G_group

multiGroupRet <-  r22[,-1] %*% x

lines(cumprod(1+multiGroupRet), col="darksalmon", lwd=2) #tangent portfolio

legend("topleft", 
       legend=c("S&P500", "Equal Allocation","Min Risk Portfolio","Historical Tangent","SIM SS", "SIM No SS", "Const Corr SS","Const Cor No SS","Multigroup"),
       col=c("pink","blue","black","green","gold","red","darkturquoise","darkolivegreen","darksalmon"),
       pch = 19,
       fill =c("pink","blue","black","green","gold","red","darkturquoise","darkolivegreen","darksalmon"),
       cex=0.8)

# AVERAGE GROWTH OF EACH PORTFOLIO

#Instead compute geometric average:
comp <- cumprod(1+ EquRet)
geoMean1 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ MinRet)
geoMean2 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ tangentRet)
geoMean3 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ simSSRet)
geoMean4 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ simNoSSRet)
geoMean5 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ constcorrSSRet)
geoMean6 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ constcorrNoSSRet)
geoMean7 <- comp[length(comp)]^(1/length(comp)) - 1
#Instead compute geometric average:
comp <- cumprod(1+ multiGroupRet)
geoMean8 <- comp[length(comp)]^(1/length(comp)) - 1

print("Equal Portfolio Average Geometric Growth")
print(geoMean1)

print("Min Risk Portfolio Average Geometric Growth")
print(geoMean2)

print("Historical Tangent Average Geometric Growth")
print(geoMean3)

print("SIM SS Average Geometric Growth")
print(geoMean4)

print("SIM No SS Average Geometric Growth")
print(geoMean5)

print("Const Corr SS Average Geometric Growth")
print(geoMean6)

print("Const Cor No SS Average Geometric Growth")
print(geoMean7)

print("Multigroup Average Geometric Growth")
print(geoMean8)


# 3.Calculate the Sharpe ratio, differential excess return, Treynor measure, and Jensen differential performance index.
R_f = 0.002
#Sharpe Ratio
print("Equal Portfolio Sharpe Ratio")
x <- rep(1/30, 30)
Sharpe1 = (mean(EquRet) - R_f)/sqrt(var(EquRet))
print(Sharpe1)

print("Min Risk Portfolio Sharpe Ratio")
Sharpe2 = (mean(MinRet) - R_f)/sqrt(var(MinRet))
print(Sharpe2)

print("Historical Tangent Sharpe Ratio")
Sharpe3 = (mean(tangentRet) - R_f)/sqrt(var(tangentRet))
print(Sharpe3)

print("SIM SS Sharpe Ratio")
Sharpe4 = (mean(simSSRet) - R_f)/sqrt(var(simSSRet))
print(Sharpe4)

print("SIM No SS Sharpe Ratio")
Sharpe5 = (mean(simNoSSRet) - R_f)/sqrt(var(simNoSSRet))
print(Sharpe5)

print("Const Corr SS Sharpe Ratio")
Sharpe6 = (mean(constcorrSSRet) - R_f)/sqrt(var(constcorrSSRet))
print(Sharpe6)

print("Const Cor No SS Sharpe Ratio")
Sharpe7 = (mean(constcorrNoSSRet) - R_f)/sqrt(var(constcorrNoSSRet))
print(Sharpe7)

print("Multigroup Sharpe Ratio")
Sharpe8 = (mean(multiGroupRet) - R_f)/sqrt(var(multiGroupRet))
print(Sharpe8)

#Differential Excess Return
marketRet = r22[,1]
sigma_market = sqrt(var(marketRet))
mean_market = mean(marketRet)
print("Equal Portfolio Differential Excess Return")
x <- rep(1/30, 30)
R_A_apos_Equ = R_f + (((mean_market- R_f)*sqrt(var(EquRet)))/sigma_market)
DER1 = mean(EquRet) - R_A_apos_Equ
print(DER1)

print("Min Risk Portfolio Differential Excess Return")
R_A_apos_Min = R_f + (((mean_market- R_f)*sqrt(var(MinRet)))/sigma_market)
DER2 = mean(MinRet) - R_A_apos_Min
print(DER2)

print("Historical Tangent Differential Excess Return")
R_A_apos_hist_tangent = R_f + (((mean_market- R_f)*sqrt(var(tangentRet)))/sigma_market)
DER3 = mean(tangentRet) - R_A_apos_hist_tangent
print(DER3)

print("SIM SS Differential Excess Return")
R_A_apos_sim_ss = R_f + (((mean_market- R_f)*sqrt(var(simSSRet)))/sigma_market)
DER4 = mean(simSSRet) - R_A_apos_sim_ss
print(DER4)

print("SIM No SS Differential Excess Return")
R_A_apos_sim_no_ss = R_f + (((mean_market- R_f)*sqrt(var(simNoSSRet)))/sigma_market)
DER5 = mean(simNoSSRet) - R_A_apos_sim_no_ss
print(DER5)

print("Const Corr SS Differential Excess Return")
R_A_apos_rho_ss = R_f + (((mean_market- R_f)*sqrt(var(constcorrSSRet)))/sigma_market)
DER6 = mean(constcorrSSRet) - R_A_apos_rho_ss
print(DER6)

print("Const Cor No SS Differential Excess Return")
R_A_apos_rho_no_ss = R_f + (((mean_market- R_f)*sqrt(var(constcorrNoSSRet)))/sigma_market)
DER7 = mean(constcorrNoSSRet) - R_A_apos_rho_no_ss
print(DER7)

print("Multigroup Differential Excess Return")
R_A_apos_group = R_f + (((mean_market- R_f)*sqrt(var(multiGroupRet)))/sigma_market)
DER8 = mean(multiGroupRet) - R_A_apos_group
print(DER8)


#Treynor Measure
marketRet = r22[,1]

print("Equal Portfolio Treynor Measure")
x <- rep(1/30, 30)
beta_p_Equ = cov(marketRet,EquRet)/var(marketRet)
T1 = (mean(EquRet) - R_f)/beta_p_Equ
print(T1)

print("Min Risk Portfolio Treynor Measure")
beta_p_Min = cov(marketRet,MinRet)/var(marketRet)
T2 = (mean(MinRet) - R_f)/beta_p_Min
print(T2)

print("Historical Tangent Treynor Measure")
beta_p_hist_tangent = cov(marketRet,tangentRet)/var(marketRet)
T3 = (mean(tangentRet) - R_f)/beta_p_hist_tangent
print(T3)

print("SIM SS Differential Treynor Measure")
beta_p_sim_ss = cov(marketRet,simSSRet)/var(marketRet)
T4 = (mean(simSSRet) - R_f)/beta_p_sim_ss
print(T4)

print("SIM No SS Differential Treynor Measure")
beta_p_sim_no_ss = cov(marketRet,simNoSSRet)/var(marketRet)
T5 = (mean(simNoSSRet) - R_f)/beta_p_sim_no_ss
print(T5)

print("Const Corr SS Differential Treynor Measure")
beta_p_rho_ss = cov(marketRet,constcorrSSRet)/var(marketRet)
T6 = (mean(constcorrSSRet) - R_f)/beta_p_rho_ss
print(T6)

print("Const Cor No SS Differential Treynor Measure")
beta_p_rho_no_ss = cov(marketRet,constcorrNoSSRet)/var(marketRet)
T7 = (mean(constcorrNoSSRet) - R_f)/beta_p_rho_no_ss
print(T7)

print("Multigroup Treynor Measure")
beta_p_group = cov(marketRet,multiGroupRet)/var(marketRet)
T8 = (mean(multiGroupRet) - R_f)/beta_p_group
print(T8)

#Jensen Differential Performance index
marketRet = r22[,1]

print("Equal Portfolio Jensen Differential Performance index")
x <- rep(1/30, 30)
beta_p_Equ = cov(marketRet,EquRet)/var(marketRet)
R_A_apos_Equ2 = R_f + (mean_market - R_f)*beta_p_Equ
JDPI1 = mean(EquRet) - R_A_apos_Equ2
print(JDPI1)

print("Min Risk Portfolio Jensen Differential Performance index")
beta_p_Min = cov(marketRet,MinRet)/var(marketRet)
R_A_apos_Min2 = R_f + (mean_market - R_f)*beta_p_Min
JDPI2 = mean(MinRet) - R_A_apos_Min2
print(JDPI2)

print("Historical Tangent Jensen Differential Performance index")
beta_p_hist_tangent = cov(marketRet,tangentRet)/var(marketRet)
R_A_apos_hist_tangent2 = R_f + (mean_market - R_f)*beta_p_hist_tangent
JDPI3 = mean(tangentRet) - R_A_apos_hist_tangent2
print(JDPI3)

print("SIM SS Jensen Differential Performance index")
beta_p_sim_ss = cov(marketRet,simSSRet)/var(marketRet)
R_A_apos_sim_ss2 = R_f + (mean_market - R_f)*beta_p_sim_ss
JDPI4 = mean(simSSRet) - R_A_apos_sim_ss2
print(JDPI4)

print("SIM No SS Jensen Differential Performance index")
beta_p_sim_no_ss = cov(marketRet,simNoSSRet)/var(marketRet)
R_A_apos_sim_no_ss2 = R_f + (mean_market - R_f)*beta_p_sim_no_ss
JDPI5 = mean(simNoSSRet) - R_A_apos_sim_no_ss2
print(JDPI5)

print("Const Corr SS Jensen Differential Performance index")
beta_p_rho_ss = cov(marketRet,constcorrSSRet)/var(marketRet)
R_A_apos_rho_ss2 = R_f + (mean_market - R_f)*beta_p_rho_ss
JDPI6 = mean(constcorrSSRet) - R_A_apos_rho_ss2
print(JDPI6)

print("Const Cor No SS Jensen Differential Performance index")
beta_p_rho_no_ss = cov(marketRet,constcorrNoSSRet)/var(marketRet)
R_A_apos_rho_no_ss2 = R_f + (mean_market - R_f)*beta_p_rho_no_ss
JDPI7 = mean(constcorrNoSSRet) - R_A_apos_rho_no_ss2
print(JDPI7)

print("Multigroup Jensen Differential Performance index")
beta_p_group = cov(marketRet,multiGroupRet)/var(marketRet)
R_A_apos_group2 = R_f + (mean_market - R_f)*beta_p_group
JDPI8 = mean(multiGroupRet) - R_A_apos_group2
print(JDPI8)

#4. Decompose the overall performance using Fama’s decomposition (net selectivity and diversification) for
#the single index model when short sales are not allowed. Please show this decomposition on the plot
#expected return against beta.
R_A_bar_apos = R_f + (mean_market - R_f)*beta_p_sim_no_ss

beta_A_double_apos = sqrt(var(simNoSSRet)/var(marketRet))
R_A_bar_double_apos = R_f + (mean_market - R_f)*beta_A_double_apos
# plot function is used to plot
# the data type with "n" is used
# to remove the plotted data

plot(1, type = "n", xlab = "Beta",
     ylab = "Return", xlim = c(0, 1.12),
     ylim = c(0, 0.02),main = "Fama's decomposition")

points(beta_p_sim_no_ss,mean(simNoSSRet),lwd = 5, col = "gold")
points(beta_p_sim_no_ss,R_A_bar_apos,lwd = 5,col = "green")
points(beta_A_double_apos,R_A_bar_double_apos,lwd = 5,col = "blue")
abline(a = R_f, b = (mean_market - R_f) , lwd = 2, col = "red")
text(beta_p_sim_no_ss, mean(simNoSSRet), labels="R_A",cex = 0.5)
text(beta_p_sim_no_ss, R_A_bar_apos, labels="R_A_apos",cex = 0.5)
text(beta_A_double_apos, R_A_bar_double_apos, labels="R_A_double_apos",cex = 0.5)
legend("topleft", 
       legend=c("Return of the portfolio","R_A_bar_apos","R_A_bar_double_apos","Market Line"),
       col=c("gold","green","blue","red"),
       pch = 19,
       fill =c("gold","green","blue","red"),
       cex=0.8)

print("Return from selectivitiy")
print(mean(simNoSSRet) - R_A_bar_apos )

print("Return from net selectivitiy")
print(mean(simNoSSRet) - R_A_bar_double_apos )

print("Return from diversification")
print(R_A_bar_double_apos - R_A_bar_apos )