#Compute PRESS
a_all <- read.csv("stockData.csv", sep=",", header=TRUE) 
# Use 5 year data to train 
a <- a_all[1:60,] 
#Convert adjusted close prices into returns:
r <- (a[-1,4:ncol(a)]-a[-nrow(a),4:ncol(a)])/a[-nrow(a),4:ncol(a)] # return of stocks
r_m <- (a[-1,3]-a[-nrow(a),3])/a[-nrow(a),3] # return of market

#use test data to compute actual predictions
a_test <- a_all[61:nrow(a_all),]

r_test <- (a_test[-1,4:ncol(a_test)]-a_test[-nrow(a_test),4:ncol(a_test)])/a_test[-nrow(a_test),4:ncol(a_test)] # return of stocks
r_m_test <- (a_test[-1,3]-a_test[-nrow(a_test),3])/a_test[-nrow(a_test),3] # return of market


n_stocks = 30

#Compute unadjusted historical betas
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
#Compute actual betas
mean_Rm_test= mean(r_m_test)
var_Rm_test <- var(r_m_test)
stdev_Rm_test <- var_Rm_test^.5

mean_Ri_test = colMeans(r)

betas_test = rep(0,n_stocks)
alphas_test = rep(0,n_stocks)
var_es_test = rep(0,n_stocks)
var_betas_test = rep(0,n_stocks)

for (i in 1:n_stocks){
  fit <- lm(r_test[,i] ~ r_m_test)
  betas_test[i] = fit$coefficients[2]
  alphas_test[i] = fit$coefficients[1]
  var_es_test[i] = sum(fit$residuals^2)/ (nrow(r_test) - 2)
  var_betas_test[i] = vcov(fit)[2,2]
}

#Compute PRESS
PRESS_direct = sum((betas - betas_test)^2)/(n_stocks)

fit <- lm(betas_test ~ betas)
beta_regress = fit$coefficients[2]
r2 = summary(fit)$r.squared

#Bias Component
term1 = (mean(betas_test) - mean(betas))^2
print("BIAS term:")
print(term1)

Sa2 <- (29/30)*var(betas_test)
Sp2 <- (29/30)*var(betas)

term2 = ((1-beta_regress)^2)*Sp2
print("Inefficiency")
print(term2)
term3 = (1-r2)*Sa2
print("Random error")
print(term3)
PRESS_indirect = term1 + term2 + term3