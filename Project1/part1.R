#^GSPC,
#AAPL,MSFT,NVDA,TSM,ASML,AVGO,
#GOOGL,META,DIS,TMUS,VZ,CMCSA,
#AMZN,TSLA,HD,BABA,MCD,TM,
#WMT,PG,KO,PEP,COST,FMX,
#BHP,LIN,RIO,VALE,APD,SCCO

a_all <- read.csv("stockData.csv", sep=",", header=TRUE)
#Convert adjusted close prices into returns:
a <- a_all[1:60,] # Use 5 year data to train
r <- (a[-1,3:ncol(a)]-a[-nrow(a),3:ncol(a)])/a[-nrow(a),3:ncol(a)]

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
     xlim = c(0, 0.2),
     ylim = c(0, 0.06),
     pch=19)

#Assume equal allocation portfolio using the 30 stocks.
new_means <- colMeans(r[,-1])
new_covmat <- cov(r[,-1]) 
new_cormat <- cor(r[,-1]) 
new_variances <- diag(new_covmat)
new_stdev <- diag(new_covmat)^.5
 
number_of_stocks = 30
ones_vector <- rep(1, number_of_stocks)
equal_weight_vector <- ones_vector/number_of_stocks

equal_varp <- t(equal_weight_vector) %*% new_covmat %*% equal_weight_vector
equal_sdp <- sqrt(equal_varp)

equal_Rp <- t(equal_weight_vector) %*% new_means
  
points(equal_sdp, equal_Rp, pch = 19, lwd=5, col="red")

#Assume minimum risk portfolio

ones_vector <- rep(1, number_of_stocks)
inverse_new_covmat <- solve(new_covmat)
min_risk_weight_vector <- inverse_new_covmat  %*% ones_vector / as.numeric(t(ones_vector)  %*% inverse_new_covmat  %*% ones_vector)

min_risk_varp <- t(min_risk_weight_vector) %*% new_covmat %*% min_risk_weight_vector
min_risk_sdp <- sqrt(min_risk_varp)
min_risk_Rp <- t(min_risk_weight_vector) %*% new_means

points(min_risk_sdp, min_risk_Rp, pch=19, lwd=5, col="green")

legend("topright", legend=c("Minimum Risk Portfolio", "Equal Allocation", "Individual stocks"),
       col=c("green", "red","black"),pch = 19,fill =c("green", "red","black"),cex=0.8)
