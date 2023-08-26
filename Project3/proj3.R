#a
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/statc183c283_5stocks.txt", header=T)

#Exxon-mobil -> P1
#General Motors -> P2
#Hewlett Packard -> P3
#McDonalds -> P4
#Boeing -> P5


#Reverse the list such that older values appear on top
b<- a[dim(a)[1]:1,]

#Convert adjusted close prices into returns:
r <- (b[-1,2:ncol(b)]-b[-nrow(b),2:ncol(b)])/b[-nrow(b),2:ncol(b)]

#b

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
#Compute inverse of variance covariance matrix 
inv_covmat <- solve(covmat)

# c
ones_2 = rep(1,2)

r_2 = r[c("P1","P5")]

means_2 <- colMeans(r_2)
covmat_2 <- cov(r_2)
stdev_2 <- diag(covmat_2)^.5
inv_covmat_2 <- solve(covmat_2)

min_risk_weight_vector_2 <- inv_covmat_2  %*% ones_2 /as.numeric(t(ones_2)  %*%  inv_covmat_2 %*% ones_2) 
min_risk_varp_2 <- t(min_risk_weight_vector_2) %*% covmat_2 %*% min_risk_weight_vector_2 
min_risk_Rp_2 <- t(min_risk_weight_vector_2) %*% means_2 
min_risk_sigmap_2 <- sqrt(min_risk_varp_2)

#d
x1 = seq(from = -5,to = 5, by = 0.01) 
x2 = 1 - x1
means_plot= rep(0,length(x1))
vars_plot = rep(0,length(x1))
for (i in 1:length(x1)){
  coef_temp = c(x1[i],x2[i])
  means_plot[i] = t(coef_temp) %*% means_2
  vars_plot[i] = t(coef_temp) %*% covmat_2 %*% coef_temp
}
plot(sqrt(vars_plot), means_plot, ylab = 'E', xlab = expression(sigma), main="Risk-Return Plot 2 Stocks (d)") 
points(sqrt(min_risk_varp_2), min_risk_Rp_2, pch=19,lwd=1,col="green")
legend("topright", 
       legend=c("Minimum Risk Portfolio","Random portfolios/Efficient Frontier"),
       col=c("green","black"),
       pch = 19,
       fill =c("green","black"),
       cex=0.8)
# e
coef_3 <- read.table("http://www.stat.ucla.edu/~nchristo/datac183c283/statc183c283_abc.txt", header=T)

n_samples_3 = dim(coef_3)[1]

ones_3 = rep(1,3)

r_3 = r[c("P1","P4","P5")]

means_3 <- colMeans(r_3)
covmat_3 <- cov(r_3)
stdev_3 <- diag(covmat_3)^.5
inv_covmat_3 <- solve(covmat_3)

min_risk_weight_vector_3 <- inv_covmat_3  %*% ones_3 /as.numeric(t(ones_3)  %*%  inv_covmat_3 %*% ones_3) 
min_risk_varp_3 <- t(min_risk_weight_vector_3) %*% covmat_3 %*% min_risk_weight_vector_3 
min_risk_Rp_3 <- t(min_risk_weight_vector_3) %*% means_3 
min_risk_sigmap_3 <- sqrt(min_risk_varp_3)

means_plot_3 = rep(0,n_samples_3)
vars_plot_3 = rep(0,n_samples_3)
for (i in 1:n_samples_3){
  coef_temp = c(coef_3$a[i],coef_3$b[i],coef_3$c[i])
  means_plot_3[i] = t(coef_temp) %*% means_3
  vars_plot_3[i] = t(coef_temp) %*% covmat_3 %*% coef_temp
}
plot(sqrt(vars_plot_3), means_plot_3,pch=19,cex = 0.2,lwd=0.1,ylab = 'E', xlab = expression(sigma), main="Risk-Return Plot 3 Stocks") 
points(min_risk_sigmap_3,min_risk_Rp_3, pch=19,lwd=1,col="gold")


# f

R_f = 0.001
R_new = matrix(means_3 - R_f)
z = inv_covmat_3 %*% R_new
lambda_g = ones_3 %*% z
x_G = z / as.numeric(lambda_g) # composition


varg <- t(x_G) %*% covmat_3 %*% x_G 
Rg <- t(x_G) %*% means_3 
sigmag <- sqrt(varg)
points(sigmag,Rg, pch=19,lwd=1,col="blue")
part_g_pf_R = R_f * 0.4 + 0.6 * Rg
part_g_pf_sigma = (part_g_pf_R - R_f) / ((Rg - R_f)/ sigmag)
points(part_g_pf_sigma,part_g_pf_R, pch=19,lwd=1,col="purple")
abline(a = R_f, b = (Rg - R_f)/sigmag , lwd = 2, col = "red")

# h
E = as.numeric(Rg)
composition_portfolio_g = (E - R_f) *( inv_covmat_3 %*% R_new / as.numeric( t(R_new) %*% inv_covmat_3 %*% R_new))
#This is the composition of portfolio in plotted with purple( 60% risk, 40% free)
print(paste("composition of portfolio in plotted with purple( 60% risk, 40% free)",composition_portfolio_g))

# i

# 1

R_f1 = 0.001
R_newA = matrix(means_3 - R_f1)
z = inv_covmat_3 %*% R_newA
lambda_g = ones_3 %*% z
x_A = z / as.numeric(lambda_g) # composition

R_f2 = 0.002
R_newB = matrix(means_3 - R_f2)
z = inv_covmat_3 %*% R_newB
lambda_g = ones_3 %*% z
x_B = z / as.numeric(lambda_g) # composition

#2
cov_AB = t(x_A) %*% covmat_3 %*% x_B
var_A = t(x_A) %*% covmat_3 %*% x_A
var_B = t(x_B) %*% covmat_3 %*% x_B
mean_A = t(x_A) %*% means_3
mean_B = t(x_B) %*% means_3
mean_AB = c(mean_A,mean_B)

#points(sqrt(var_A), mean_A,pch=19, ylab = 'E', xlab = expression(sigma), main="Risk-Return Plot",col="orange") 
points(sqrt(var_B), mean_B,pch=19, ylab = 'E', xlab = expression(sigma), main="Risk-Return Plot",col="green") 


legend("topright", 
       legend=c("Minimum Risk Portfolio", "Stock on CAL or A", "Tangent(G)","CAL", "Stock B","Random portfolios"),
       col=c("gold","purple", "blue","red","green","black"),
       pch = 19,
       fill =c("gold","purple", "blue","red","green","black"),
       cex=0.8)

#3
vec = c(var_A,cov_AB,cov_AB,var_B)
covmat_AB = matrix(vec, nrow = 2, byrow = TRUE)

#d

x1 = seq(from = -5,to = 5, by = 0.01) 
x2 = 1 - x1

vars_plot = rep(0,length(x1))
for (i in 1:length(x1)){
  coef_temp = c(x1[i],x2[i])
  means_plot[i] = t(coef_temp) %*% mean_AB
  vars_plot[i] = t(coef_temp) %*% covmat_AB %*% coef_temp
}
plot(sqrt(vars_plot_3), means_plot_3,pch=19,cex = 0.2,lwd=0.1,ylab = 'E', xlab = expression(sigma), main="Risk-Return Plot 3 Stocks") 

points(sqrt(vars_plot), means_plot,pch=19 , xlab = expression(sigma),cex = 0.1, main="Risk-Return Plot",col="green") 

#4
min_risk_weight_vector_3 <- inv_covmat_3  %*% ones_3 /as.numeric(t(ones_3)  %*%  inv_covmat_3 %*% ones_3) 
min_risk_varp_3 <- t(min_risk_weight_vector_3) %*% covmat_3 %*% min_risk_weight_vector_3 
min_risk_Rp_3 <- t(min_risk_weight_vector_3) %*% means_3 
min_risk_sigmap_3 <- sqrt(min_risk_varp_3)
points(min_risk_sigmap_3, min_risk_Rp_3,pch=19 , xlab = expression(sigma),cex = 1, main="Risk-Return Plot",col="blue") 
legend("topright", 
       legend=c("Minimum Risk Portfolio","Random portfolios","Efficient Frontier/Traced with A and B"),
       col=c("blue","black","green"),
       pch = 19,
       fill =c("blue","black","green"),
       cex=0.8)