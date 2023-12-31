#Midterm 2 2022, Q8
n  = 2
#When two stocks are present use p12
var1 = 0.0058
var2 = 0.0019

mean1 = 0.03283572
mean2 = 0.01752981

R_f = 0.01

rho = 0.503

#Initialize the vectors:
col1 <- rep(0,n)
col2 <- rep(0,n)
col3 <- rep(0,n)

#Initialize the var-covar matrix:
y <- rep(0,n*n)
mat <- matrix(y, ncol=n, nrow=n)

#Compute necessary quantities:
Rbar <- c(mean1,mean2)
Rbar_f <- Rbar-R_f
sigma <- c(sqrt(var1),sqrt(var2))
Ratio <- Rbar_f/sigma

#Initial table:
xx <- (cbind(Rbar, Rbar_f, sigma, Ratio))

#Order the table based on the excess return to sigma ratio:
aaa <- xx[order(-Ratio),]


#Create the last 3 columns of the table:
for(i in(1:n)) {
  
  col1[i] <- rho/(1-rho+i*rho)
  
  col2[i] <- sum(aaa[,4][1:i])
}

#Compute the Ci:
for(i in (1:n)) {
  
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
for(i in 1:n){
  
  for(j in 1:n){
    
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


#Plot all the stocks and the two tangency points:
plot(aaaa[,3], aaaa[,1], ylim=c(-0.013, 0.023), xlab="Risk", ylab="Expected return")

points(sd_p_opt,R_p_opt, col="green", pch=19)

points(sd_p_opt_no,R_p_opt_no, col="blue", pch=19)


#====================================================================
#====================================================================
#Note:  When short sales are allowed we can also find exactly the same #solution using Z=SIGMA^-1*R:
rr <- aaa[,1]-.05
znew <- solve(mat) %*% rr
xnew <- znew/sum(znew)

#====================================================================
#====================================================================
