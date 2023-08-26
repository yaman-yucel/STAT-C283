#CONSTANT CORRELATION MODEL
#Read the data:

#Compute the average correlation:
#rho <- (sum(cor(data1[1:10]))-10)/90
rho = 0.5

#Initialize the vectors:
col1 <- rep(0,3)
col2 <- rep(0,3)
col3 <- rep(0,3)

#Initialize the var-covar matrix:
y <- rep(0,9)
mat <- matrix(y, ncol=3, nrow=3)

#Compute necessary quantities:
Rbar <- c(0.29,0.19,0.08)
Rbar_f <- Rbar-0.05
sigma <- c(0.03,0.02,0.15)
Ratio <- Rbar_f/sigma

#Initial table:
xx <- (cbind(Rbar, Rbar_f, sigma, Ratio))

#Order the table based on the excess return to sigma ratio:
aaa <- xx[order(-Ratio),]


#Create the last 3 columns of the table:
for(i in(1:3)) {
  
  col1[i] <- rho/(1-rho+i*rho)
  
  col2[i] <- sum(aaa[,4][1:i])
}

#Compute the Ci:
for(i in (1:3)) {
  
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
for(i in 1:3){
  
  for(j in 1:3){
    
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
