
means <- c(0.25,0.30,0.35)
seq1 <- c(0.04,0.02,0.01,0.02,0.09,0.05,0.01,0.05,0.16)
cov_mat <- matrix(seq1, nrow = 3, ncol = 3)
solve(cov_mat)

means_2 = means - 0.1

Z = cov_mat %*% means_2
X = Z/sum(Z)
R_g = t(X) %*% means
var_g = t(X) %*% cov_mat %*% X

R_A = 0.5* R_g + 0.5 * 0.1
x_A = 0.5 * X