#Creating the function####
wishart_sample <- function(Sigma, m) {
  p <- ncol(Sigma)
  t(chol(Sigma))%*%matrix(rnorm(m*p),ncol=p)%*%chol(Sigma)
}
install.packages("corpcor")
library(corpcor)
# Set parameters
p <- 5
m <- 100
df <- p + 1
Sigma_inv <- diag(p)
# Generate matrices Σ from inverse-Wishart prior distribution
for (i in 1:m) {
  Sigma <- solve(wishart_sample(Sigma_inv,5))
  C <- cov2cor(Sigma)
  off_diag <- C[lower.tri(C)]
  if (i == 1) {
    all_off_diag <- off_diag
  } else {
    all_off_diag <- c(off_diag)
  }
}
hist(off_diag, breaks=200, main="Histogram of off-diagonal elements of simulated C-matrices")