mNW <- function(x, X, Y, h, K = dnorm) {

  Kx <- sapply(X, function(Xi) K((x - Xi) / h) / h)
  
  W <- Kx / rowSums(Kx)
  
  drop(W %*% Y)
}

set.seed(1)
n <- 51
eps <- rnorm(n, mean = 0, sd = 0.01)
m <- function(x) x
X <- seq(from = 0, to = 1, by = 0.02)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

h <- 0.1

plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h), col = 'blue')
legend("top", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = c("black", "blue"))
