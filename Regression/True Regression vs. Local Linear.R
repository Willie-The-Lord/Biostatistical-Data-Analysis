set.seed(1)
n <- 51
eps <- rnorm(n, mean = 0, sd = 0.01)
m <- function(x) 120*(x^2) + 3*x + 7
X <- seq(from = 0, to = 1, by = 0.02)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

h <- 0.1

lp1 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = 2,
                           range.x = c(-10, 10), gridsize = 5000)

plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(lp1$x, lp1$y, col = 2)
legend("top", legend = c("True regression", "Local linear"), lwd = 2, col = 1:2)