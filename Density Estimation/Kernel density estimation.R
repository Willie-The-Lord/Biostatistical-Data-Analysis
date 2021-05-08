n <- rexp(500, rate = 1)

x <- 0:10
y <- exp(-x)

curve(exp(-x), 0, 10, ylab = "density")
title(main = "True density function v.s. Kernel density estimation")
#curve(x, y, type = 'l', main = "true density v.s. kernel density")
lines(density(n, kernel = "gaussian", bw = "nrd0"), col = "green")
lines(density(n, kernel = "gaussian", bw = "sj"), col = "orange")
lines(density(n, kernel = "epanechnikov", bw = "nrd0"), col = "red")
lines(density(n, kernel = "epanechnikov", bw = "sj"), col = "blue")



x1 = rexp(500, rate = 1) 
#Estimate
est = density(x1, kernel = "gaussian", bw = "nrd0")
# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dexp(est$x))^2)
integrate(splxy, lower = min(x1), upper = max(x1)) 

x2 = rexp(500, rate = 1) 
#Estimate
est = density(x2, kernel = "gaussian", bw = "sj")
# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dexp(est$x))^2)
integrate(splxy, lower = min(x2), upper = max(x2)) 

x3 = rexp(500, rate = 1) 
#Estimate
est = density(x3, kernel = "epanechnikov", bw = "nrd0")
# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dexp(est$x))^2)
integrate(splxy, lower = min(x3), upper = max(x3)) 

x4 = rexp(500, rate = 1) 
#Estimate
est = density(x3, kernel = "epanechnikov", bw = "sj")
# Create approx func obj and integrate()
splxy = splinefun(est$x, (est$y - dexp(est$x))^2)
integrate(splxy, lower = min(x4), upper = max(x4)) 
