pbeta1 <- -0.1
pbeta2 <- 0.3
lu <- function (u, beta1, beta2) beta1*u + beta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
uEstrela <- optimize(f = lu, c(0,1), maximum = TRUE, beta1 = -0.1, beta2 = 0.3 )
