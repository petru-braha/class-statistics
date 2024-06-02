#'*last three exercises*

exII2 = function()
{
  f <- function(u) exp(-2*u^2)
  
  lambda = 3
  n = 50000
  sum = 0
  
  exact = sqrt(pi)/8
  for(i in 1:n)
  {
    u = rexp(1, lambda)
    density_u = lambda * exp(-u*lambda)
    sum = sum + (f(u) / density_u )
  }
  
  aprox = sum / n 
  
  abs_error = abs(aprox - exact)
  rel_error = abs_error / abs(exact)
  
  cat("estimated value:", format(aprox, digits=5), "\n")
  cat("exact value:", format(exact, digits=5), "\n")
  cat("absolute error:", format(abs_error, digits=4), "\n")
  cat("relative error:", format(rel_error, digits=5), "\n")
}

exIII2 = function()
{
  p1 = 1/4
  p2 = 3/4
  
  lambda1 = 4
  lambda2 = 12
  
  N = 10000
  sum_x = 0
  for (i in 1:N) 
  {
    x = p1*rexp(1, lambda1) + p2*rexp(1, lambda2)
    sum_x = sum_x + x
  }
  expectation = sum_x/N
  
  cat("estimated expectation:", expectation, "\n")
}

exIV1 = function()
{
  n = 100000
  x = rgeom(n, 0.3)
  y = rgeom(n, 0.5)
  
  p = mean(x > y^2)
  
  n = 100
  error = 1
  while (error > 0.005) {
    x <- c(x, rgeom(n, 0.3))
    y <- c(y, rgeom(n, 0.5))
    p_new <- mean(x > y^2)
    error <- qnorm(0.975)*sqrt(p_new*(1-p_new)/length(x))
    n <- n + 100 
  }
  
  cat("estimated probability:", p, "\n")
  cat("number of runs:", length(x), "\n")
}
