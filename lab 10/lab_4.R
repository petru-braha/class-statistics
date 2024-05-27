# first two exercices
sphere_volume = function(n)
{
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if(x*x + y*y + z*z <= 1)
      N_C = N_C + 1;
  }
  
  return(8*N_C/N);
}

exI1 = function(condition)
{
  abs_err = abs(exI1(10000) - 4*pi/3);
  rel_err = abs_err/(4*pi/3);
  print(abs_err);
  print(rel_err);
}

MC_integration = function(N)
{
  sum = 0;
  for(i in 1:N)
  {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  return(10*sum/N);
}

MC_integr_average = function(k, N) 
{
  estimates = vector();
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

MC_improvd = function(n)
{
  sum = 0;
  for(i in 1:n)
  {
    u = rexp(1, 1);
    sum = sum + exp(-u*u) / exp(-u);
  }
  return (sum / n);
}

MC_integr_improvd = function(k, N) 
{
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improvd(N);
  print(mean(estimates));
  print(sd(estimates));
}

area_parabola = function() 
{
  f <- function(x) -2*x^2 + 5*x - 2
  
  a = 0
  b = 2
  exact_area = integrate(f, a, b)$value
  
  set.seed(123)
  n = 10000
  x = runif(n, a, b)
  y = runif(n, 0, max(f(x)))
  
  inside = y <= f(x)
  prop_inside = mean(inside)
  
  area = (b - a) * max(f(x)) * prop_inside
  rel_error = abs(exact_area - area) / exact_area
  
  result = list(area = area, exact_area = exact_area, rel_error = rel_error)
  return(result)
}

exI2 = function() 
{
  result = area_parabola()
  
  cat("Estimated area:", format(result$area, digits = 8), "\n")
  cat("Exact area:", format(result$exact_area, digits = 8), "\n")
  cat("Relative error:", format(result$rel_error, digits = 2), "%\n")
}

sin2x_integral = function()
{
  f = function(x) sin(x)^2
  
  a = 0
  b = pi
  exact_value = pi / 2
  
  n = 10000 
  x = seq(a, b, length.out = n+1)
  h = (b - a) / n
  integral = h * (sum(f(x)) - 0.5*(f(a) + f(b)))
  
  abs_error = abs(exact_value - integral)
  rel_error = abs_error / exact_value
  
  result = list(integral = integral, exact_value = exact_value,
                 abs_error = abs_error, rel_error = rel_error)
  return(result)
}

exII1a = function()
{
  result <- sin2x_integral()
  
  cat("Estimated value of integral:", format(result$integral, digits = 8), "\n")
  cat("Exact value of integral:", format(result$exact_value, digits = 8), "\n")
  cat("Absolute error:", format(result$abs_error, digits = 8), "\n")
  cat("Relative error:", format(result$rel_error * 100, digits = 2), "%\n")
}

exII1b = function()
{
  f <- function(x) exp(x)
  
  a <- 1
  b <- 4
  exact <- integrate(f, a, b)$value
  
  n <- 10000
  x <- seq(a, b, length.out = n+1)
  y <- f(x)
  approx <- (b-a)/(2*n) * (sum(y) - y[1] - y[n+1] + 2 * sum(y[2:n]))
  
  abs_error <- abs(exact - approx)
  rel_error <- abs_error / exact * 100
  
  cat("Exact value: ", exact, "\n")
  cat("Approximation: ", approx, "\n")
  cat("Absolute error: ", abs_error, "\n")
  cat("Relative error: ", rel_error, "%\n")
}

MC_INTEGRAL = function()
{
  f <- function(x) 1 / (4*x^2 - 1)
  
  a <- 1
  b <- Inf
  exact_value <- log(3/4)
  
  set.seed(123)
  n <- 100000
  x <- runif(n, a, b)
  
  integral <- (b - a) * mean(f(x))
  
  abs_error <- abs(integral - exact_value)
  rel_error <- abs_error / exact_value
  
  result <- list(integral = integral, exact_value = exact_value, abs_error = abs_error, rel_error = rel_error)
  return(result)
}

exII1d = function()
{
  result = MC_INTEGRAL()
  
  cat("Estimated value:", format(result$integral, digits = 8), "\n")
  cat("Exact value:", format(result$exact_value, digits = 8), "\n")
  cat("Absolute error:", format(result$abs_error, digits = 8), "\n")
  cat("Relative error:", format(result$rel_error, digits = 2), "%\n")
}
