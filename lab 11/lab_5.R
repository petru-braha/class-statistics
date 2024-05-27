generation <- function(x, p)
{
  U <- runif(1)
  actual <- 0
  
  for(i in seq_along(p))
  {
    actual <- actual + p[i]
    if(U <= actual)
      return(x[i])
  }
  
  return(x[length(x)])
}

ex1 <- function()
{
  cat("output:", generation(c(1, 2, 3), c(0.2, 0.5, 0.3)), "\n")
}