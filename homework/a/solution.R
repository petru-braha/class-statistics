exa1a = function(lambda, p, n, m, k)
{
  x = seq(k, m, length(m-k))
  prob_x_p = dpois(x, lambda)
  prob_x_g = dgeomz(x, p)
  prob_x_b = dbinom(x, size = n, prob = p)
}

