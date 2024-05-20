exA1a = function(lambda, p, n, m, k)
{
  x = seq(k, m, length(m-k))
  prob_x_p = dpois(x, lambda)
  prob_x_g = dgeom(x, p)
  prob_x_b = dbinom(x, size = n, prob = p)
  
  print(prob_x_b) #bin
  print(prob_x_g) #geo
  print(prob_x_p) #pis
}

exA1b = function(lambda, p, n, m, k)
{
  x = seq(k, m, length(m-k))
  prob_x_p = dpois(x, lambda)
  prob_x_g = dgeom(x, p)
  prob_x_b = dbinom(x, size = n, prob = p)
  
  plot(x, prob_x_b, main='binomial distribution', xlab='x', ylab='probability')
  plot(x, prob_x_g, main='geometric distribution', xlab='x', ylab='probability')
  plot(x, prob_x_p, main='poisson distribution', xlab='x', ylab='probability')
}

exA1c = function(lambda)
{
  final = 1-10^(-6);
  sum = 0
  k0 = 0
  while(sum <= final)
  {
    sum = sum + (lambda^k0) * exp(-lambda) / factorial(k0)
    k0 = k0 + 1
  }
  print(k0)
}

exA2a = function() # to review
{
  data = read.csv("note_PS.csv", header = T, sep = ',')
  p_mark = data[['P']]
  s_mark = data[['S']]
  
  absolute_p = table(p_mark)
  relative_p = absolute_p / length(p_mark)
  
  absolute_s = table(s_mark)
  relative_s = absolute_s / length(s_mark)
  
  p_mean = mean(p_mark)
  s_mean = mean(s_mark)
}

exA2b = function(file_name, sample)
{
  values = read.table(file_name)
  
  q1 = as.vector(quantile(sample))[2]
  q3 = as.vector(quantile(sample))[4]
  iqr = q3 - q1
  for(i in 1:length(values))
    if(values[i] < q1 + 1.5*iqr | values[i] > q3 + 1.5*iqr )
    {
      temp = values[i]
      values[!values == temp]
    }
  
  return(values)
}