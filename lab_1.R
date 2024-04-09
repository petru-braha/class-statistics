vector_sqrt = function(x) {
  for(i in 1:length(x)) {
    if(x[i] > 0)
      x[i] = sqrt(x[i])
    else
      x[i] = sqrt(-x[i])
  }
}

variance = function(x, p)
{
  expectation = sum(x*p)
  variance = sum(p*(x-expectation)^2)
  return (variance)
}

ex8 = function()
{
  table= read.table("test.txt", header=T)
  x = table[['AA']]
  y = table[['BB']]
  plot(x, y, type='l')
  b = x*y
  c = abs(x-y)/(sum(x-y)^2)
  
  d = 
}