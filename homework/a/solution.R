exa1a = function(lambda, p, n, m, k)
{
  #poisson
  x = k:(m-1)
  prob_x_p = dpois(x, lambda)

  #geometric
  x = k:(m-1)
  prob_x_g = dgeom(x, p)
  
  #binomial
  x = k:(m-1)
  prob_x_b = dbinom(x, size = n, prob = p)
}

exa1b = function()

# A1. b) ####################################################
GeometricImpar <- function(n, p)
{
  x=0:(n-1)
  y=dgeom(x,p) #P(X=impar)
  return (sum(y)/2)
}

Geometric4 <- function(n, p)
{
  x=0:(n-1)
  y=dgeom(x, p) #P(X>=4)
  return (1-sum(y))
}

Geometric20 <- function(n, p)
{
  x=0:(n-1)
  y=dgeom(x, p) #P(X<=20)
  return (sum(y))
}

GeometricImpar(n, p) #0.4976262
Geometric4(4, p) #0.2401
Geometric20(20, p) #0.9992021
# A1. c) ####################################################
PoissonK0 <- function(n, lambda)
{
  x=0:(n-1)
  y=dpois(x, lambda) #P(Y>=k0), aici P(X>=n)
  return (1-sum(y))
}

k0=0
val_poisson=PoissonK0(k0, lambda)
while(val_poisson>=0.0000001)
{
  k0=k0+1
  val_poisson=PoissonK0(k0, lambda)
}
print(k0-1) #16
# A2. a) ####################################################
StatisticiA <- function(fisier)
{
  prob=fisier[['P']]
  stat=fisier[['S']]
  
  print(median(prob)) #6.25
  print(mean(prob)) #6.67726
  print(sd(prob)) #1.616303
  print(quantile(prob)) 
  #0%  25%  50%  75% 100% 
  #3.00  5.50  6.25  7.75 10.00 
  
  print(median(stat)) #5.5
  print(mean(stat)) #5.702966
  print(sd(stat)) #1.824378
  print(quantile(stat))
  #0%  25%  50%  75% 100% 
  #2.00  4.50  5.50  6.75 10.00 
}

#fisier=read.delim("note.txt", header=TRUE, sep=" ")
fisier=read.csv("note.csv", header=TRUE, sep=",")
StatisticiA(fisier)
# A2. b) ####################################################
StatisticiB <- function(fisier, esantion)
{
  m=mean(esantion)
  s=sd(esantion)
  outliers=vector()
  j=0
  for(i in 1:length(esantion))
  {
    if(esantion[i]<m-2*s | esantion[i]>m+2*s)
    {
      j=j+1
      outliers[j]=esantion[i]
      esantion[-c(i)]
      i=i-1
    }
  }
  return(esantion)
}

prob=fisier[['P']]
stat=fisier[['S']]
StatisticiB(fisier, prob) #prea multe valori de scris in comentarii, la ambele
StatisticiB(fisier, stat)
# A2. c) ####################################################
StatisticiC <- function(fisier)
{
  prob=fisier[['P']]
  stat=fisier[['S']]
  esantion=vector()
  for(i in 1:length(prob)) #si prob si stat au aceeasi lungime, in teorie
  {
    esantion[i]=(prob[i]+stat[i])/2 #calculeaza nota finala la PS
  }
  interval=seq(1, 10, 1)
  hist(esantion, breaks=interval, right=TRUE, freq=FALSE)
}

StatisticiC(fisier)
