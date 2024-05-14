#last two exercices
#exII2
f <- function(u) {
  exp(-2*u^2)
}

lambda <- 3
N <- 50000
x <- rexp(N, lambda)

estimate <- sum(f(x))/(lambda*N)

true_value <- sqrt(pi)/8
error <- abs(estimate - true_value)

cat("Estimate:", estimate, "\n")
cat("True value:", true_value, "\n")
cat("Error:", error, "\n")

#exIII2
p1 <- 1/4
p2 <- 3/4

lambda1 <- 4
lambda2 <- 12

N <- 10000
sum_x <- 0
for (i in 1:N) {
  x <- p1*rexp(1, lambda1) + p2*rexp(1, lambda2)
  sum_x <- sum_x + x
}
expectation <- sum_x/N

cat("Estimated expectation:", expectation, "\n")

#exIV1
n <- 100000
x <- rgeom(n, 0.3)
y <- rgeom(n, 0.5)

p <- mean(x > y^2)

n <- 100
error <- 1
while (error > 0.005) {
  x <- c(x, rgeom(n, 0.3))
  y <- c(y, rgeom(n, 0.5))
  p_new <- mean(x > y^2)
  error <- qnorm(0.975)*sqrt(p_new*(1-p_new)/length(x))
  n <- n + 100 
}

cat("Estimated probability:", p, "\n")
cat("Number of runs:", length(x), "\n")