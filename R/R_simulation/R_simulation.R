h <- function (x, n) {
  total = 0
  for (i in seq(0,n)) {
    total <- total + x^i    
  }
  print(total)
  print((1-x^(n+1))/(1-x))
}

h(2,3)
h(0.3,55)
h(6.6,8)

v <- function (x, n) {
  total = sum(x^seq(0,n))
  print(total)
  print((1-x^(n+1))/(1-x))
}

v(1,8)


demo(graphics)

## Ch7 label plot with math
demo(plotmath)
curve(100*(x^3-x^2)+15, from = 0 , to = 1, 
      xlab = expression(alpha),
      ylab = expression(100 %*% (alpha^3 - alpha^2) + 15),
      main = expression(paste("Function: ", f(alpha) == 100 %*% (alpha^3-alpha^2) + 15)))

myMu <- 0.5
mySigma <- 0.25
par(usr = c(0, 1, 0, 1)) # Change coordinates within plot
text(0.1, 0.1, bquote(sigma[alpha] == .(mySigma)), cex=1.25)
text(0.6, 0.6, paste("(The mean is ", myMu, ")", sep=""), cex=1.25) 
text(0.5, 0.9,
     bquote(paste("sigma^2 = ", sigma^2 == .(format(mySigma^2, 2)))))


## Ch9 Numerical

.Machine$integer.max
.Machine

# program spuRs/resources/scripts/newtonraphson.r
# loadable spuRs function
newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  # Newton_Raphson algorithm for solving ftn(x)[1] == 0
  # we assume that ftn is a function of a single variable that returns
  # the function value and the first derivative as a vector of length 2 #
  # x0 is the initial guess at the root
  # the algorithm terminates when the function value is within distance
  # tol of 0, or the number of iterations exceeds max.iter
  # initialise
  x <- x0
  fx <- ftn(x)
  iter <-  0
  # continue iterating until stopping conditions are met while ((abs(fx[1]) > tol) && (iter < max.iter)) {
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
  x <- x - fx[1]/fx[2]
  fx <- ftn(x)
  iter <- iter + 1
  cat("At iteration", iter, "value of x is:", x, "\n")
  }

# output depends on success of algorithm
if (abs(fx[1]) > tol) {
  cat("Algorithm failed to converge\n")
  return(NULL)
} else {
  cat("Algorithm converged\n")
  return(x) }
}


ftn4 <- function(x) {
    # returns function value and its derivative at x
    fx <- log(x) - exp(-x)
    dfx <- 1/x + exp(-x)
    return(c(fx, dfx))
}

newtonraphson(ftn4, 2, 1e-6)

## Ch.10 integration

trapezoid <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using the trapezoid rule with n subdivisions #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive integer
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, ftn)
  T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2) 
  return(T)
}

ftn6 <- function(x) return(4*x^3)
trapezoid(ftn6, 0, 5, n = 100000)


simpson_n <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with n subdivisions #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive even integer
  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2)) 
  return(S)
}
simpson_n(ftn6, 0, 1, 20)


#### Ch 12 Optimization:
### Newton's in higher dimension


f3 <- function(x){
  a <- x[1]^2/2 - x[2]^2/4
  b <- 2*x[1] - exp(x[2])
  f <- sin(a)*cos(b)
  f1 <- cos(a)*cos(b)*x[1] - sin(a)*sin(b)*2
  f2 <- -cos(a)*cos(b)*x[2]/2 + sin(a)*sin(b)*exp(x[2]) 
  f11 <- -sin(a)*cos(b)*(4 + x[1]^2) + cos(a)*cos(b) - cos(a)*sin(b)*4*x[1]
  f12 <- sin(a)*cos(b)*(x[1]*x[2]/2 + 2*exp(x[2])) + cos(a)*sin(b)*(x[1]*exp(x[2]) + x[2])
  f22 <- -sin(a)*cos(b)*(x[2]^2/4 + exp(2*x[2])) - cos(a)*cos(b)/2 - cos(a)*sin(b)*x[2]*exp(x[2]) + sin(a)*sin(b)*exp(x[2]) 
  return(list(f, c(f1, f2), matrix(c(f11, f12, f12, f22), 2, 2)))
}


newton <- function(f3, x0, tol = 1e-9, n.max = 100) { # Newton's method for optimisation, starting at x0 # f3 is a function that given x returns the list # {f(x), grad f(x), Hessian f(x)}, for some f
  x <- x0
  f3.x <- f3(x)
  n <- 0
  while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
    x <- x - solve(f3.x[[3]], f3.x[[2]])
    f3.x <- f3(x)
    n <- n + 1
  }
  if (n == n.max) {
    cat('newton failed to converge\n') } else {
      return(x) }
}


### R can evaluate derivatives

Df <- deriv(z ~ sin(x^2/2 - y^2/4)*cos(2*x - exp(y)), c('x', 'y'), func=TRUE, hessian=TRUE)
f3 <- function(x) {
  Dfx <- Df(x[1], x[2])
  f <- Dfx[1]
  gradf <- attr(Dfx, 'gradient')[1,] hessf <- attr(Dfx, 'hessian')[1,,] return(list(f, gradf, hessf))
}

## When cannot find derivatives, either use derivative free methods like Nelder & Mead, 
## or try to estimate the derivatives and Hessian matrix


### example of curve fitting using optim function in R
install.packages("spuRs")
library(spuRs)
richards <- function(t, theta) {
  theta[1]*(1 - exp(-theta[2]*t))^theta[3]
}
  
loss.L2 <- function(theta, age, vol) {
  sum((vol - richards(age, theta))^2)
}
  
loss.L1 <- function(theta, age, vol) {
  sum(abs(vol - richards(age, theta)))
}

trees <- read.csv("~/library/spuRs/resources/data/trees.csv")

tree <- trees[trees$ID=="1.3.11", 2:3]
theta0 <- c(1000, 0.1, 3)
theta.L2 <- optim(theta0, loss.L2, age=tree$Age, vol=tree$Vol)
theta.L1 <- optim(theta0, loss.L1, age=tree$Age, vol=tree$Vol)
par(las=1)
plot(tree$Age, tree$Vol, type="p", xlab="Age", ylab="Volume", + main="Tree 1.3.11")
lines(tree$Age, richards(tree$Age, theta.L2$par), col="blue")
lines(tree$Age, richards(tree$Age, theta.L1$par), col="blue", lty=2)


