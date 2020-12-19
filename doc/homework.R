## ----random, echo=TRUE--------------------------------------------------------
ctl <- c(6.21,3.72,4.37,5.49,3.12,5.34,7.20,5.07,4.10,3.36)
trt <- c(5.72,5.28,3.06,4.77,6.31,4.28,7.19,3.71,5.45,3.75)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
plot(lm.D9)

## ----table, echo=TRUE---------------------------------------------------------
knitr::kable(head(iris))

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----cars---------------------------------------------------------------------
summary(cars)

## ----pressure, echo=FALSE-----------------------------------------------------
plot(pressure)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
library(Pareto)
my_pareto <- function(n, a, b){
  u <- runif(n)
  x <- b*((1-u)^(-1/a))
  return(x)
}

## -----------------------------------------------------------------------------
set.seed(1000)
n <- 1e3; a <- 2; b <- 2; xmin_ <- 2; xmax_ <- 20
X_mine <- my_pareto(n, a, b)
X_gt <- rPareto(n, a, b)

## ---- fig.show="hide"---------------------------------------------------------
p1 <- hist(X_mine, breaks=500, prob = TRUE)
p2 <- hist(X_gt, breaks=500, prob = TRUE)

## ---- fig.width=12, fig.height=8----------------------------------------------
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE),widths=c(1,1),height=c(1,1))
plot(p1, col=rgb(0,0,1,1/4), xlim=c(0,20), xaxt="n", yaxt="n", main="Histogram of my_pareto and rpareto", ylab="Density", xlab="X")
plot(p2, col=rgb(1,0,0,1/4), xlim=c(0,20), xaxt="n", yaxt="n", add=TRUE)
axis(side=1,at=seq(0,20,1))
axis(side=2,at=seq(0,50,5))
plot(p1, xlim=c(0,20), main="Histogram of my_pareto", ylab="Density", xlab="X")
plot(p2, xlim=c(0,20), main="Histogram of rpareto", ylab="Density", xlab="X")

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
my_kernel <- function(n){
  set.seed(1); u1 <- runif(n, -1, 1)
  set.seed(2); u2 <- runif(n, -1, 1)
  set.seed(3); u3 <- runif(n, -1, 1)
  # compare |u3| and |u2|
  compare_32 <- (abs(u3) >= abs(u2)) + 0
  # compare |u3| and |u1|
  compare_31 <- (abs(u3) >= abs(u1)) + 0
  # record the position of "(|u3| >= |u2|) && (|u3| >= |u1|)"
  compare_final <- ((compare_32+compare_31)==2) + 0
  return(u2*compare_final + u3*(1-compare_final))
}

## ---- fig.width=6, fig.height=4-----------------------------------------------
n <- 1e6
X <- my_kernel(n)
hist(X, breaks=200, prob = TRUE, main = expression("Simulated random variates"))
Y <- seq(-1,1,.01)
lines(Y, 3/4*(1-Y^2),col="dark red", lwd=2)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
n <- 1e3; r <- 4; beta <- 2
set.seed(313); lambda <- rgamma(n, r, beta)
set.seed(31313); x <- rexp(lambda)

## -----------------------------------------------------------------------------
library(Pareto)
xfit <- seq(1/n, 10, length=n)
y1 <- dPareto(xfit+beta, r, beta)
y2 <- dPareto(xfit+beta, 2, beta)

## ---- fig.height=6------------------------------------------------------------
hist(x, breaks=100, xlim=c(0,10), prob = TRUE)
lines(xfit, y1, col="blue", lwd=2)
lines(xfit, y2, col="red", lwd=2)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
set.seed(819); m <- 1e5; t <- runif(m, min=0, max=pi/3)
theta.hat <- mean(sin(t)) * pi / 3

## -----------------------------------------------------------------------------
print(c(theta.hat, 1/2))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
set.seed(819); m <- 1e6; x1 <- runif(m, min=0, max=1)
theta_hat_1 <- exp(x1)

## -----------------------------------------------------------------------------
set.seed(521); m <- 5e5; x2 <- runif(m, min=0, max=1);
theta_hat_2 <- (exp(x2) + exp(1-x2)) / 2

## -----------------------------------------------------------------------------
print(c(var(theta_hat_2) / var(theta_hat_1)))

## -----------------------------------------------------------------------------
print(c(var(theta_hat_2), (var(exp(x2))+var(exp(1-x2)))/4))

## -----------------------------------------------------------------------------
print(c(mean(theta_hat_1), mean(theta_hat_2), exp(1)-1))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ----fig.width=10-------------------------------------------------------------
  x <- seq(1,10,.01)
  w <- 2
  g <- x^2 * exp(-x^2 / 2) / sqrt(2 * pi) # g(x)
  f0 <- rep(1, length(x)) # f0(x)
  f1 <- exp(-x) # f1(x)
  gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
          expression(f[0](x)==1),
          expression(f[1](x)==e^{-x}))
  
  # for color change lty to col
  par(mfrow=c(1,2))
  # figure (1)
  plot(x, g, type = "l", ylab = "",
       ylim = c(0,2), lwd = w, col = 1, main='(1)')
  lines(x, f0, lty = 2, lwd = w, col = 2)
  lines(x, f1, lty = 3, lwd = w, col = 3)
  legend("topright", legend = gs, lty = 1:3, lwd = w, inset = 0.02, col = 1:3)
  # figure (2)
  plot(x, g/f0, type = "l", ylab = "", ylim = c(0,3.2), lwd = w, lty = 2, col = 2, main = '(2)')
  lines(x, g/f1, lty = 3, lwd = w, col = 3)
  legend("topright", legend = gs[-1], lty = 2:3, lwd = w, inset = 0.02, col = 2:3)

## -----------------------------------------------------------------------------
  set.seed(520)
  x_min <- 1
  x_max <- 1000
  m <- 10000
  theta.hat <- se <- var <- numeric(2)
  g <- function(x, x_min_, x_max_) {
    x^2 * exp(-x^2/2) / sqrt(2*pi) * (x > x_min_) * (x <= x_max_)
  } # define g(x)
  x <- runif(m, x_min, x_max) # using f0
  fg <- g(x, x_min, x_max)
  theta.hat[1] <- mean(fg)
  se[1] <- sd(fg)
  var[1] <- var(fg)
  x <- rexp(m, 1) # using f1
  fg <- g(x, x_min, x_max) / exp(-x)
  theta.hat[2] <- mean(fg)
  se[2] <- sd(fg)
  var[2] <- var(fg)
  print(c(round(var, 6))) # the variances of f0 and f1

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ----fig.width=10-------------------------------------------------------------
  x <- seq(0,1,.01)
  w <- 2
  g <- exp(-x) / (1 + x^2) # g(x)
  f0 <- (1 / pi) / (1 + x^2) # f0(x)
  f1 <- exp(-x) / (1 - exp(-1)) # f1(x)
  gs <- c(expression(g(x)==e^{-x}/(1+x^2)),
          expression(f[0](x)==1/pi/(1+x^2)),
          expression(f[1](x)==e^{-x}/(1-e^{-1})))
  
  # for color change lty to col
  par(mfrow=c(1,2))
  # figure (1)
  plot(x, g, type = "l", ylab = "",
       ylim = c(0,2), lwd = w, col = 1, main='(1)')
  lines(x, f0, lty = 2, lwd = w, col = 2)
  lines(x, f1, lty = 3, lwd = w, col = 3)
  legend("topright", legend = gs, lty = 1:3, lwd = w, inset = 0.02, col = 1:3)
  # figure (2)
  plot(x, g/f0, type = "l", ylab = "", ylim = c(0,3.2), lwd = w, lty = 2, col = 2, main = '(2)')
  lines(x, g/f1, lty = 3, lwd = w, col = 3)
  legend("topright", legend = gs[-1], lty = 2:3, lwd = w, inset = 0.02, col = 2:3)

## -----------------------------------------------------------------------------
  set.seed(520)
  m <- 10000
  theta.hat <- se <- var <- numeric(2)
  g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
  }
  x <- rcauchy(m) # using f0
  i <- c(which(x > 1), which(x < 0))
  x[i] <- 2
  fg <- g(x) / dcauchy(x)
  theta.hat[1] <- mean(fg)
  se[1] <- sd(fg)
  var[1] <- var(fg)
  u <- runif(m) # using f1, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat[2] <- mean(fg)
  se[2] <- sd(fg)
  var[2] <- var(fg)
  print(c(round(var, 6))) # the variances of f0 and f1

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
  m <- 10000
  k_max <- 10
  N <- 50 # number of times to repeat the estimation
  est <- matrix(0, N, 1)
  for (i in 1:N){
    count <- 0.0
    for (j in 1:k_max){
      x <- runif(m, j, j+1)
      y <- x^2 * exp(-x^2/2) / sqrt(2*pi)
      count <- count + mean(y)
    }
    est[i, 1] <- count
  }
  apply(est, 2, mean)

## -----------------------------------------------------------------------------
  m <- 10000; k <- 10
  r <- m/k # replicates per stratum
  N <- 50 # number of times to repeat the estimation
  T2 <- numeric(k)
  est <- matrix(0, N, 1)
  g<-function(x)exp(-x)/(1+x^2)*(x>0)*(x<1)
  for (i in 1:N) {
    for(j in 1:k)T2[j]<-mean(g(runif(m/k,(j-1)/k,j/k)))
    est[i, 1] <- mean(T2)
  }
  apply(est, 2, mean)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
  set.seed(819)
  M <- 10000; k <- 50
  r <- M / k
  N <- 100 # number of times to repeat the estimation
  T2 <- numeric(k)
  est <- matrix(0, N, 2)
  g<-function(x)exp(-x)/(1+x^2)*(x>0)*(x<1)
  for (i in 1:N) {
    est[i, 1] <- mean(g(runif(M)))
    for(j in 1:k)T2[j]<-mean(g(runif(M/k,(j-1)/k,j/k)))
    est[i, 2] <- mean(T2)
  }
  apply(est, 2, mean)
  apply(est, 2, sd)
  apply(est, 2, var)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
  set.seed(520)
  m <- 10000
  X <- rlnorm(m)
  mean <- mean(log(X)); sd <- sd(log(X))
  print(c(mean - 1.96*sd, mean + 1.96*sd))

## -----------------------------------------------------------------------------
  alpha <- 1; beta <- 0
  m <- 1e4; n <- 20; set.seed(520)
  beta.hat <- beta.se <- p.val1 <- numeric(m)
  x <- rexp(n)
  for(i in 1:m){
    y <- alpha + beta * x + rlnorm(n)
    coe <- summary(lm(y~x))$coef
    beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
    p.val1[i] <- coe[2,4]
  }
  p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
  print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
  alpha <- 1; beta <- 0
  m <- 1e4; n <- 20; set.seed(520)
  beta.hat <- beta.se <- p.val1 <- numeric(m)
  x <- rexp(n)
  for(i in 1:m){
    y <- alpha + beta * x + rchisq(n,2)
    coe <- summary(lm(y~x))$coef
    beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
    p.val1[i] <- coe[2,4]
  }
  p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
  print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
alpha <- 1; beta <- 0 # null hypothesis
m <- 1e4; n <- 10; set.seed(520)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- rexp(n)
for(i in 1:m){
  y <- alpha + beta * x + rbeta(n, 1, 1) # beta distribution
  coe <- summary(lm(y~x))$coef
  beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
  p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## -----------------------------------------------------------------------------
alpha <- 1; beta <- -1 # Alternative hypothesis
m <- 1e4; n <- 10; set.seed(520)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- rexp(n)
for(i in 1:m){
  y <- alpha + beta * x + rbeta(n, 1, 1) # beta distribution
  coe <- summary(lm(y~x))$coef
  beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
  p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## -----------------------------------------------------------------------------
alpha <- 1; beta <- -1 # Alternative hypothesis
m <- 1e4; n <- 10; set.seed(520)
beta.hat <- beta.se <- p.val1 <- numeric(m)
x <- rexp(n)
for(i in 1:m){
  y <- alpha + beta * x + rt(n, 1) # t distribution
  coe <- summary(lm(y~x))$coef
  beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
  p.val1[i] <- coe[2,4] # t-test p-value
}
p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05)))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
count5test <- function(x, y)
{
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy)) > 5)) # return 1 (reject H0) or 0 (do not reject H0)
}

## -----------------------------------------------------------------------------
set.seed(520)
n1 <- n2 <- 20
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
m <- 1e3
test_mean <- mean(replicate(m, expr = {
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
x <- x - mean(x) # centered by sample mean
y <- y - mean(y)
count5test(x, y)
}))
print(test_mean)

## -----------------------------------------------------------------------------
set.seed(520)
n1 <- n2 <- 20 * 1e3
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
print(var.test(x, y,conf.level = 0.945)) # at significance level 0.055

## ----echo=FALSE---------------------------------------------------------------
set.seed(520)
n1 <- n2 <- 6
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
m <- 1e3
test_mean <- mean(replicate(m, expr = {
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
x <- x - mean(x) # centered by sample mean
y <- y - mean(y)
count5test(x, y)
}))
print(test_mean)
set.seed(520)
n1 <- n2 <- 6 * 1e3
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
print(var.test(x, y,conf.level = 0.945)) # at significance level 0.055

## ----echo=FALSE---------------------------------------------------------------
set.seed(520)
n1 <- n2 <- 100
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
m <- 1e3
test_mean <- mean(replicate(m, expr = {
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
x <- x - mean(x) # centered by sample mean
y <- y - mean(y)
count5test(x, y)
}))
print(test_mean)
set.seed(520)
n1 <- n2 <- 100 * 1e3
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 1.5
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
print(var.test(x, y,conf.level = 0.945)) # at significance level 0.055

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
library(MVN)

## -----------------------------------------------------------------------------
n <- 5e3
x <- matrix(0,n,2)
set.seed(520)
x[,1] <- rnorm(n)
set.seed(521)
x[,2] <- rnorm(n)
result <- mvn(data = x, mvnTest = "mardia")
print(result$multivariateNormality)

## -----------------------------------------------------------------------------
n <- 5e3
x <- matrix(0,n,2)
set.seed(520)
x[,1] <- rlnorm(n)
set.seed(521)
x[,2] <- rlnorm(n)
result <- mvn(data = x, mvnTest = "mardia")
print(result$multivariateNormality)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(bootstrap) # for the law data

b.cor <- function(x, y, i) cor(x[i], y[i])
# set up the jackknife
n <- nrow(law) # sample size
theta.hat <- b.cor(law$LSAT, law$GPA, 1:n)
theta.jack <- numeric(n) # storage for replicates

# jackknife estimate of standard error of R
for (i in 1:n){
  theta.jack[i] <- b.cor(law$LSAT, law$GPA, (1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat, bias.jack=bias.jack, se.jack=se.jack),3)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- warning=FALSE-----------------------------------------------------------
library(boot)
lambda<-seq(0.5,2,0.1)
interval.norm <- interval.basic <- interval.perc <- interval.bca <- numeric(length(lambda))
m <- 1e2; set.seed(520)
boot.mean <- function(x, i) mean(x[i])
ci.norm <- ci.basic <- ci.perc <- ci.bca <- matrix(NA, m, 2)
# R <- aircondit$hours
n <- 1e1
for (j in 1:length(lambda)){
  mu <- 1 / lambda[j]
  for (i in 1:m){
    R <- rexp(n, lambda[j])
    de <- boot(data = R, statistic = boot.mean, R = 999)
    ci <- boot.ci(de, type=c("norm","basic","perc","bca"))
    ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
    ci.perc[i,]<-ci$percent[4:5];ci.bca[i,]<-ci$bca[4:5]
  }
  interval.norm[j] <- mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu)
  interval.basic[j] <- mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu)
  interval.perc[j] <- mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu)
  interval.bca[j] <- mean(ci.bca[,1]<=mu & ci.bca[,2]>=mu)
}
plot(lambda, interval.norm, type = "l", xlab = "lambda", ylim = c(0.75,0.95), lty=1)
lines(lambda, interval.basic, col='red', lty=2)
lines(lambda, interval.perc, col='blue', lty=3)
lines(lambda, interval.bca, col='green', lty=4)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(bootstrap)
print(names(scor))
n <- 88
X<- matrix(NA, n, 5)
X[,1] <- scor$mec
X[,2] <- scor$vec
X[,3] <- scor$alg
X[,4] <- scor$ana
X[,5] <- scor$sta
Cov_X <- matrix(NA, 5, 5)

# calculate theta.hat
for (i in 1:5){
  for (j in 1:5){
    Cov_X[i,j] = cov(X[,i], X[,j])
  }
}
eigen_value <- eigen(Cov_X)$val
theta.hat <- eigen_value[1] / sum(eigen_value)

# calculate theta.jack
theta.jack <- numeric(n)
for (k in 1:n){
  New_X <- X[-k,]
  for (i in 1:5){
    for (j in 1:5){
      Cov_X[i,j] = cov(New_X[,i], New_X[,j])
    }
  }
  eigen_value <- eigen(Cov_X)$val
  theta.jack[k] <- eigen_value[1] / sum(eigen_value)
}

# calculate the jackknife estimates of bias and standard error of theta_hat
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)

# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
  J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
  yhat4 <- exp(logyhat4)
  e4[k] <- magnetic[k] - yhat4
}
print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1))

# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  for (j in 1:(n-1)){
    x <- x[-j]
    y <- y[-j]
    index <- (k-1)*(n-1) + j
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[index] <- magnetic[k] - yhat1
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
    e2[index] <- magnetic[k] - yhat2
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[index] <- magnetic[k] - yhat3
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[index] <- magnetic[k] - yhat4
  }
}
print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 10000
alphahat <- mean(replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x) #centered by sample mean
  y <- y - mean(y)
  count5test(x, y)
  }))
print(alphahat)

## -----------------------------------------------------------------------------
n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 10000
alphahat <- mean(replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  k <- sample(n2, size=n2-n1, replace=FALSE)
  y <- y[-k]
  x <- x - mean(x) #centered by sample mean
  y <- y - mean(y)
  count5test(x, y)
  }))
print(alphahat)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)
# illustration of NN test
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}

## -----------------------------------------------------------------------------
n1 <- 50
n2 <- 50
mu1 <- mu2 <- 0
sigma1 <- 1; sigma2 <- 4
set.seed(520)
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
z <- c(x,y); N <- c(length(x), length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
                 sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
p1.value <- mean(ts>=ts[1])
print(p1.value) # NN test

boot.obs <- eqdist.etest(z, sizes=N, R=999)
p2.value <- boot.obs$p.value
print(p2.value) # energy test

p3.value = bd.test(x = x, y = y, R=999)
print(p3.value$p.value) # Ball test

## -----------------------------------------------------------------------------
n1 <- 50
n2 <- 50
mu1 <- -1; mu2 <- 1
sigma1 <- 1; sigma2 <- 4
set.seed(520)
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
z <- c(x,y); N <- c(length(x), length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
                 sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
p1.value <- mean(ts>=ts[1])
print(p1.value) # NN test

boot.obs <- eqdist.etest(z, sizes=N, R=999)
p2.value <- boot.obs$p.value
print(p2.value) # energy test

p3.value = bd.test(x = x, y = y, R=999)
print(p3.value$p.value) # Ball test

## -----------------------------------------------------------------------------
n1 <- 50
n2 <- 50
set.seed(520)
x <- rt(n=n1*2, df=1)
y <- c(rnorm(n2, 0, 1), rnorm(n2, 0, 4))
z <- c(x,y); N <- c(length(x), length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
                 sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
p1.value <- mean(ts>=ts[1])
print(p1.value) # NN test

boot.obs <- eqdist.etest(z, sizes=N, R=999)
p2.value <- boot.obs$p.value
print(p2.value) # energy test

p3.value = bd.test(x = x, y = y, R=999)
print(p3.value$p.value) # Ball test

## -----------------------------------------------------------------------------
n1 <- 10
n2 <- 100
set.seed(520)
mu1 <- 0; mu2 <- 0
sigma1 <- 1; sigma2 <- 1
set.seed(520)
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
z <- c(x,y); N <- c(length(x), length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
                 sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
p1.value <- mean(ts>=ts[1])
print(p1.value) # NN test

boot.obs <- eqdist.etest(z, sizes=N, R=999)
p2.value <- boot.obs$p.value
print(p2.value) # energy test

p3.value = bd.test(x = x, y = y, R=999)
print(p3.value$p.value) # Ball test

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ----warning=FALSE------------------------------------------------------------
library(LaplacesDemon)
rw.Metropolis <- function(m, s, sigma, x0, N)
{
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N)
  {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dlaplace(y, m, s) / dlaplace(x[i-1], m, s)))
      x[i] <- y
    else
    {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x, k=k))
}

## -----------------------------------------------------------------------------
m <- 0; s <- 1
N <- 1000
sigma <- c(.005, .05, .5, 1, 5, 10)
x0 <- 0
rw1 <- rw.Metropolis(m, s, sigma[1], x0, N)
rw2 <- rw.Metropolis(m, s, sigma[2], x0, N)
rw3 <- rw.Metropolis(m, s, sigma[3], x0, N)
rw4 <- rw.Metropolis(m, s, sigma[4], x0, N)
rw5 <- rw.Metropolis(m, s, sigma[5], x0, N)
rw6 <- rw.Metropolis(m, s, sigma[6], x0, N)
#number of candidate points rejected
print(c(rw1$k, rw2$k, rw3$k, rw4$k, rw5$k, rw6$k))

## -----------------------------------------------------------------------------
par(mfrow=c(2,3)) #display 6 graphs together
refline <- qlaplace(c(.025, .975), location=m, scale=s)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x, rw5$x, rw6$x)
for (j in 1:6)
{
  plot(rw[,j], type="l",
    xlab=bquote(sigma == .(round(sigma[j],3))),
    ylab="X", ylim=range(rw[,j]))
  abline(h=refline)
}
par(mfrow=c(1,1)) #reset to default

## ----warning=FALSE------------------------------------------------------------
library(xtable)
a <- c(.05, seq(.1, .9, .1), .95)
Q <- qlaplace(a, m, s)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x, rw5$x, rw6$x)
mc <- rw[501:N, ]
Qrw <- apply(mc, 2, function(x) quantile(x, a))
print(round(cbind(Q, Qrw), 3))

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi)
{
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)
  B <- n * var(psi.means)
  psi.w <- apply(psi, 1, "var")
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + (B/n)
  r.hat <- v.hat / W # G-R statistic
  return(r.hat)
}

## ----warning=FALSE------------------------------------------------------------
library(LaplacesDemon)
laplace.chain <- function(scale, sigma, N, X1)
{
  # generates a Metropolis chain for the standard Laplace distribution
  # with Normal(X[t], sigma) proposal distribution
  # and starting value X1
  x <- rep(0, N)
  x[1] <- X1
  u <- runif(N)
  for (i in 2:N)
  {
    xt <- x[i-1]
    y <- rlaplace(1, xt, scale) # Laplace distribution
    r1 <- dnorm(y, 0, 1) * dnorm(xt, y, sigma)
    r2 <- dnorm(xt, 0, 1) * dnorm(y, xt, sigma)
    r <- r1 / r2
    if (u[i] <= r)
    {
      x[i] <- y
    }
    else
    {
      x[i] <- xt
    }
  }
  return(x)
}

scale <- 1.
sigma <- .5
k <- 6
n <- 10000
b <- 1000
x0 <- c(-10, -5, -1, 1, 5, 10)
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- laplace.chain(scale, sigma, n, x0[i])

psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))

par(mfrow=c(2,3))
for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",
    xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1))

rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
f <- function(a, k) {
  x1 <- sqrt(a*a*k/(k+1-a*a))
  x2 <- sqrt(a*a*(k-1)/(k-a*a))
  
  return (pt(x1, 1) - pt(x2, 1))
}

## -----------------------------------------------------------------------------
k_vals <- c(4:25, 100, 500, 1000)
for (k in k_vals){
  res <- uniroot(f, c(0.001,sqrt(k)-0.001), k)
  print(unlist(res)[1:3])
}

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
p=1/3; q=1/3; r=1/3
x=0; y=0
for (i in 1:10){
  x=444*p*p/(p*p+2*r*p)
  y=132*q*q/(q*q+2*r*q)
  p=(2*x+(444-x)+63)/(2*1000)
  q=(2*y+(132-y)+63)/(2*1000)
  r=(2*361+(444-x)+(132-y))/(2*1000)
  lnf=-(444*log(p*p+2*p*r)+132*(q*q+2*q*r)+2*361*r+63*log(2*p*q))
  print(c(p,q,r, lnf))
}

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
# the formulas
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

## -----------------------------------------------------------------------------
# use lapply()
lapply(formulas, lm, data = mtcars)
# use for loops
formulas_2 <- vector('list', length(formulas))
for(i in seq_along(formulas)) {
    formulas_2[[i]] <- lm(formulas[[i]], data = mtcars)
}
formulas_2

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

## -----------------------------------------------------------------------------
# use sapply() and an anonymous function to extract the p-value from every trial
sapply(trials, function(x) x$p.value) # function(x): an anonymous function

## -----------------------------------------------------------------------------
# get rid of the anonymous function by using [[ directly
sapply(trials, `[[`, 'p.value')

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## ---- eval = FALSE------------------------------------------------------------
#  # a combination of Map() and vapply()
#  mcvMap <- function(x, f, FUN.VALUE, ...){
#    vapply(x, Map(f, ...), FUN.VALUE)
#  }

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
lap_f = function(x) exp(-abs(x)) # the Laplace function
# the function of standard Laplace distribution
rw.Metropolis = function(sigma, x0, N){
  x = numeric(N)
  x[1] = x0
  u = runif(N)
  k = 0
  for (i in 2:N){
    y = rnorm(1, x[i-1], sigma)
    if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) # compare
      x[i] = y 
    else{
      x[i] = x[i-1]
      k = k+1
    }
  }
  return(list(x = x, k = k))
}

## -----------------------------------------------------------------------------
library(Rcpp)
sourceCpp(
  code = '
    #include <Rcpp.h>
    #include <random>
    using namespace Rcpp;
    
    double lap(double x) {
      return exp(-abs(x));
    }
    
    // [[Rcpp::export]]
    List metropolisC(double sigma, double x0, int N) {
      std::default_random_engine generator_u;
      std::default_random_engine generator_n;
      std::uniform_real_distribution<double> distribution_u(0.0,1.0);
      NumericVector x(N);
      x[0] = x0;
      NumericVector u(N);
      for (int i = 0; i < N; i++) {
        u[i] = distribution_u(generator_u);
      }
      int k = 0;
      for (int i = 1; i < N; i++) {
        std::normal_distribution<double> distribution_n(x[i-1], sigma);
        double y = distribution_n(generator_n);
        if (u[i] <= (lap(y) / lap(x[i-1])))
          x[i] = y;
        else {
          x[i] = x[i-1];
          k += 1;
        }
      }
      List out(2);
      out[0] = x;
      out[1] = k;
      return out;
    }
  '
)

## -----------------------------------------------------------------------------
set.seed(819)
N = 1e3; sigma = 1.0; x0 = 25.0;
rw_r = rw.Metropolis(sigma = sigma, x0 = x0, N = N)
rw_c = metropolisC(sigma, x0, N)
qqnorm(rw_r$x, pch = 1, frame = FALSE)
qqnorm(rw_c[[1]], pch = 1, frame = FALSE)

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(MetropolisR=rw.Metropolis(sigma, x0, N), MetropolisC=metropolisC(sigma, x0, N))
    summary(ts)[,c(1,3,5,6)]

## ---- echo=FALSE--------------------------------------------------------------
rm(list=ls())

