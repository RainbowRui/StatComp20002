## ----eval=FALSE---------------------------------------------------------------
#  function(N, start){
#    # generates a Metropolis chain for multinomial distribution
#    # with random walk proposal distribution and starting value start
#    sizes <- c(125,18,20,34)
#    x <- rep(0, N)
#    x[1] <- start
#    u <- runif(N)
#    v <- runif(N,-0.25,0.25)
#    prob <- function(theta){
#      p <- c(0.5+theta/4,(1-theta)/4,(1-theta)/4,theta/4)
#    }# the probabilities of the corresponding multinomial distribution
#    prob.ratio <- function(x1,x2){
#      prod(prob(x1)^sizes/prob(x2)^sizes)
#    }
#  
#    for (i in 2:N){
#      xt <- x[i-1]
#      y <- xt + v[i] # candidate point
#      r <- min(prob.ratio(y,xt),1)
#      if (!is.nan(r) && u[i] <= r){
#        x[i] <- y
#      }else{
#        x[i] <- xt
#      }
#    }
#    return(x)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  function(n,theta=0,eta=1){
#    x <- numeric(n)
#    u <- runif(n)
#    x[1] <- rnorm(1)
#    k <- 0
#    # cauchy functions
#    f <- function(x, theta=1, eta=0){
#      out <- 1/(pi * theta * (1+((x-eta)/theta)^2))
#      return(out)
#    }
#  
#    for(i in 2:n){
#      xt <- x[i-1]
#      y <- rnorm(1,mean=xt)
#      R <- f(y)*dnorm(xt,mean=y)/(f(xt)*dnorm(y,mean=xt))
#      if(u[i] <= R){
#        x[i] <- y
#      }else{
#        x[i] <- xt
#        k <- k+1
#      }
#    }
#    return(x)
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp20002)
example_Metropolis = SampleChain(N=10, start=1)
example_Cauchy = rcauchyMH(n=10, theta=0, eta=1)
print(example_Metropolis)
print(example_Cauchy)
plot(example_Metropolis,type='l')

