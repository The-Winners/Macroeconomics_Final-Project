library(FKF)
library(arfima)
log_exp_target = function(x){
  return(dexp(x,rate=1, log=TRUE))
}

#y <- arfima.sim(100, model = list(phi = .2, dfrac = .3, dint = 2))
n <- 1000

## Set the AR parameters
ar1 <- 0.8
ar2 <- 0.2
ma1 <- -0.2
sigma <- sqrt(2)

y <- arima.sim(model = list(ar = c(ar1, ar2), ma = ma1), n = n,
               innov = rnorm(n) * sigma)

arma21ss <- function(ar1, ar2, ma1, sigma) {
    Tt <- matrix(c(ar1, ar2, 1, 0), ncol = 2)
    Zt <- matrix(c(1, 0), ncol = 2)
    ct <- matrix(0)
    dt <- matrix(0, nrow = 2)
    GGt <- matrix(0)
    H <- matrix(c(1, ma1), nrow = 2) * sigma
    HHt <- H %*% t(H)
    a0 <- c(0, 0)
    P0 <- matrix(1e6, nrow = 2, ncol = 2)
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,
                HHt = HHt))
}
objective <- function(theta, yt) {
    sp <- arma21ss(theta["ar1"], theta["ar2"], theta["ma1"], theta["sigma"])
    ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
               Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
    return(-ans$logLik)}

kalmanfilter <- function(){
    theta <- c(ar = c(0,0), ma1 = 0, sigma = 1)
    fit <- optim(theta, objective, yt = rbind(y), hessian = TRUE)
    p <- cbind(actual = c(ar1=ar1,ar2=ar2,ma1=ma1,sigma=sigma),
           estimate = fit$par,
           lowerCI = fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))),
           upperCI = fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))
    print(p)
    sp <- arma21ss(1-d, d, fit$par["ma1"], fit$par["sigma"])
    ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
            Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = rbind(y))
    return(ans)
}
best<-kalmanfilter()

likelihood = function(param){
    likelihoods=c()
    #param a vector contaning (d,sigma) if sigma or d is free set equal to 0
    d<- c(seq(0.1,0.5,by=0.1))
    sigma <- c(seq(1,10,by=1))
    
    likelihoods = append(likelihoods, kalmanfilter(param[1],param[2])$logLik)
    out<-likelihoods[which.max(likelihoods)]
    return(out)
}



posterior = function(param,d){
    options(digits=6)
    like <- likelihood(param)
    if (d==TRUE) {
       prior<- punif(param[1],0,0.5)
    } else {
       prior <- punif(param[2],0,10)
    }
    post <- exp(like) * prior
    return(post)
}
run_metropolis_MCMC = function(startvalue, iterations){
    chain = array(dim = c(iterations+1,2))
    chain[1,] = startvalue
    
    for (i in 2:iterations){
        chain[i,]<-cbind(runif(1,0,0.5),runif(1,0,10))
        probab = exp(posterior(cbind(chain[i,1],chain[i-1,2]),TRUE) - posterior(chain[i-1,],TRUE))
        probab =min(1,probab)
        if (runif(1) < probab){
            chain[i,1] <- d <-chain[i,1]
        }else{
            chain[i,1] <- d <- chain[i-1,1]
        }

        sigma <- chain[i,2]
        probab = exp(posterior(cbind(d,sigma),FALSE) - posterior(cbind(d,chain[i-1,2]),FALSE))
        probab =min(1,probab)
        if (runif(1) < probab){
            chain[i,2] <- sigma 
        }else{
            chain[i,2] <- chain[i-1,1]
        }

        print(paste("Iteration:",i,"d=",chain[i,1], sep = " ", collapse = NULL))
    }
    return(chain)
}
startvalue = c(0.3,4)
chain = run_metropolis_MCMC(startvalue, 10)
 
burnIn = 9
acceptance = 1-mean(duplicated(chain[-(1:burnIn),1]))
acceptance
