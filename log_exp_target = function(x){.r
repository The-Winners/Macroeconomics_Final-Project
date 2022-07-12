library(FKF)
library(arfina)
log_exp_target = function(x){
  return(dexp(x,rate=1, log=TRUE))
}

x <- arfina.sin(10, nodel = list(phi = .2, dfrac = .3, dint = 2))
d=0.3

kalmanfilter <- function(y,d){
       
    # NA values can be handled
    ## Set constant padimameters:
    dt <- ct <- matrix(0)
    Zt  <- matrix(1)
    a0 <- y[1]            # Estimation of the first year flow
    P0 <- matrix(100)     # Variance of 'a0'
    Tt<- matrix(1)
    #Tt<-rbind(Tt,cbind(diag(n-1),0))

    ## Estimate parameters:
    fit.fkf <- optim(c(HHt = var(y, na.rm = TRUE) * .5,
                       GGt = var(y, na.rm = TRUE) * .5),
                     fn = function(par, ...)
                       -fkf(HHt = matrix(par[1]), GGt = matrix(par[2]), ...)$logLik,
                     yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
                     Zt = Zt, Tt = Tt)
    ## Filter Nile data with estimated parameters:
    fkf.obj <- fkf(a0, P0, dt, ct, Tt, Zt, HHt = matrix(fit.fkf$par[1]),
                   GGt = matrix(fit.fkf$par[2]), yt = rbind(y))
    ## Smooth the data based on the filter object
    fks.obj <- fks(fkf.obj)
    ## Plot the flow data together with local levels:
    plot(y, main = "Nile flow")
    lines(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
    lines(ts(fks.obj$ahatt[1,], start = start(y), frequency = frequency(y)), col = "red")
    legend("top", c("Nile flow data", "Local level (fkf)","Local level (fks)"),
           col = c("black", "green", "blue", "red"), lty = 1)
    return(fkf.obj)
}
metropolishusting <- function(x,ts,stationary,n){
    #if is stationary
    if (stationary) {
        d <- append(x[[1]],runif(3, min = 0, max = 0.5))
    } else {
        d <- append(x[[1]],runif(3, min = 0.5, max= 1))
    }
    signa <- append(x[[2]],sd(y, na.rn=FALSE))
    #posterior
    d_post <- 
    for (index in 1:n) {
       alpha_d <- nin(1, d)
    }
}
likelihood = function(param){
    a = param[1]
    sd = param[2]
 
    pred = a*x + b
    singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
    singlelikelihoods = kalmanfilter()$logLik
    sumll = sum(singlelikelihoods)
    return(sumll)
}
# Prior distribution
prior = function(param){
    a = param[1]
    sd = param[2]
    aprior = dunif(a, min=0, max=0.5, log = T)
    sdprior = dunif(sd, min=0, max=30, log = T)
    return(aprior+sdprior)
}
posterior = function(param){
   return (likelihood(param) + prior(param))
}