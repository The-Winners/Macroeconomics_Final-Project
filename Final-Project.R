# THE FINAL PROJECT
# Author: The Winners
# Date: 10/10/2017


# PRELIMINARY OPERATIONS


# Clear the variables
rm(list = ls())


# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Install packages
packages <- c("tidyverse", "rsdmx", "eurostat", "tbl2xts",
              "tidyquant", "BCDating", "pwt10", "dplyr",
              "stargazer", "car", "forecast", "tseries",
              "quantmod", "eurostat", "stargazer",
              "skedastic","Metrics","mFilter", "aTSA","lmtest","xts", "prediction")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

# Load packages
library(quantmod)
library(eurostat)
library(FKF)
library(arfima)


#Import data
# Download inflation rates from Fred
freddata <- c("PCEPI")
for (i in 1:length(freddata)) {
  getSymbols(freddata[i], src = "FRED")
}
plot(PCEPI)

#Check for NA values
nas=c(which(is.na(PCEPI)))
nas

#Check for null values
zeroes=c(which(PCEPI==0))
zeroes

#Transform PCEPI in PCE inflation rates
#which is a timeseries with monthly yoy
#inflation rates

inflationus=c()
for(i in 13:length(PCEPI)){
  inflationus[i]=((as.numeric(PCEPI[i])-as.numeric(PCEPI[i-12]))/(as.numeric(PCEPI[i-12])))*100
}
plot(inflationus, type="l")
for(i in 13:length(PCEPI)){
  PCEPI[i]=inflationus[i]
}
PCEPI[1:12]=NA
PCEPI=na.omit(PCEPI)


plot(PCEPI, type="l")


#Store Canada, Norway, Sweden, and UK CPI data


#Canada
canadacpi=read.csv("CanadaCPI3.csv", sep=",")
canadacpi <- canadacpi %>%
  filter(Products.and.product.groups == "All-items") %>%
  filter(GEO == "Canada")
time=canadacpi[,1]
cutcanadacpi=as.data.frame(canadacpi[,11])
cutcanadacpi[,2]=canadacpi[,1]
cutcanadacpi[,3]=cutcanadacpi[,2]
cutcanadacpi[,2]=cutcanadacpi[,1]
cutcanadacpi[,1]=cutcanadacpi[,3]
cutcanadacpi[,3]=NULL
names(cutcanadacpi)=c("Date","Rate")
cpican=cutcanadacpi


inflationcan=c()
for(i in 13:length(cpican$Rate)){
  inflationcan[i]=((as.numeric(cpican$Rate[i])-as.numeric(cpican$Rate[(i-12)]))/(as.numeric(cpican$Rate[(i-12)])))*100
}
plot(inflationcan, type="l")
for(i in 13:length(cpican$Rate)){
  cpican$Rate[i]=inflationcan[i]
}
cpican[1:12,]=NA
cpican=na.omit(cpican)
cpican=cpican[25:length(cpican$Rate),]

plot(cpican$Rate, type="l")


#Norway
norwaycpi=read.csv("Norway-CPI.csv", sep=";")
norwaydata=matrix(NA,1,2)
norwaydata=as.data.frame(norwaydata)
names(norwaydata)=c("Date","Rate")
count=1
for (i in 2:length(norwaycpi$X)){
  for (s in 12:1){
    norwaydata[count,2]=norwaycpi[i,2+s]
    count=count+1
  }
}
norwaydata$Rate=rev(norwaydata$Rate)
plot(norwaydata$Rate)
norwaydata$Date <- data.frame(time = seq(as.Date('1929-01-01'), by = 'months', length = 1116))

#inflrate=function(cpi){
#  inflation=c()
#  
#}




inflationnor=c()
for(i in 13:length(norwaydata$Rate)){
  inflationnor[i]=((as.numeric(norwaydata$Rate[i])-as.numeric(norwaydata$Rate[(i-12)]))/(as.numeric(norwaydata$Rate[(i-12)])))*100
}
plot(inflationnor, type="l")
for(i in 13:length(norwaydata$Rate)){
  norwaydata$Rate[i]=inflationnor[i]
}
norwaydata[1:12,]=NA
norwaydata=na.omit(norwaydata)
norwaydata=norwaydata[757:1116,]

plot(norwaydata$Date,norwaydata$Rate, type="l")



#Sweden
swedencpi=read.csv("Sweden-CPI.csv", sep=" ")
names(swedencpi)=c("Date","Rate")
swedencpi=swedencpi[73:length(swedencpi$Date),]


#UK
ukcpi=read.csv("UK-CPI.csv", sep=",")
ukcpi=ukcpi[222:length(ukcpi$Title),]
names(ukcpi)=c("Date","Rate")


#EU (from 1999 onward)
eucpi=read.csv("eu-CPI.csv", sep=",")
eucpi=eucpi[4466:4770,7:8]
eucpi=eucpi[25:length(eucpi$TIME_PERIOD),]

#Germany (from 1993 to 1999)
gercpi=read.csv("germany-CPI.csv", sep=";")
gercpi=gercpi[,c(1,2,4)]
names(gercpi)=c("Date","Date2","Rate")





plot(cpican$Rate, type="l", col="blue", dev="svg")
lines(norwaydata$Rate, type="l", col="red")
lines(gercpi$Rate, type="l", col="green")
lines(eucpi$OBS_VALUE, type="l", col="yellow")
lines(ukcpi$Rate, type="l", col="pink")
lines(swedencpi$Rate, type="l", col="grey")
lines(PCEPI)


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

kalmanfilter <- function(y){
    theta <- c(ar = c(0,0), ma1 = 0, sigma = 1)
    fit <- optim(theta, objective, yt = rbind(y), hessian = TRUE)
    p <- cbind(
           estimate = fit$par,
           lowerCI = fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))),
           upperCI = fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))
    sp <- arma21ss(theta["ar1"], theta["ar2"], theta["ma1"], theta["sigma"])
    ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
            Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = rbind(y))
    plot(ans, type = "acf")
    sm <- fks(ans)
    plot(sm)
    lines(y,col="black", lty="dotted")
    return(p)
}
print("Canada")
kalmanfilter(cpican$Rate)

print("Sweden")
kalmanfilter(as.numeric(swedencpi$Rate))

print("Norway")
kalmanfilter(as.numeric(norwaydata$Rate[1:348]))#ci sono dei NA

print("Germany")
kalmanfilter(gercpi$Rate)

print("Euro")
kalmanfilter(eucpi$OBS_VALUE)

print("UK")
kalmanfilter(as.numeric(ukcpi$Rate))

print("US")
kalmanfilter(as.numeric(PCEPI))



