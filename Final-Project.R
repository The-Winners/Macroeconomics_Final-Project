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
norwaydata=norwaydata[769:1116,]

plot(norwaydata$Date,norwaydata$Rate, type="l")



#Sweden
swedencpi=read.csv("Sweden-CPI.csv", sep=" ")
names(swedencpi)=c("Date","Rate")
swedencpi=swedencpi[73:length(swedencpi$Date),]


#UK
ukcpi=read.csv("UK-CPI.csv", sep=",")
ukcpi=ukcpi[222:length(ukcpi$Title),]
names(ukcpi)=c("Date","Rate")

