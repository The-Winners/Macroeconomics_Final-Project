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
