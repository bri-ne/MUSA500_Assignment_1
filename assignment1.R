#--- load libraries ----
install.packages("DAAG")
install.packages("car")
install.packages("MASS")
install.packages("rsq")
library(tidyr)
library(dplyr)
library(DAAG)
library(car)  #to calculate VIF
library(MASS)
library(rsq)

#---- Upload Data ----

ourdata <- read.csv("https://raw.githubusercontent.com/bri-ne/MUSA500_Assignment_1/main/RegressionData.csv")

#test for code collaboration 
#second test
