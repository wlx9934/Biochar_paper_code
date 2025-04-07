#packageVersion("rhdf5")
#install.packages("simba")
library(usethis)
library(devtools)
#install.packages("rhdf5")
#require("devtools")
#devtools::install_github(repo = 'zellerlab/SIMBA')
#library(rhdf5)
#library(phyloseq)
#devtools::install_local("D:/R-4.3.1/library/SIMBA-main.zip")

df<-openxlsx::read.xlsx("./NC.xlsx")

# Slope difference of linear model
diffslope(df$`NUn`, df$Cun, df$`Nbu`, df$Cbu)

# The intercept difference of linear models
diffic(df$`NUn`, df$Cun, df$`Nbu`, df$Cbu)
