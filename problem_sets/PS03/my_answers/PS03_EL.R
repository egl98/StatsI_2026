#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)
install.packages("ggplot2")
library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
# Reviewing Data
inc.sub


# Question 1 - voteshare + difflog
# Question 1.a - regression 
reg1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(reg1)
# Explanation: Holding all things constant, on average for every unit increase in difference in campaign spending, the incumbent and challenger's vote share increases by .042. 

# Question 1.b -  scatter plot 
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Spending Difference",
     ylab = "Inumbent voter share",
     main = "Incumbent vote share vs. Spending differene",
     pch = 16, col = "gray")
abline(reg1, col = "red",lwd = 2)

# Question 1.c -  Save the residuals of the model.
res1 <- residuals(reg1)
summary(res1)
stargazer(data.frame(res1),
          type = "latex",
          title = "Summary of Residuals",
          digits = 4,
          out = "residuals_table3.tex")

# Question 1.d prediction equation 
# voteshare = 0.58 + 0.042 X difflog

# Question 2 - presvote + difflog
# Question 2.a - Regression
reg2 <- lm(presvote ~ difflog, data = inc.sub)
summary(reg2)
# Explanation: Holding all things constant, on average for every unit increase in difference in campaign spending, the presidential candidate vote share for the incumbent's party increases by .023. 


# Question 2.b - Scatterplot
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "Spending Difference",
     ylab = "Pres.Incumbent",
     main = "Incumbent vote share vs. Pres.Incumbent",
     pch = 16, col = "gray")
abline(reg2, col = "red",lwd = 2)

# Question 2.c - Residuals 
res2 <- residuals(reg2)
summary(res2)
stargazer(data.frame(Residuals = res2),
          type = "latex",
          title = "Summary of Residuals",
          digits = 4,
          out = "residuals_table2.tex")

# Question 2.d - Prediction Equation
# voteshare = -0.21 + .023 X presvote

# Question 3 - voteshare + presvote
# Question 3.a - Regression 
reg3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(reg3)
# Explanation: Holding all things constant, on average for every unit increase of the vote share, the presidential candidate vote share for the incumbent's party increases by .38

# Question 3.b - Scatterplot 
plot(inc.sub$voteshare, inc.sub$presvote,
     xlab = "Spending Difference",
     ylab = "Pres.Incumbent",
     main = "Incumbent vote share vs. Pres.Incumbent",
     pch = 16, col = "gray")
abline(reg3, col = "red",lwd = 2)

#Adding the residual 
res3 <- residuals(reg3)
summary(res3)

# Question 3.c - Prediction Equation 
# voteshare = .044 + .38 X voteshare


# Question 4 - Regression on residuals + explanatory variable
reg4 <- lm(res1 ~ res2)
summary(reg4)
# Explanation: When the residuals from question 1 and 2 are use, there is a -6.12 variation that is not explained between the incumbent an the challenger.  
# Question 4.b - Scatterplot 
plot(res1, res2,
     xlab = "Res 1",
     ylab = "Res 2",
     main = " Res 1 vs. Res 2",
     pch = 16, col = "gray")
abline(reg4, col = "red",lwd = 2)

# Question 4.c - Prediction Equation 
# voteshare = -6.5 + -6.1 X res 1

# Question 5 - President Popularity, Spend, and incumbent/challenger 
# Question 5.a - Regression 
reg5 <-lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(reg5)

# Question 5.b
# voteshare = .44 +.035 X difflog + .25 X presvote


# Question 5.c - Output comparison 
# The residuals for the outputs of questions 4 and 5 are identical. I believe this is because regressing one set of residuals on another isolates the variation that remains unexplained by the other variables. When additional variables are added to the regression, they account for that remaining variation, so the residuals end up the same.
