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
library(stargazer)

lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Question 1: Economics
# Data install
data("Prestige")
help("Prestige")
head(Prestige)

# Question 1.a - creating the professional variable (profs = 1, blue/white = 0)
# New prof Variable 

Prestige$prof <- ifelse(Prestige$type == "prof", 1, 0)
Prestige$bw <- ifelse(Prestige$type %in% c("bc", "wc"), 0, 1)


# Question 1.b - linear model with prestige as outcome, income + professional and the
# interaction of the two as predictors ~ continuous dummy interaction

model_1 <- lm(prestige ~ income + prof + income:prof, data = Prestige)
summary(model_1)

stargazer(model_1,
                   type = "latex",
                   title = "Regression Results",
                   digits = 4,
                   out = "model_2.tex")

# Question 1.c - writing prediction equation 
# prestige = 21.1422589 + 0.0031709(income) + 37.7812800(prof) 
# - 0.0023257 (income X prof)

# Question 1.d - interpret the coefficient for income
# For non-professionals (prof = 0), for each dollar increase of income
# there is a 0.0031709 increase of prestige on average. 


# Question 1.e - interpret the coefficient for professional
# For non-professionals (prof = 0), for each unit of professionalism 
# there is a 37.7812800 increase of prestige score, on average. 

# Question 1.f - income of $1.00 increase on prestige score when professional is 1
# Y_Hat_Prestige = 21.1422589*1000 + 37.7812800*1
# - 0.0023257*1000 = 21177.71

# Question 1.g - change in non-professionals to professional (0 - 1) with income of $6000

# Y_Hat_Prof(0) = 21.1422589 + 0.0031709 * 6000 
# + 37.7812800  * 0 - 0.0023257  (6000*0) = 40.16533

# Y_Hat_Prof(1) = 21.1422589 + 0.0031709 * 6000 
# + 37.7812800  * 1 - 0.0023257  * 6000 = 63.99474

# Change in Y_Hat = 37.7812800  - 0.0023257 * 6000


# Question 2 - Political Science 

# Question 2.a
# Regression --> voteshare = 0.302 + 0.042 (assigned) + 0.042 (adjacent) + error
# Hypothesis Test: 
# Stating the Hypothesis --> 
# H0: b equal 0 (lawn signs have no effect on vote share)
# Ha: b not equal  0 (lawn signs have effects in vote share)

# T-test -->
# t = 0.042/0.016 (assigned)
# Answer = 2.625


# Comparing the α = .05 (1.96 critical value)
# 2.625 > 1.96 (reject the hypothesis for assigned)


# At α = .05, we cant reject the null hypothesis that lawn signs have no effect on 
# voteshare. Therefore, we can assume that there is affects on voteshare when signs
# are adjacent. 

# Question 2.b - Next to precincts
# Hypothesis Test
# H0: β = 0 (being next to a precinct with yard signs has no effect on vote share)
# Ha: β ≠ 0 (being next to a precinct with yard signs has an effect on vote share)

# T-Stat
# t = 0.042/0.013 (adjacent)
# Answer = 3.230769
# 3.230 > 1.96 (reject the hypothesis for adjacent)

# At α = .05, we cant reject the null hypothesis that lawn signs have adjacent no effect on 
# voteshare. 

# Question 2.c - Constant interpret
# When everything is held at zero, the constant of 0.302 (about 30%) of the vote 
# share when there are no signs or adjacent. 

# Question 2.s - regression evaluate
# Observing the R square of 0.094,  show the amount of variation of the model.
# With this r square amount, we can determine that there is not a lot of variation between
# location of the signs an the voteshare. There are other factors that should come into 
# Consideration when building this model such as demographics or turnout in prior elections. 




