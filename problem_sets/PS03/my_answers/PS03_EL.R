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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
# Reviewing Data
inc.sub

# Question 1
# Question 1.a - regression 
reg1 <- lm(voteshare ~ difflog, data = inc.sub)
reg1
summary(reg1)
# Explanation: For every unit change in spend difference between incumbent an challenger vote share there is a 4.2 increase in the incumbent's vote share. 

# Question 1.b -  scatter plot 
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Spending Difference",
     ylab = "Inumbent voter share",
     main = "Incumbent vote share vs. Spending differene",
     pch = 16, col = "gray")
abline(reg1, col = "red",lwd = 2)

