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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

#####################
# QUESTION 1
#####################
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# The first determining the length which is the number of values that
# we have in this data set. 

length(y)

# ANSWER FOR THE LENGTH OF Y IS: 25

# The second is to determine the mean of the sample of the scores.

mean_score <- mean(y)

mean_score

# ANSWER OF MEAN SCORE FOR THE SCORES:98.44

# The third is to determine the standard deviation of the of scores.The standard
# deviation is showing how far out the data is spread out.

sd_score <- sd(y)

sd_score

# ANSWER OF SD SCORE FOR THE SCORES: 13.09287

# I have calculated the length, mean and SD. In order to determine the 
# confidence interval, the standard error will also have to be calculated.

se_score <- sd_score / sqrt(length(y))

se_score

# ANSWER FOR THE STANDARD ERROR SCORES: 2.618575 

# Now having together the length, mean, standard deviation, and the standard error
# we can now calculate the confidence interval at 90%

# CI = mean +/- critical value * SE

# The confidence interval is set at 90%

# qnorm gives the z-value given proportion of the standard normal distribution

qnorm(0.95)
qnorm(0.05)

# ANSWER TO THE UPPER QNORM IS:  1.644854
# ANSWER TO THE LOWER QNORM IS: -1.644854

# Calculating the CI

lower_90 <- qnorm(0.05,
                    mean = mean_score,
                    sd = se_score)

upper_90 <- qnorm(0.95,
                  mean = mean_score,
                  sd = se_score)
lower_90
# ANSWER FOR THE LOWER TAIL: 94.13283
upper_90
# ANSWER FOR THE UPPER TAIL: 102.7472

# FULL ANSWER FOR QUESTION 1: We are 90% confident that the upper score average is 
# approximately 103. While the lower average score is 94.

####################
# QUESTION 2
####################

# Hypothesis testing will begin with two statements:

# Null hypothesis is: the average student's IQ is higher than 100 among all
# schools in the country. HO > 100

# Alternative hypothesis is: the average student's IQ is not higher than 100 
# among all schools in the country. HA < 100

# This will be at one-sided test because we are looking at if something is 
# or is not the value of the null. 

# The test statistic is calculated to determine the how far the estimate is from the
# null hypothesis. 


# Test = estimate - null value / SE

# T-statistic will be used due to the small population size (i,e 25 data points) and
# because this is a one-sided test. 

# The test will look at y (data sampling) and the degrees of freedom. With this
# we can observe if the we can reject the null hypothesis of HO > 100  through the 
# p-value outcome.

t.test(y, mu = 100,alternative = "greater") #H0 ~ p-value of 0.7215

t.test(y, mu = 100,alternative = "less") #HA ~ p-value of 0.2785

# FULL ANSWER FOR QUESTION 2:ANSWER FOR QUESTION 2.1: We failed to reject the null at the standard significance rate of 0.05 because the p-value is greater than the standard used. Thus we cannot conclude the average IQ of the counselor's school is greater than the country's IQ scores.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
 
# Checking the data of the table

head(expenditure)

####################
# QUESTION ONE 
####################

pairs(expenditure[, c("Y", "X1", "X2", "X3")], col = expenditure$Region)

#Taking all of these into account of all three plots, there is slight correlation that increases in capita expenditure can increase well being in Urban areas.

##################
# QUESTION TWO
##################

boxplot(Y ~ Region, data = expenditure,
        main = "Per capita expenditure on shelters/housing assistance by Region",
        xlab = "Region",
        ylab = "Per capita expenditure on shelters/housing assistance",
        col = "lightblue")

# On average the region that has the most shelter/housing assistance is region 4, the West.

#################
# QUESTION THREE
################


library(ggplot2)

ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm")

#Plot 2 with regions 

ggplot(expenditure, aes(x = X1, y = Y,
                        color = factor(Region),
                        shape = factor(Region))) +
  geom_point(size = 3) +
  labs(color = "Region", shape = "Region",
       title = "Relationship between Y and X1 by Region",
       x = "X1", y = "Y") +
  theme_minimal()

# The relationship between Y and X1 shows that the more capital put into shelter and housing assistance, the income stays around $2000. There is steady growth. When adding  an additional variable of regions you see that region 3 (the south), puts in low capita for housing and shelter and have lower income.

