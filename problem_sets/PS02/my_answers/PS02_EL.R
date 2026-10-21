# Question 1 - Political Science 

# Question 1.a
# Creating table for observation for the data provided

bribe_table <- as.table(rbind(c(14,6,7), c(7,7,1)))
dimnames(bribe_table) <- list(
  Class = c("Upper Class", "Lower Class"),
  Bribe = c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
)

bribe_table

# Manually calculating the X^2

# Row sum for Upper and Lower Class
margin.table(bribe_table, 1)
# Total Upper Class = 27
# Total Lower Class = 15

# Column sum for Upper and Lower Class
margin.table(bribe_table,2)
# Not Stopped = 21
# Bribe Requested = 13
# Stopped/Given Warning = 8

# Finding the F_Expected 
expected <- outer(rowSums(bribe_table), colSums(bribe_table))/sum(bribe_table)
expected

# Chi Square Test
chi_sq <- sum((bribe_table - expected)^2 / expected)
chi_sq  
# X Sqr = 3.791168

# Chi-Square Test in R
chisq.test((bribe_table))
# X Sqr is 3.7912

# Question 1.b 
# Calulating the P-Value

# 1) Degrees of Freedom Calculation 
df <- (nrow(bribe_table)-1) * (ncol(bribe_table)-1)
df
# df = 2

# 2) P-Value

p_value <- pchisq(chi_sq,df,lower.tail = FALSE)
p_value
# P_Value = 0.1502306
# We can conclude that with the of the p at 0.1502306,and alpha at 0.1, we can reject the null hypothesis.The value is right near .1, and we can also assume that the variables are dependent to each other.

# Question 1.c
# Calculating the standardize residuals for each cell

sum_bt <- sum(bribe_table)
row_p <- rowSums(bribe_table)/sum_bt
col_p <- colSums(bribe_table)/sum_bt
exp <- outer(rowSums(bribe_table), colSums(bribe_table))/sum_bt

res <- (bribe_table -exp)/ sqrt(exp * outer(1 - row_p, 1-col_p))
res

# Question 1.d 
# When observing the data from the residuals, you can notice that the lower class had more expediencies to have a bribe offer compare to those in the upper class. The use of this is to look for patterns. 

# Question 2 - Economics 
# Researchers found that more woman complain about the quality of the drinking water than men.
# GOAL: Estimate the effect of the reservation policy on the number of new policy on the number of new or repaired drinking facilities in the villages.

# Data 
women_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
women_data

# Question 2.a 
# Two tail hypothesis 
# H0 is that female politicians are not likely to support policies female voter want than more than men. (I.e, gender does not determine the out come for the vote issued by the female politician)
# HA is that females politicians are likely to support female voter wants. 

# Question 2.b
# Creating the bivariate regressions to the test the hypothesis

model <-lm(water~reserved, data = women_data)
summary(model)

# Plotting
plot(women_data$reserved,women_data$water)
abline(model) 

#Question 2.c

coef(model)
# The model that I created use the variables water an reserved. When a village had a reserved leadership for women, the increase of the water will increase by 9.252423. There for we can suggest that when there are women leaders in office the increase of women's demands are reflected. 
# We can also look at the p value of 0.0197, we can reject the null hypothesis that suggest that female leaders do not take gender into considerations. # 