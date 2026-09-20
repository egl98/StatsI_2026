# CALULATE SOME CIs 
# Let's say our confidence coefficient is = .95
# calculate the appropriate confidence interval for the 
# mean level of support on how Preisdent Trump has 
## handle the war in Iran (bushIraq)

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)
#(1- Confidence coefficient/2)
n <- length(na.omit(trump_iran$num))
sample_me <- sample_meam - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)

#what does tis meam?
round(confint95, 3)