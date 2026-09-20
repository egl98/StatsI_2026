# we want to create "abnormal" dostribution to make out point
# generate an off transformation of the data
x <- runif(100000, min = -1, max = 1)
x <- sqrt (1+x) + 2*x^3 - 23*x + abs(log(abs(x))) + 2*(x > .5) + -2*(x < -.5)
# add a large mass of data in the middle at zero
# do so by assinging all values between -5 and 5
# to zero
x[x > -5 & x < 5]<- 0
# plot and save as pdf
pdf("ZeroCenteredDist.pdf")
plot(density(x, bw = .5), main="", xlab = "")
dev.off()

# STEP 1: CREATE A "RANDOM SAMPLE" PARAMETERS TO ESTIMATE

# don't forget to set you seed so that you can reproduce your
# sample example!
set.seed(123)
# we'll take 1000 random samples
n.sample <- 1000
# with a sample size = 20
sample.size <-20
# use a "for loop"
# (1) make an empty vector to store all our sample
# statistics
# fill the empty vector with NAs (missing data)
# remeber, make it lengeth 1000
x.bars <- rep(NA, n.sample)

# STEP 2: GENERSTE RANDOM SSMPLE AND ESTIMATE VALUE

# (2) "loop" over the vector 1, 2,....,1000
# in each iteration the variable "i" will increment up to
# the next value
for(i in 1:n.sample){
  # draw a random sample
  this.sample <- sample(x, size = sample.size, replace=F)
  # calculate mwan and add it to the vector of NAs
  x.bars[i] <- mean(this.sample)
}
# plot and save sampling distrubtion of mean Xs
pdf("ExmpleSamplingDist.pdf")
plot(density(x.bars), main="", xlab = "")
abline(v = mean(x.bars), col="red", lwd=3, lty=2)
dev.off()

# Now with a larger sample size
# Update the sample size to 2000 instead of 20
sample.size <- 2000
# create new vector of x bars (still length 100)
new.x.bars <- rep(NA, n.sample)
# "loop" again (create random sample of 2000) and
# get the mean, repeat 1000x
for(i in 1:sample.size){
  this.sample <- sample(x, size.saze, replace=F)
  new.x.bars[i] <- mean(this.sample)
}
# plot and save new distrubtion
pdf("NewDistOf2000v2.pdf")
plot(density(x.bars), main="", xlab = "")
abline(v = mean(x.bars), col="red", lwd=3, lty=2)
dev.off()

# CALULATE SOME CIs 
# Let's say our confidence coefficient is = .95
# calculate the appropriate confidence interval for the 
# mean level of support on how Preisdent Trump has 
## handle the war in Iran (bushIraq)
z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)
#(1- Confidence coefficient/2)
n <- length(na.omit(trump_iran$num))
sample_me <- sample_meam - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n))))
confint95 <- c(lower_95, upper_95)

