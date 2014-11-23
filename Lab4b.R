# - Central limit theorem:
# 
# - CLT Condition
# 
# Independence. Random sampling and by sampling without replacement and about n < 10% of all
# 
# instead 1 observation is 1 person, 1 observation is the mean of n observations. 
# this causes the histogram to be more symmetric the higher n is.

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData");

#Simple random sample size of 60 from population

population <- ames$Gr.Liv.Area

samp <- sample(population, 60)

# Exercise 1 describe dist of sample, Would you call it "typical" size
# State precisely what you interpreted "typical" to mean

hist(samp)
mean(samp)
# Gives 1500.233
# It look likes the histogram is right skewed, might be good for skiing

# Exercise 2 Would you expect another student's dist to be identical
# to yours? 
# Would you expect to be similar? why? why not?

# I wouldn't expect another students distribution to be identical
# I would however think it would be somewhat similar, since we use
# the same data and that would end up with somewhat the same tendencies

hist(ames$Gr.Liv.Area)
# This show that it is somewhat as mine, and i would expect others to
# look about the same, in smaller scale, with some chance to be different
# mainly due to the small number of random samples taken.

# CONFIDENCE INTERVALS

sample_mean <- mean(samp)

# How to calculate the 95% confidence interval

se <- sd(samp)/sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se

c(lower, upper)
# [1] 1350  1650

# Exercise 3: For the confidence interval to be valid, the sample mean
# must be normally distributed and have standard error s/sqrt(n). 
# What coniditons must be met for this to be true?

# To be normally distributed it must be symmetric

# CONFIDENCE LEVELS

# Exercise 4: what does 95% confidence" mean? if you're not sure
# see Section 4.2.2

# Means the chance that the entire mean will be within the 95% part
# of the random sampling 

mean(population)
# was 1499

# Exercise 5: Does your confidence interval capture the true average
# size of houses in Ames?If you are working on this lab in a lab,
# does your neighbor's interval capture this value?

# Yes the true average is captured wihing the 
# My neighbor also captured the average, but was way close to the
# lower bound 

# Exercise 6: Each student in your class should have gotten a slightly
# different confidence interval. 
# what proportion of those intervals would you expect to capture 
# the true population mean?

# I would expect 95% of the students to capture it.

# Using R we're creating many samples to learn how samples means 
# and confidence intervals vary from one another. Loops can be handy

# Outline is
# (1) Obtain a random sample
# (2) Calculate the samples mean and sd
# (3) Use these statistics to calculate a confidence interval.
# repeat steps 50 times

samp_mean <- rep(NA, 50) # Initializes mean vector

samp_sd <- rep(NA, 50) # Initializes sd vector

n <- 60 # Number of samples from population

for(i in 1:50){
  samp <- sample(population, n)
  samp_mean[i] <- mean(samp)
  samp_sd[i] <- sd(samp)
}

lower_vector <- samp_mean - 1.96 * samp_sd/sqrt(n)
upper_vector <- samp_mean + 1.96 * samp_sd/sqrt(n)

# ON YOUR OWN #
# 1. Use function plot_ci to plot all intervals and see if its about 95% confidence

plot_ci(lower_vector, upper_vector, mean(population))

# Had about 3 not capturing true average, which is 6% so that fits quite close

