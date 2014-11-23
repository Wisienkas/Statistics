# Lab exercises Lab4C
# Exercises: {5.5, 5.12, 5.19}
#

################
# Exercise 5.5 #
################
# Gifted children.
# Research collected a simple random sample of 36 children who had been
# identified as gifted in a large city. The following histograms show the
# distribution of the IQ scores of mothers and fathers of these children

#         | Mother  | Father  | Diff.
# Mean    | 118.2   | 114.8   | 3.4
# SD      | 6.5     | 3.5     | 7.5
# n       | 36      | 36      | 36

# (A) Are the IQs of mothers and the IQs of fathers in this data set related? Explain.
#  
# To find out if they are related we'll make a confidence interval
 
M_F_rel <- 3.4

SE <- 7.5 / sqrt(36)

Z <- (M_F_rel - 0) / SE 

P <- 2 * (1 - pnorm(Z))

result <- 0.05 < P
# The null hypothesis can be rejected based on result == false

# (B) Conduct a hypothess test to evaluate if the scores are equal on average.
# Make sure to clearly state your hypothesis, check the relevant conditions,
# and state you conclusion in the context of the data


#################
# Exercise 5.12 #
#################

diff <- 30.56 - 28.79
ym_n <- 867
ym_sd <- 14.35

mm_n <- 133
mm_sd <- 13.48

SE <- sqrt((ym_sd * ym_sd)/ym_n + (mm_sd * mm_sd)/mm_n) 

Z <- (diff - 0) / SE

P <- 2 * ( 1 - pnorm(Z))

result <- 0.05 < P
