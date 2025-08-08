# Here is an example using Osteosarcoma, a type of bone cancer.
# This is based on incidence data, per 100,00 population 2016-2018,
# Published by Cancer Research UK:
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/bone-sarcoma/incidence
# The data have been slightly regrouped to aid the exercise.

# Load the data:

library(readr)
library(ggplot2)
library(mgcv)
sarcoma <- read_csv("./data/sarcoma.csv", name_repair = "universal")

# Columns in the data are:
# Age:            Upper number of 5-year age bands. e.g. 0 - 4, is coded as 4.  90+ is coded as 95
#                 for the sake of this exercise.
# Female.Cases:   Number of incident cases reported in females.
# Male.Cases:     Number of incident cases reported in males.
# Female.Rates:   Incidence Rate per 100,000 population, in this age group, for females
# Male.Rates:     Incidence Rate per 100,000 population, in this age group, for males



#  The gam we finished the last exercise with:


sarcoma_gam1 <- gam(Male.Rates ~ s(Age),data=sarcoma)

summary(sarcoma_gam1)

plot(sarcoma_gam1)


############################################################

# Changing the smoother and knots

# There are various smoother types and mgcv defaults to the 'thinplate spline'.
# Use the help for the 's' function to see if you can work out how to cahgne this to a cubic spline.

?s

sarcoma_gam2 <- gam(Male.Rates ~ s(Age, bs="cr"),data=sarcoma)

# Remember to plot it.  Is it much different to the default?
plot(sarcoma_gam2, residuals = TRUE, pch = 1)



# Now, using the help, try and specify 'k' the dimension of the bases.
# This is slight different for each smoother type, and it is the same as 'df' in the earlier
# example using naturalSpline()'.  Try 3,6,9 and 12, and visualise each.

sarcoma_gam3 <- gam(Male.Rates ~ s(Age, bs="cr", k=3),data=sarcoma)
sarcoma_gam4 <- gam(Male.Rates ~ s(Age, bs="cr", k=6),data=sarcoma)
sarcoma_gam5 <- gam(Male.Rates ~ s(Age, bs="cr", k=9),data=sarcoma)
sarcoma_gam6 <- gam(Male.Rates ~ s(Age, bs="cr", k=12),data=sarcoma)


plot(sarcoma_gam3, residuals = TRUE, pch = 1)
plot(sarcoma_gam4, residuals = TRUE, pch = 1)
plot(sarcoma_gam5, residuals = TRUE, pch = 1)
plot(sarcoma_gam6, residuals = TRUE, pch = 1)


# Can you explain the shapes of the visualisations?
# - At lower k, there are not many basis functions to flex.
# 3 is a straight line, 4 is 2 'S'-shaped functions
# 9 and 12 are higher, but minimally different.

#############################################################

# Smoothing parameters / penalty
# The smoothing parameter (penalty) works with the likelihood to dampen 'wiggliness'
# This affected the two models with k=6 and k=12, penalising the 12 more to avoid too much 'wiggliness'.
# Let's alter the smoothing parameter.  This is essential to avoid over fitting.
# Try it with 10, 1 and 0.1 0.1 and visualise.

sarcoma_gam7 <- gam(Male.Rates ~ s(Age, bs="cr", k=12),data=sarcoma, sp=10)
sarcoma_gam8 <- gam(Male.Rates ~ s(Age, bs="cr", k=12),data=sarcoma, sp=1)
sarcoma_gam9 <- gam(Male.Rates ~ s(Age, bs="cr", k=12),data=sarcoma, sp=0.1)

plot(sarcoma_gam7, residuals = TRUE, pch = 1)
plot(sarcoma_gam8, residuals = TRUE, pch = 1)
plot(sarcoma_gam9, residuals = TRUE, pch = 1)

# The higher penalties reduce the 'wiggliness'
# Extreme:
sarcoma_gam7a <- gam(Male.Rates ~ s(Age, bs="cr", k=12),data=sarcoma, sp=1e6)
plot(sarcoma_gam7a, residuals = TRUE, pch = 1)


############################################################
# Model fitting and diagnostics
###########################################################

# Moving back to our first model, let's use mgcv's automatic fitting tools.
# It sets a default dimension to k=10, and estimated the required penalty.
# We'll check that the dimension, k is large enough, using the
# use the method option 'REML'


?choose.k


sarcoma_gam10 <-  gam(Male.Rates ~ s(Age, bs="cr"), method = "REML", data=sarcoma)

plot(sarcoma_gam10, residuals = TRUE, pch = 1)

library('gratia')
draw(sarcoma_gam10, residuals = TRUE)


### Model checking
# let's check the dimensions of smooths

gam.check(sarcoma_gam10)

# Model checks
# using gratia
appraise(sarcoma_gam10)

# using mgcViz
check.gamViz(getViz(sarcoma_gam10))

# mgcv has functions already for some of diagnostics
qq.gam(sarcoma_gam10, pch=1)


