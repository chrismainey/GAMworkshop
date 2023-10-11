# Here is an example using Osteosarcoma, a type of bone cancer.
# This is based on incidence data, per 100,00 population 2016-2018,
# Published by Cancer Research UK:
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/bone-sarcoma/incidence
# The data have been slightly regrouped to aid the exercise.

# Load the data:

library(readr)
library(ggplot2)
sarcoma <- read_csv("./data/sarcoma.csv", name_repair = "universal")

# Columns in the data are:
# Age:            Upper number of 5-year age bands. e.g. 0 - 4, is coded as 4.  90+ is coded as 95
#                 for the sake of this exercise.
# Female.Cases:   Number of incident cases reported in females.
# Male.Cases:     Number of incident cases reported in males.
# Female.Rates:   Incidence Rate per 100,000 population, in this age group, for females
# Male.Rates:     Incidence Rate per 100,000 population, in this age group, for males




############################################################

# Now let's fit it as a GAM using mgcv package.
# Use the 's()' function to wrap a smoother around age.  We'll let it chose for use at first.

library(mgcv)
sarcoma_gam1 <- gam(Male.Rates ~ s(Age),data=sarcoma)

summary(sarcoma_gam1)


# Plot the results
plot(sarcoma_gam1, residuals = TRUE, pch = 1)


#### Can also use alternatives like `gratia` or mgcVis packages:

# https://gavinsimpson.github.io/gratia/index.html
gratia::draw(sarcoma_gam1, residuals = TRUE)

library(mgcViz)
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
b <- mgcViz::getViz(sarcoma_gam1)

o <- plot( sm(b, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()



# Extract the model coefficients with `coef` function.  How many coefficients are there for basis functions that make up this smooth?
coef(sarcoma_gam1)


# Can you interpret any of those coefficients?
# How do you know if they are 'significant'?

summary(sarcoma_gam1)
anova(sarcoma_gam1)

# Both show the pooled significance of the whole smoother.

# Predict and fit manually:
# We'll create a colmn called 'gam1_preds' for the predictions in our original data frame

sarcoma$gam1_preds <- predict(sarcoma_gam1, newdata = sarcoma)

# also compare to predctions fomr our original model

sarcoma$lm_preds <- predict(sarcoma_lm, newdata = sarcoma)
sarcoma$lm_poly_preds <- predict(sarcoma_lm_poly, newdata = sarcoma)

ggplot(sarcoma, aes(y=Male.Rates))+
  geom_point(aes(x=gam1_preds), col="mediumpurple")+
  geom_point(aes(x=lm_preds), col="mediumaquamarine")+
  geom_point(aes(x=lm_poly_preds), col="orange2")+
  geom_abline(intercept=0, slope=1, col="red")

# In general, purple points (gam) are closer to the line (perfect prediction) compared to the regular lm.
# Lets sum up the residual error, squaring it so +/- don't cancel.
sum(residuals(sarcoma_lm)^2)
sum(residuals(sarcoma_lm_poly)^2)
sum(residuals(sarcoma_gam1)^2)
