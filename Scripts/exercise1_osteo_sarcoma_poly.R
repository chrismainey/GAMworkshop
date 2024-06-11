# Here is an example using Osteosarcoma, a type of bone cancer.
# This is based on incidence data, per 100,00 population 2016-2018,
# Published by Cancer Research UK:
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/bone-sarcoma/incidence
# The data have been slightly regrouped to aid the exercise.

# Load the data:

library(readr)
sarcoma <- read_csv("./data/sarcoma.csv", name_repair = "universal")

# Columns in the data are:
# Age:            Upper number of 5-year age bands. e.g. 0 - 4, is coded as 4.  90+ is coded as 95
#                 for the sake of this exercise.
# Female.Cases:   Number of incident cases reported in females.
# Male.Cases:     Number of incident cases reported in males.
# Female.Rates:   Incidence Rate per 100,000 population, in this age group, for females
# Male.Rates:     Incidence Rate per 100,000 population, in this age group, for males




###################### Part 1: Linear regression with a single predictor  ######################################

# With the Sarcoma data, we will examine the relationship between age and incidence
# Pick either the Male or Female reporting category and lets consider the number of Cases
summary(sarcoma)

View(sarcoma)




# Let's visualise the shape of the distribution.
# Scatter plots, line or bar charts may be good starts.

library(ggplot2)
ggplot(sarcoma, aes(x=Age, y=Male.Cases))+
  geom_col(fill="firebrick", col=1, alpha=0.5)+
  labs(title="Incidence of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))

ggplot(sarcoma, aes(x=Age, y=Female.Cases))+
  geom_col(fill="orange2", col=1, alpha=0.5)+
  labs(title="Incidence of Osteosarcoma in UK Females, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))




# Does the number of cases appear related to age?  -  Some increasing relationship
# If so, is it linearly?



# Try the same with the rate to see if population size clouds it.

ggplot(sarcoma, aes(x=Age, y=Male.Rates))+
  geom_col(fill="mediumpurple4", col=1, alpha=0.5)+
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))

ggplot(sarcoma, aes(x=Age, y=Female.Rates))+
  geom_col(fill="mediumaquamarine", col=1, alpha=0.5)+
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))



# Does the number of cases appear related to age?  -  Looks to be stronger
# If so, is it linearly? No, appears to be a peak in teenage to early twenties





################################################################################################################
# Modelling the rate in Males.

# Build a linear regression model (lm) to examine it using the summary function

sarcoma_lm <- lm(Male.Rates ~ Age,data=sarcoma)

summary(sarcoma_lm)


# Let's visualise it using ggplots 'geom_smooth', giving it the method = lm

ggplot(sarcoma, aes(x=Age, y=Male.Rates))+
  geom_col(fill="mediumpurple4", col=1, alpha=0.5)+
  geom_smooth(col="red", method = "lm") +
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))


# Examine this plot.  Is the model a good fit?
# Is it good for any of the range?


#################################################################################################################

# Let's try and do this with a polynomial term or two: Age + Age^2 + Age^3
# You can manually do it, or use the poly() function, which helps reduce correlation
# The more higher order the terms are, the more 'wiggly' the fit.
sarcoma_lm_poly <- lm(Male.Rates ~ poly(Age,degree=4),data=sarcoma)

summary(sarcoma_lm_poly)

anova(sarcoma_lm_poly)

# Visualise this again with ggplot geom_smooth.  Add the argument: formula = "y ~ naturalSpline(x)", including your options

ggplot(sarcoma, aes(x=Age, y=Male.Rates))+
  geom_col(fill="mediumpurple4", col=1, alpha=0.5)+
  geom_smooth(col="red", method = "lm", formula = "y ~ poly(x,degree=4)") +
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))



### Discuss overfitting.


#################################################




# Fit the model with a spline instead
# Use the 'splines2' package, and the natural cubic spline function naturalSpline
# Try specifying a number of degrees of freedom  - explain
# Try specifying the knots at specific ages

library(splines2)

sarcoma_lm_spline1 <- lm(Male.Rates ~ naturalSpline(Age, df=4),data=sarcoma)

summary(sarcoma_lm_spline1)


sarcoma_lm_spline2 <- lm(Male.Rates ~ naturalSpline(Age, knots= c(24,49,79)),data=sarcoma)

summary(sarcoma_lm_spline2)

# Has the model improved? - Yes, drastically. R-squared practically doubled.


# Visualise this again with ggplot geom_smooth.  Add the argument: formula = "y ~ naturalSpline(x)", including your options

ggplot(sarcoma, aes(x=Age, y=Male.Rates))+
  geom_col(fill="mediumpurple4", col=1, alpha=0.5)+
  geom_smooth(col="red", method = "lm", formula = "y ~ naturalSpline(x, df=4)") +
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))


ggplot(sarcoma, aes(x=Age, y=Male.Rates))+
  geom_col(fill="mediumpurple4", col=1, alpha=0.5)+
  geom_smooth(col="red", method = "lm", formula = "y ~ naturalSpline(x,  knots= c(24,49,79))") +
  labs(title="Incidence Rate of Osteosarcoma in UK Males, 2016 - 2018", subtitle="Source: Cancer Research UK")+
  theme_minimal()+
  theme(plot.subtitle = element_text(face="italic"))

