# Here is an example using Osteosarcoma, a type of bone cancer.
# This is based on incidence data, per 100,00 population 2016-2018,
# Published by Cancer Research UK:
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/bone-sarcoma/incidence
# The data have been slightly regrouped to aid the exercise.

# Load the data:

library(readr)
library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyr)
sarcoma <- read_csv("./data/sarcoma.csv", name_repair = "universal")

# Columns in the data are:
# Age:            Upper number of 5-year age bands. e.g. 0 - 4, is coded as 4.  90+ is coded as 95
#                 for the sake of this exercise.
# Female.Cases:   Number of incident cases reported in females.
# Male.Cases:     Number of incident cases reported in males.
# Female.Rates:   Incidence Rate per 100,000 population, in this age group, for females
# Male.Rates:     Incidence Rate per 100,000 population, in this age group, for males



# Let's rearrange it so we have both male and female and a column to distinguish
sarcoma_pivot <-
  sarcoma %>%
  select(Age, Male.Rates, Female.Rates) %>%
  pivot_longer(cols = -Age, names_to = "Sex",  names_pattern = "(.)") %>%
  mutate(Sex = factor(Sex))


# Let's test for an interaction:  age might not independent of sex, due to life expectancy.
summary(lm(value ~ Age+Sex, data=sarcoma_pivot))
summary(lm(value ~ Age*Sex, data=sarcoma_pivot))
# The effect is not 'significant' at 95%, but it has improved the R^2.  The lack of significant is likely
# due to the small dataset used.



########################################################################
# Representing interactions in a gam: categorical

# No interaction
sarcoma_gam11 <- gam(value ~ s(Age) + Sex , data=sarcoma_pivot)
summary(sarcoma_gam11)

# A smooth for age, by each Sex
sarcoma_gam12 <- gam(value ~ Sex + s(Age, by = Sex) , data=sarcoma_pivot)
summary(sarcoma_gam12)

plot(sarcoma_gam12, residuals = TRUE,  pch = 1)

vis.gam(sarcoma_gam12 , theta = 130, plot.type = "persp")
vis.gam(sarcoma_gam12 , theta = 260, plot.type = "persp")



######################################################
# "Generalized"  applied to a binary outcome
#####################################################
# we will now predict the 'Death' variable from the LOS_model data set in the NHSRdatasets package
# We will use two other columns age and LOS.  Again we will look at interactions here, and another
# way to represent this in a GAM.

library(NHSRdatasets)

data("LOS_model")


# Visualise distributions
ggplot(LOS_model, aes(Age))+
  geom_histogram(bins = 15, fill = "dodgerblue2", alpha=0.6, col="black")

# Visualise distributions
ggplot(LOS_model, aes(y=Age, x=factor(Death)))+
  geom_boxplot(fill = "dodgerblue2", alpha=0.6, col="black")


ggplot(LOS_model, aes(LOS))+
  geom_histogram(bins = 15, fill = "yellow2", alpha=0.6, col="black")

# Visualise distributions
ggplot(LOS_model, aes(y=LOS, x=factor(Death)))+
  geom_boxplot(fill = "yellow2", alpha=0.6, col="black")





# Initial models:  logistic regression (GLM) and logistic GAM

glm_death <- glm(Death ~ Age + LOS, family = "binomial", data = LOS_model)

gam_death <- gam(Death ~ s(Age) + s(LOS), family = "binomial", data = LOS_model, method = "REML")

AIC(glm_death)
AIC(gam_death)


library(ModelMetrics)
auc(glm_death)
auc(gam_death)

# Models appear equivalent

# Visualise the model
plot(gam_death, pages = 1)

# problem is that it is on the 'logit' (log-odds of outcome) scale.
# More interpretable if we transform back to probability scale
plot(gam_death, pages = 1, trans = plogis)


# Add the intercept in
plot(gam_death, pages = 1, trans = plogis, shift = coef(gam_death)[1])
#Each partial effect plot can be interpreted as showing the probability of the outcome
#if all other variables were at their average value.
#At their own average value, you get only the effect of the intercept.



# Visualise the interaction

ggplot(LOS_model, aes(y=Age, x=LOS, col=factor(Death)))+
  geom_point() + theme_minimal()


# examine the linear effect
summary(lm(Age ~ LOS, data=LOS_model))



# Now model let's model
# first as a logistic model with the interaction term
glm_death2 <- glm(Death ~ Age * LOS, family = "binomial", data = LOS_model)

summary(glm_death2)

# Now as a GAM with a tensorproduct
?te

gam_death2 <- gam(Death ~ te(Age, LOS), family = "binomial", data = LOS_model)

gam_death3 <- gam(Death ~ s(Age) + s(LOS) + ti(Age, LOS), family = "binomial", data = LOS_model)

summary(glm_death2)
summary(gam_death2)
summary(gam_death3)

AIC(glm_death2)
AIC(gam_death2)
AIC(gam_death3)


gam.check(gam_death2)

plot(gam_death, residuals = TRUE,  pch = 1, shade = TRUE, shade.col = "lightpink")
plot(gam_death2, residuals = TRUE, pch = 1, shade = TRUE, shade.col = "lightblue")

plot(gam_death2, scheme = 1)
plot(gam_death2, scheme = 2)

vis.gam(x = gam_death2, plot.type = "persp")

vis.gam(x = gam_death2, plot.type = "persp", view = c("Age","LOS"))



