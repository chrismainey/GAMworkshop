### Examining Framingham data

#The Framingham Heart disease study is a long-standing cohort study that is responsible for
#much of what we know about heart diseases and associated conditions.  For more info, see:
#https://en.wikipedia.org/wiki/Framingham_Heart_Study

# We are going to use GAMs to investigate some of it:

framingham <- read.csv("./data/framingham.csv")

# The columns we have are:
# male            0 = Female; 1 = Male
# age             Age at examination time.
# education       1 = Some High School; 2 = High School or GED;
#                 3 = Some College or Vocational School; 4 = college
# currentSmoker   0 = nonsmoker; 1 = smoker
# cigsPerDay      number of cigarettes smoked per day (estimated average)
# BPMeds          0 = Not on Blood Pressure medications; 1 = Is on Blood Pressure medications
# prevalentStroke
# prevalentHyp
# diabetes        0 = No; 1 = Yes
# totChol         mg/dL
# sysBP           mmHg
# diaBP           mmHg
# BMI             Body Mass Index calculated as: Weight (kg) / Height(meter-squared)
# heartRate       Beats/Min (Ventricular)
# glucose         mg/dL
# TenYearCHD      0 = No CHD at 10-year follow-up; 1 = CHD at 10-year follow-up


# Try and build the best regression model you can:

library(mgcv)

CHDrisk <- gam(TenYearCHD ~  factor(male)
               + factor(currentSmoker)
               + s(age)
               + s(glucose)
               + s(sysBP, diaBP)
               + factor(diabetes)
               + factor(prevalentStroke)
               + factor(prevalentHyp)
               + s(totChol)
               , data = framingham
               , family = "binomial"
               , metho = "REML")

CHDrisk2 <- gam(TenYearCHD ~  factor(male)
               + factor(currentSmoker)
               + s(age)
               + s(glucose)
               + s(sysBP)
               + s(diaBP)
               + factor(diabetes)
               + factor(prevalentStroke)
               + factor(prevalentHyp)
               + s(totChol)
               , data = framingham
               , family = "binomial"
               , metho = "REML")



CHDrisk3 <- gam(TenYearCHD ~  factor(male)
                + factor(currentSmoker)
                + s(age)
                + te(sysBP, diaBP)
                + s(glucose, by = factor(diabetes))
                + factor(prevalentStroke)
                + factor(prevalentHyp)
                + s(totChol)
                , data = framingham
                , family = "binomial"
                , metho = "REML")


CHDrisk4 <- gam(TenYearCHD ~  factor(male)
                + factor(currentSmoker)
                + s(age, sysBP)
               # + te(age, sysBP)
                + s(sysBP)
                + s(diaBP)
                + s(glucose, by = factor(diabetes))
                + factor(prevalentStroke)
                + factor(prevalentHyp)
                + s(totChol)
                , data = framingham
                , family = "binomial"
                , metho = "REML")





gam.check(CHDrisk)
gam.check(CHDrisk2)
gam.check(CHDrisk3)
gam.check(CHDrisk4)

library(gratia)
appraise(CHDrisk)

gratia::draw(CHDrisk)

ModelMetrics::auc(CHDrisk)
summary(CHDrisk)
summary(CHDrisk2)
summary(CHDrisk3)
summary(CHDrisk4)

# Change of 4 ~ asymptotic to 95% significant
AIC(CHDrisk)
AIC(CHDrisk2)
AIC(CHDrisk3)
AIC(CHDrisk4)
