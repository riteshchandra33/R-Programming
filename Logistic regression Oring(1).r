# File     Logistic regression
# 
#          Adapted from Julian Faraway, 
#          Extending the Linear Model with R, 2006
#          Chapman and Hall/CRC 
# 
# Due      Plot from 6 
# 
# What is Logistic Regression?
# Linear regression is appropriate for continuous outcomes 
# In many studies including biomedical research, our outcomes are more commonly of different forms
# Binary is probably the most prevalent
# example) disease versus not disease
# example) success versus failure
# example) progressed versus not progressed
# example) dead versus alive (or survival)
# 
# 
# 1. Set up
# 
# Install the package faraway

## Run
library(faraway)
data(orings)
## 

# 2. Space shuttle O-ring seals data and the
#    Challenger 1986 explosion 
# 
# Study of possible causes of 
# The Challenger explosion shortly after launch
# in January 1986 drew attention to the O-ring
# seals in the rocket boosters. The temperature
# at launch was much lower than at previous
# shuttle launches. At lower temperatures rubber
# becomes brittle and less effective as a
# sealant. Might this have been the cause of the
# explosion? Should the danger have been 
# understood by the engineers?  
# 
# Edward Tufte say that the failure to
# appreciate the danger was strongly associated
# with the poor graphics used to look at the data.  
# See his discussion and the graphics in
# Edward Tufte. 1997. Visual Explanations
# pp. 39-53
#    
# The goal here to model the data as presented
# by Julian Faraway.  Faraway refers to Dala, Fowlkes
# and Hoadly (1989)for a more detailed description.
# 
# This dataset combines observations from 23
# previous shuttle missions. Each shuttle has
# two boosters, each with three O-rings. The
# dependent variable is the number of the 6
# O-rings that show evidence of damage due to
# blow by and erosion.  
# 
# A simple model is that the data for each
# shuttle is binomial data with trials of size
# six and the observation being the number
# of damaged O-rings.  We have 23
# experiments and are interested in estimating
# probability of damage as a function of the
# independent variable, temperature.   


## Run

orings

## End

# 3. Regression Models for Binomial Data
# 
# With a simple regression model in mind
# a first thought might be to consider a model
# of the form

# p[i] = a + b * x[i] + e [i]
# 
# for the ith mission where 
# p[i] is the probability of damage,
# x[i] is the temperature at launch, and
# e[i] is the error. 
# 
# The model could lead to a 
# nonsensical result for some temperatures
# because we know that probabilities
# are limited to values in the closed
# interval [0  1], but the model does
# not enforce this constraint.   
# 
# Instead if we use the model
# p[i] = exp(a + b * x[i])/ ( 1 + exp(a + b * x[i] ), 
# the probabilities are forced to be between 0 and 1.  
# 
# Let z[i]  = a + b*x[i]
# The right hand side is of form
# exp( z[i] ) / (1+exp(z[i]))
# 
# We omit the i subscript and consider the
# the function
# p(z) = exp(z)/(1+exp(z)).
# 
# Since exp(z) >= 0 we can see
# p(z) is non-negative and less than
# 1 for finite values of z.  
# 
# What happens when z approaches -infinity? 
#   For negative z  
#   exp(z)is the same as 1/exp(|z|)
#   As |z| get large this approaches 0.  
# 
#   The denominator, 1+1/exp(|z|) approaches 1
# The ratio approaches 0/1 = 0. 
# 
# What happens when z approaches infinity?
#   Consider adding and subtracting 1
#   from the numerator.   
#   (1+exp(z)  -1)/(1+exp(z))= 
#     1        -1/(1+exp(z))
#   The second term goes to 0.
# The ratio approaches 1.  
# 
# Hence 0 <= p(z) <= 1      
# 
# Solving 
#   p = exp(z)/(1+exp(z)) for z yields
#   z = log( p/(1-p) )
# 
# The right hand side is the log of the odds
# ratio p/(1-p).  
#   
# Returning to the data at hand we have
#       a + b * x[i]  = log(p[i]/(1-p[i]) 
# so the simple linear combination is effectively
# modeling the log odds ratio which is free to
# range from -infinity to infinity.
# 
# A host of other models for p[i] could be used.
# Any continuous monotone increasing cumulative
# distribution function with support from -infinity
# to infinity will work. 
# 
# You might ask what about using the cumulative normal
# distribution? Yes, this works and the model
# has a name, the probit model. 
# 
# The logit and probit models are often used.
# 
# A third sometimes-used model is the complementary
# log-log function.  This has form z = log(-log(1-p))
# 
# 4. Estimating model parameters
# 
# We want to model the probability p, (a parameter
# in the binomial family of probability distributions),
# as a function of a linear combination of predictor variables.
# For several families of probability distributions we can use
# the generalized linear model algorithm implemented in the
# glm() function.  
# 
# Briefly, since this is binomial data and we will be assuming
# independent observations given the data, it is straight forward
# to write the log likelihood. The standard approach of obtaining
# parameter estimates by maximizing the log likelihood works. 
# We can obtain estimates for a, b, and p.  The theory also
# supports estimating standard errors for the maximum
# likelihood estimates.  The p-values are derived from
# z-scores = estimate/standardError  
#  
# 5. Fitting three models

## Run

# Naive linear model

linearModel = lm(damage/6 ~ temp,data=orings)
summary(linearModel)


# Logit model

logitModel = 
  glm(cbind(damage,6-damage) ~ temp,
     family=binomial, data = orings) 
summary(logitModel)

# Probit 

probitModel = 
  glm(cbind(damage,6-damage) ~ temp,
     family=binomial(probit),data=orings)
summary(probitModel)
## End


# 6. Plots of fitted values

##Run

windows()
plot(damage/6~temp,orings,xlim=c(25,85),ylim=c(0,1),las=1,
xlab="Temperature (Degrees Fahrenheit)", ylab="Damage Probability",
pch=21,bg="red",col="red",
main="O-ring Models\nLinear:Black, Probit:Blue, Logit:Red")

tempGrid = 25:85
a=coef(linearModel)
lines(tempGrid,a[1]+a[2]*tempGrid,col="black",lwd=2)

a=coef(probitModel)
lines(tempGrid,pnorm(a[1]+a[2]*tempGrid),col="blue",lwd=2)

a = coef(logitModel)
lines(tempGrid,ilogit(a[1]+a[2]*tempGrid),col="red",lwd=4)

##End


# 7.  Comment
# 
# The predicted temperature for the launch time was
# 26 to 29 degrees Fahrenheit.  The plot from Section
# 6 shows extrapolated probabilities of damage for this
# interval. The probabilities for the two  
# generalized linear models are about 1 for this
# interval. Even the high probabilities from the simple
# linear model would be cause for concern by the
# engineers, administrators and crew.    
# 
# (Yes, the plot in Section 6 could be redesigned to
# draw more visual attention to this low temperature
# interval.) 
# 
# Without the modeling, Tufte's position-along-a-scale
# plot of damage versus temperature would have 
# caused concern. Yes, entertainment graphics gathered
# by Tufte (and likely shown in class) can fail to
# bring out the patterns in the data. In some cases
# this can lead to
# disastrous decisions. 
# 
# Of course it is easy to be critical in hindsight. 
# Nonetheless there is reason to push for quality graphics
# and for more education so a simple scatterplot is not
# considered too advanced for the general public.            
#       