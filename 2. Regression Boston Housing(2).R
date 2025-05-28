                  # Regression: Boston Housing Data

# Includes sections from the
# ISLR Chapter 3 Lab: Linear Regression
# and a lot more.

#Due: 5 points

# 1.2  The last scatterplot matrix
# 3.3  The better plot
# 3.4  The regression diagnostic plot
# 4.2  The natural spline ggplot
# 6.1  The second corrplo that has numbers.
#========================================================

# Sections:

# 1.  Learning about the data
# 1.1 Viewing data with a data editor
# 1.2 Viewing the data in scatterplot matrix
#     using univariate and bivariate density
#     plots and smooths

# 2.  Single predictor variable models
# 2.1 Fit a single predictor model
# 2.2 Look at the model summary
# 2 2.3 Extracting estimates
#       coefficients, predicted values, ...
# 2.4 Computing confidence interval
#     based on linear regression models

# 3.  Looking graphics
# 3.1 Note on the attach() function
# 3.2 Quick R base-level graphics
# 3.3 A better plot
# 3.4 Regression diagnostics
# 3.4.1 Residuals versus fitted value
# 3.4.2 A Quantile - Quantile plot
# 3.4.3 A spread - location plot
# 3.4.4 Standarized residuals versus leverage
#3.4.5 Normal distribution hypothesis test


# 4.  Multivariate Regression
# 4.1 R model specification syntax
# 4.2 Syntax for variable transformation
#     such logs, product, polynomials and
#     splines.
#4.3 Model comparison with Anova

#5.  The model matrix
#5.1 Looking at the model.matrix produced
#    by factors
#5.2 Looking at the model.matrix produce
 #   transformations

#6.  Correlation
#6.1 Original predictor correlation plots
#6.2 The variable inflation factor

#======================================

#0. Setup

## Run

library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(ggplot2)
source("hw.r")
library(car)   # vif() and qqPlot functions
library(splines)
library(corrplot)
library(graphics)
library(grDevices)                  

##========================================

# 1. Learning about the data
#
# Read the class pdf file:
# Learning about data and model building.
#
# 1.1 View the data with a data editor
#
# The View() function provide
# a scrollable table.

View(Boston)

# What are the variable names?
# We can read the names from the table
# but they leave a lot of questions.
#
# What do the variable names mean?
# What are the units of measure?
# What is the dependent variable?
#
# What is known the variable relationships?
# Are there previous models of the data?
# Are there models of similar data?

?Boston
#
# The documentation begins to answer questions.
# More can be learned from the cited papers.
#
# A paper title suggests there are collinearity.
# issues to address with this data.
#
# 1.2 Viewing the data in scatterplot matrix
#     using univariate and bivariate density
#     plots and smooths
#
# The dependent variable is the median value
# of owner-occupied homes. Below we put this in
# the top left corner the scatterplot matrix
# where many people will look first.
#
# Below we show the univariate densities
# down the diagonal from the top left to
# the bottom right. Off the diagonal we
# show bivariate density using hexagon bin
# plots. The darker hexagon cells have
# more counts.

varNum <- function(x){
   val <- 1:ncol(x)
  names(val)<- colnames(x)
   return(val)
}
varNum(Boston)

Boston1 <- Boston[,c(14,1:13)]

# hexbin is to modifed have to 15 xbins and
# the  trans() function power is set to 0.5.

offDiag <- function(x,y,...){
   panel.grid(h=-1,v=-1,...)
   panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
     trans=function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}

offDiag

 onDiag <- function(x, ...){
     yrng <- current.panel.limits()$ylim
     d <- density(x, na.rm=TRUE)
     d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
     panel.lines(d,col=rgb(.83,.66,1),lwd=2)
     diag.panel.splom(x, ...)
 }

splom(Boston1,as.matrix=TRUE,
  xlab='',main="Boston Housing: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

# In the top row the y-axis represents the
# dependent variable, the median
# value ($1000) of owner-occupied homes
# in different Boston Suburbs.
#
# in the top row fro  column 2 to the right,
# the x-asis shows the input (predictor
# or explanatory) variables on the
# x-axis.
#
# The two predictor variables rm ans lstat
# are more directly suggestive
# of functional relationships than the
# others inputs.  We can envision a smooth curve
# going though the body of data with
# vertical variability about the smooth
# not being very irregular across
# the predictor variable domain.
#
# However are not equally adept at envisioning
# smooth fits to the data. Script based
# visualations of smoothes provide
# a replicable foundation for thinking
# about smooth fits to the data. Below
# we augment panels with smoothes.
#
# Sometimes smoothing functios seem
# inappropriate due sturcture of the
# data.here are big gaps in some of the
# input variable domains that are
# not ideal for fitting smooths.  Below
# To avoid the failures we omit variables
# with one or two high univariate density
# spikes.  Looking down the diagonal we see that
# variable crim, zn, chas and black
# have such spikes. We do keep some of the
# variables that have gaps on the x-axis.
#
# (An automated procedure could run smoothing
# functions and omit the variables for
# which it doesn't work.)

varNum(Boston1)

Boston2 <- Boston1[,-c(2,3,5,13)]

# uncomment the panel.loess line
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
    trans=function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}

splom(Boston2, as.matrix=TRUE,
  xlab='',main="Boston Housing: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

# As envisioned from the scatterplot matrix,
# The top row variables rm and lstat look promising
# because they track more of y-axis (housing price)
# variability.
#
# Historical to explore the data
# we would fit the dominate strucuure,
# obtain the residuals, replace y
# in the scatterplot matrix with the residuals
#  make another scatterplot matrix.
#
# The idea was to remove the visuully dominant
# strucure to see if there was additional
# structure beneath the surface.
#
# The generalized additive models of today(gam)
# have put this idea to work.
# This beyond the scope of this class.
# Those interested can read class texts:
# R For Everyone Section 20.3 and
# ISLR Chapter 7 and Section 7.1
#=============================================

# 2.  Single predictor models

# 2.1 A first model

#model name<- lm(dependant variable ~ explaatory variable, dataset)
lm.fit <- lm(medv~lstat,data=Boston)
lm.fit


# 2.2  Look at the model summary

summary(lm.fit)

# For the moment assume the model
# distribution assumptions are satisfied.
#
# Is the model significantly better
# than then fitting just the mean?
# What does the F-statistic indicate?
#
# Which regression coefficients,
# if any, are significantly
# different than 0?
#
# What fraction of the variability is
# explained by the model?
#
# 2.3 Extracting estimates
#     coefficients, predicted values, ...
#
# The R script above saves the components of
# linear regression in the object lm.fit
# This is a list object.  We could extract
# the results using the list component
# names.  However there are extractor
# functions written for model list objects
# that are very handy.  Some
# extractor functions include
# useful computations. Examples appear
# below and in section 4.4.

# See component names
names(lm.fit)

# Access by name
lm.fit$coefficients

# Some extractor functions

coef(lm.fit)
predict(lm.fit)
residuals(lm.fit)
rstandard(lm.fit)

x11() #opens a new plot window
plot(lm.fit)

inf <- influence(lm.fit)
range(inf$hat) # hat values indicate point leverage

# 2.4 Computing confidence intervals
#     based on linear regression models
#
# Compute confidence intervals for
# regression coefficients

confint(lm.fit) # 95% is the default
confint(lm.fit,level=.99)

# R will compute confidence intervals
# for the mean response at given predictor
# values.
#
# R will compute confidence intervals
# for future observations at given
# predictor values. Each new value
# also includes is own random error.
# This increases the size of the
# confidence interval.

## Run prediction at 3 values

vals = c(5,10,15)

predict(lm.fit,data.frame(lstat=vals),
  interval="confidence")

predict(lm.fit,data.frame(lstat=vals),
  interval="prediction")

#=====================================
#
# 3. Graphics
#
# 3.1 Note on the attach() function
#
# Attaching a data.frame makes its variables
# accessible without reference to the data.frame.
# We can detach() the data.frame when we no
# longer want direct access to the variables.

# 3.2 Quick R base-level graphic used in ISLR

attach(Boston)

windows()
plot(lstat,medv)

windows()
plot(lstat,medv,pch=20, col="red")

windows()
plot(lstat,medv,pch="+",col="blue")

# Plot redesign opportunities.
#
# There is no title that provides context.
#
# The description (meaning) of the x and y
# axis variables is vague and the units
# of measure are absent.
#
# Horizontal text for y axis tick labels
# (or grid line labels) would be easier
# to read.
#
# There are no grid lines.
#
# There is no pointwise confidence interval
# for the fitted regression line.
#
# The data points could be more prominent.
#
# The plot can be better separated from
# white page by using (gray level) contrast.
#
# 3.3 A better plot
#
# We can add grid lines, predicted confidence
# intervals for the fitted line and
# better labels. Adding color provides
# some visual appeal.  We can do this
# with based level graphics but here we
# use ggplot().
#
# R syntax comment:
# Surrounding the sequence of script lines
# with () causes R to continue reading
# lines until if finds the matching parenthesis.
# My slight appearance preference is to see
# the ggplot continuation symbol "+" at the
# left of a new line.



(ggplot(Boston,aes(x=lstat,y=medv))
 + geom_point(shape=21,fill="red",
     color="black",size=2)
 + stat_smooth(method=lm,
     color="blue",fill="cyan")
 + labs(
     x="Lower Status Percent of Population",
     y="Median House Value ($1000)",
     title="Boston Housing Data")
 + hw
)

# The label positioning might be refined.

# 3.4  Regression diagnostic plots
#
# The following puts R's four standard
# regression diagnostic plots in four
# panels on one page.

#The plot may be hidden behind he RStudio screen

windows()
par(mfrow=c(2,2))
plot(lm.fit)


# 3.4.1 Residuals versus fitted values
#
# In the top left plot, distribution of the
# residuals appears to be curved function of
# the fitted values and not a horizontal line
# centered at 0.  This violates the assumption
# that model error distribution consists
# independent identically distributed
# samples from a normal distribution
# with mean 0.
#
# The diagnostics often label points that
# are stand out in terms of the diagnostics.
# Three points have labels at the top right
# of the plot but are hard to read due
# to overplotting plotting.
#
# 3.4.2 A Quantile - Quantile plot
#
# In the top right Q-Q plot has a really
# thick right tail. This include three outiers
# with labels that are a little easier to read
# due to less overplotting.
#
# In the plot y-axis has standardized residuals
# When the model errors have normal distribution
# with mean zero and a standard deviation, sigma,
# the regression design matrix leads the model
# residuals to have particular covariance matrix.
# The expected variance of each residual can
# vary a lot from case to case.
#
# The standarization will make the residuals
# have the same expected variance, 1.  If the
# model errors were from a normal distribution the
# standardized residuals should look similar to a
# standard normal random variable.
#
# Technically the standardized residual will still
# be correlated but these correllation are near
# zero so usually ignored.
#
# We see the thick tail on the right and see
# the model standarized residuals are quite
# different than randoms variables from a
# standard normal distribution.  This means
# we cannot trust the hypothesis tests and
# confidence interval produce the lm() function.
#
# However the linear model can still compared
# to other models using the Mean Squared Error
# or the adjusted Mean Squared Error.
#
# We can get confidence interval using bootstrap
# methods, but this is outside the current
# scope of this class.
#
# Sometimes the regression diagnostics point the
# a much better linear models were the assumption
# of normal distribution errors is reasonable.

# # 3.4.3 A spread - location plot
#
# The bottom left plot is a spread-location plot.
# The y-axis shows the square-root of
# the absolute value of standardized residuals.
# Taking the absolute value flips negative residuals
# to have positive y values.  The square-root makes the
# vertical spread of points have a center closer to 1.
# The red smooth line should be close to horizontal
# line at y=1. This would indicates the error
# distribution variance does not change as the
# fitted value change.  The would be the case if the
# assumption were true the thes error are independent
# and identially distributed.
#
# The curvature in the red line indicates the
# the assume is false.
#
# A sequence of random variables is called
# homoscedastic if all variables in the sequence
# have the same finite variance.  If the variances
# differ the sequence is called heteroscedastic.
#
# Scale and location plots can also
# be made with model predictor (explantory)
# variables use on the x-axis. These can
# reveal model problems and sometimes
# suggest ways to improve the model.
#
# Sometimes it is known that case values have
# different variances and have reasonable
# estimates of the variance or standard errors.
# For example the estimates of mortality rates
# for a large population states have a much small
# standard errors than those for small population
# states.
#
# Inverse variance weights can be used in
# regression model to provide much better models.
#
# Often single statistic can warn of problem
# but may fail to such how to address the
# the problem.
#
# 3.4.4 Standarized residuals versus leverage
#
# At the bottom right, the standardized residual
# versus leverage plot has high leverage points
# on the right.  Those with large magnitude
# residuals have high influence on the fitted
# values and coefficient estimates. They are
# pulling the smooth upward and away from 0.
#
# One point has a high leverage of 0.027
# (the maximum from section 2.3 above)
# As a rule of thumb the leverage
# values above 2*p/n = 2*2/506= 0.008 are high.
# Here p=2 is the number of variables in the
# model counting the column of 1's used to estimate
# the mean and n is 506, the number of cases.
#
# Some high leverage cases may be compatible
# with many other cases. They are not pulling
# the fit toward the selves so are not
# called called influential points.
#
# We might choose to omit or down weight
# influential points.  Fairly often this
# allows other points pop out as having
# high influence.
#
# As for the solid red line(the smooth of
# the standardized residuals) big departures
# from the horizontal line with y = 0 are
# warning that this an inappropriate model
# for making inferences using the standard
# methods.
#
# When red dashed lines appears in this
# diagnostics plot these are contours for Cook's Distance and
# indicate extreme include magnitudes.
#
# 3.4.5  Normal distribution hypothesis test
#
# We can test for the normality of the
# standardized residuals  For small data
# sets is not very powerful.  Looking
# at the Q-Q plots is recommended.
#
# For very large data sets the model can
# be quite good even though the null
# hypothesis is rejected.


shapiro.test(rstandard(lm.fit))

#============================================

#4. Multiple Linear Regression
#
#
# Look at the top row of the scatterplot matrix
# with the median house price, on the y-axis.
# We see how the loess smooths relate to
# the bivariate distributions.  The lstat and rm
# variables tracked the shape of the bivariate
# distributions fairly well but there is a lot
# of vertical variation about the curves. Look at
# the high house value points and see how far they
# are from the smooth curves. Many of these points
# are going to be outliers when fitting a single
# predictor variable.
#
# If a really good fit is to be found it will
# take some additional variables to address the points
# with a big vertical distance to the curve in
# the lstat plot. None of the other smooths in the
# top row look very promising in this regard.
#
# Later we will see that a random forest model is
# better suited to modeling this data.
#
# 4.1 R model specification syntax
#
# "~" means is modeled by
#
# "." means include all other variables
#
# "+" means include variables
#
# "-" means omit variables
#
# #___________________________
#
# "a:b" means interaction (product) of
#       the variables.  Same as I(a*b)
#
# "a*b" means include both variables and
#       their interaction a + b + a:b
#
# Since * is special model symbol
# symbol the notation I( ) supports
# use ordinary multiplication as to
# create a new variable as in I(a*b)
#
## Run: fit all other variable

lmAll.fit=lm(medv~.,data=Boston)
summary(lmAll.fit)

# Run: fit both variables

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

# Run: fit all other variables
#  except age

lm.fit=lm(medv~.-age,data=Boston)
summary(lm.fit)

# A model updating option with the
# same results as above

lm.fit=update(lm.fit, ~.-age)
summay(lm.fit)


# Compare some summaries

#* product of interactgon lstat+age+lstat*age
summary( lm(medv~lstat*age,
  data=Boston))
#Icommpute the product and add up to the model mathematical product
summary( lm(medv~lstat+age+I(lstat*age),
  data=Boston))

summary( lm(medv~lstat+age + lstat:age,
  data=Boston))

#--------------------------------------

# 4.2 Syntax for polynomial, spline and
#     other transformations of predictors
#
# The following fits a quadratic model
# in lstat.



# A linear model in lstat
lm.fit = lm(medv~lstat)

# A quadratic model in lstat
lm.fit2 = lm(medv~lstat+I(lstat^2))

summary(lm.fit2)
plot(lm.fit2)



# Is the quadratic model
# a significantly better fit?
# We can use anova()to compare two
# regression models when one
# model just includes additional
# variables.

anova(lm.fit,lm.fit2)

# The fit is much better. The p value
# is very close to zero. Still,
# when the residuals are inconsistent
# with model assumptions
# they undermine the foundation for
# making probability statements.
# We can still talk in terms of
# R-squared improvement.
#
# The syntax poly() supports fitting
# polynomial models and ns() supports
# fitting natural splines.
#
#
# On the left consider all the points with low
# status percent between 0 and 10.  The range of
# the median house values is large.  A regression
# line cannot fit this data very well.  Another
# predictor variable that will spread the values
# out over the x-axis will have a much better
# chance of improving the fit.
#
# On the far right side of x-axis there is not
# much data to tie down the polynomial model.
# The curve looks too curved.
#
# Polynomials can have more peaks and valleys than
# we would like.  Smoothing splines are better
# behaved.  A natural spline is below.

lm.fit4=lm(medv~ns(x=lstat,3))
summary(lm.fit4)

(ggplot(Boston,aes(x=lstat,y=medv))
 + geom_point(shape=21,fill="red",
     color="black",size=2)
 + stat_smooth(method="lm",formula=y~ns(x,5),
     color="blue",fill="cyan")
 + labs(
     x="Lower Status Percent of Population",
     y="Median House Value ($1000)",
     title="Boston Housing Data")
 + hw
)

#
# The fit looks better on the right.  There are
# still big positive residuals for x in the
# interval [5 10].
#
# We can make log and power transformations. This
# does not conflict with the special with model
# syntax.


summary(lm(medv~log(rm),data=Boston))

lm.fit6 = lm(medv~poly(lstat,2)+poly(rm,2)+ptratio,
   data=Boston)
summary(lm.fit6)
plot(lm.fit6)

# Now we are up to an R-squared of about 0.77 with
# this model.  However the single point 365 is really
# influencing the regression coefficients.
#=================================================

# 5. The model matrix
#
# R transforms functions of predictor variables
# and factors (categorical) and
# variables interactions into a model matrix.
# The model matrix is used to fit the data.
#
# 5.1 Looking at the model matrix produced
#     by continuous variable transformations
#
# We can access the model matrix variables
# produced using variable transformations
# such as poly() and ns().  We can look
# at the columns of values and at their
# correlation matrix.
#
# ## RuncorMat




corrplot(corMat,method="ellipse",
  mar=c(0,0,2,0),  # space for title
  title="Model Matrix Variable Correlations")
#
# 5.2 The model matrix for factors
#
# R converts qualitative variables encoded as
# factors into columns of the model matrix.
# Conceptually it is easy to think about using
# a column of indicator variable to represent
# each level of factor. If there three levels,
# small, medium and large, the indicator column
# for small a 1 for cases with the
# level small and 0 otherwise. The medium
# column would have a 1 for cases with level
# medium and 0 otherwise. The large indictor
# column follows the same pattern.  This matrix
# of indicators is transform into a matrix with
# one less column so that no linear combination
# of these column will produce a column of 1's
# that is used to model the mean.
#
# We skip the tranformation options and detailsin this class.
# A cursory treatment appears in ISLR 3.3.1.
#
# For a better understanding see the discussion
# in Linear Models in R by Julian Faraway.
#
# The Boston housing does not have factors.  It
# does have indictor (dummy) variables, such as chas,
# for the tract being by the Charles River. Since
# it does not a have column for not being by the Charles
# River it does not have to be transformed.
#
#
# 6.  Correlation
#
# Many people see to like correlation plots
# The ggAlly package has function that can do a lot
# with scatteplot matrices and correlation plots.
#
# Here we again use corrplot form the corrplot packages.
# This supports a host a variations.
#
# A good option for showing patterns is sort the
# variables by the first priciple component.
# Below the argument order="FPC" does this.
# There are some other reasonable sorting criteria.
#
# 6.1 Original predictor variable correlation plots

# Produce the correlation matrix

# lmAll.fit from Section 4.1

varMat <- model.matrix(lmAll.fit)[,-1]
varCor <-cor(varMat)

# Plot Correlations

corrplot(varCor,order="FPC",method="ellipse",
   tl.col="black", mar=c(1,1,3,0),
   title="Boston Housing Variables")

# Include numeric values

# The narrow elpipses have high correction.
# The positive slope and blue indicates
# positive correlations


corrplot(varCor,order="FPC",method="ellipse",
  tl.col="black", mar=c(1,1,3,0),
  title="Boston Housing Variables")

corrplot(varCor,order="FPC",add=TRUE, # add the above plot
  type="lower", method = "number",,number.font = 2,
  number.cex=.75,col="black",
  diag=FALSE,tl.pos="n", cl.pos="n")

# Of course one can plot bars and can use
# position along a scale to represent correlation
# magnitude.  Two color represent positive and
# negative.
#
#
# For some occasions audiences correlation plots may be
# appropriate.
#
# After seeing a scatterplot matrix that shows
# bivariate densites and smooths, the correlation
# matrix seems a tremendous simplification.

# 6.2 The variable inflation factor
#
# One problem with predictor variable collinearity
# (correlation) is that this inflates the
# estimated variance of regression coefficients.
#
# We can regress the ith predictor variable on the other
# predictor variables to obtain the R-squared value, Ri**2.
# The variance inflation factor for the ith regression
# coefficient is 1/(1-Ri**2).
# If the ith predictor were linearly independent of other
# predictor Ri**2 would be 0 and there would no inflation.
#
# It easy to obtain the variable inflation factor using
# saved model results.


vif(lmAll.fit)

 #

