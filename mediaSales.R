# Advertizing and MSE

# Nothing Due
# Sections not listed

# 0. Setup

library(tidyverse)
source('hw.R')
library(rgl)
adSales<-read.csv(file="Advertising.csv", header=TRUE, sep=',')

fixLights <- function(specular = gray(c(.3,.3,0))){
  clear3d(type = "lights")
  light3d(theta = -50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[1])

  light3d(theta = 50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[2])

  light3d(theta = 0,phi = -70,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[3])
}

# 1. Read the advertizing data to make a tibble

adSales <- read_csv('Advertising.csv')
adSales

# Remove the counting integers in column 1
adSales <- adSales[,-1]
adSales

# we are going fit a linear model with
# Sales as the dependent variable. The
# Unit of measure is
# "Thousands of Units Sold".
#
# Our linear model input variables are:
# Radio, TV and Newspaper expenditures.
# The units of measure are all "Budget in $1000s".
#
# Input variables may be called explanatory
# variables, predictor, or independent
# variables in different contexts.
#
# Looking at univariate summary statistics provide
# one way to start learning about the data.

summary(adSales)

# Scatterplots are often more interesting because
# they can suggest functional relationships and the
# data domain the support the model.

# 2. Scatterplots and models y = f(x) + e where
#    y is Sales and x is one of the three
#    input variables
#
#    The plots are similar to those
#    the ISLR text, Section 2.

pTV <- ggplot(adSales, aes(x = TV, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') + hw +
  labs(x = 'TV Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results')
pTV

# The vertical spread of the points
# increases from left to right.

pRadio <- ggplot(adSales, aes(x = Radio, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Radio Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') + hw
pRadio

# Same is true here

pNews <- ggplot(adSales, aes(x = Newspaper, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
labs(x = 'Newspaper Budget in $1000s',
  y = 'Thousands of Units Sold',
  title = 'Advertising Results') + hw
pNews

# Here the vertical spread of points looks fairly uniform
# going from left to right up to 5500 units. The density
# of points reduces after this. There are only two
# points larger the $90,000 and in this part of the plot
# it is hard to assess the local variation in thousands
# of units sold as a function of the budget.

# 3. Panel based plot comparisons
#
#    The one input variable at a time plots
#    can be  misleading if we make a quick visual
#    comparisons of the fitted line slopes.
#    Two reasons are:
#
#    1) The Sales are the combined
#    results from all types of advertising. The
#    plots just provide 2-D margin views of the
#    higher-dimensional data structure. (A rotating
#    3-D ray glyph plot can show all four variables.
#    Ray angle encodes the units sold.)
#
#    2) The ranges of the x-axis scales are different
#    in the plots above.  This influences the angles
#    of regression lines in each plot so complicates
#    their direct visual comparison. Below we use
#    facet_grid to juxatapose panel plots that
#    provide a common x-axis.
#
#    In ggplot graphics, we use factor levels to specify
#    tibble rows that are to be plotted in separate panels.
#    Then we use the facet_grid function to specify the
#    layout of panels within the composite plot.
#
#    Below we use the gather() function to do the following
#    1) Stack the TV, Radio, and Newspaper budget
#       columns into  a single long column.
#    2) Create factor that distinguishes Newspaper, TV and Radio
#       rows.
#    3) Stack the values of other variables (here sales)
#       in long columns. Yes, each sales value will appear
#       three times.
#
#    The gather() function below makes this relatively easy
#    as shown below.
#    The first argument is the input tibble name.
#    The "key" argument names the column with the panel factor
#    The "value" argument names the stack column of selected
#                variables
#    The TV:Newspaper is a convenient way to specify
#       a set of adjacent columns to be stacked.
#       (The column names can be specified as
#       as individal argments separated by columns)
#    The factor_key = TRUE says the column specification
#       order such become the levels of the factor.
#       (No alphabetic sorting)


adSales

adSalesG <- gather(
  adSales,
  key = "Media",
  value = "Budget",
  TV:Newspaper,
  factor_key = TRUE
)

adSalesG



ggplot(adSalesG, aes(x = Budget, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') +
  facet_grid(Media ~ .)  + hw

# Note the media labels on the right of the
# three panels.
#
# Now the x-axes are the same and we
# clearly see that Radio has the steepest slope.
#
# The fitted blue lines suggest that $50,000 for Radio
# adversizing yielded about same units sold as $300,000 for
# TV sales.
#
# We can really know if the units sold will continue
# to increase with an increase budget because we don't
# have data.
#
# Which slope is larger,
# TV or Newspaper?

# For comparison proposes superposed panels
# provide an alternative juxtaposed comparison
# Below we use the color aesthetic in the geom_smooth
# to distinguish the Media .


ggplot(adSalesG, aes(x = Budget, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(color = Media)) +
  labs(x = 'Budget in $1000s',
    y = 'Thousands of Units Sold',
    title = 'Advertising Results') + hw

# It looks to me like the TV slope is smaller.
# Let's check.

lm(Sales~TV,data = adSales)
lm(Sales~Newspaper,data = adSales)

summary(lm(Sales~TV,data = adSales))
summary(lm(Sales~Newspaper,data = adSales))

lm0<-lm(Sales~Newspaper,data = adSales)
predict(lm0,newdata)
# Why was so much money spent for TV advertizing!
# when the sales increase per $1000 is smallest.
# Below we will see why Newspaper spending
# drops out of the multiple linear regression
# model.
#


# 4. The regression input variable domain.

# The domain of the input variables
# determines the extend to which
# the model output is supported by actual
# observations. Using input values
# outside the domain to obtain estimates is
# a kind of extrapolation.
#
# Extrapolation should always be viewed as
# risky endeavor. Knowledge about the
# phenomena may suggest taking a little risk.

#4.1  Below we produce a 3D scatterplot
#     to parts of the data domain with
#     points relative the range (min and max)
#     for each of the variables

domain <- as.matrix(adSales[, -4])
open3d(FOV = 0)  # no perspective projection
fixLights()
plot3d(
  domain,
  type = "s",
  radius = 3.2,
  col = rgb(1, .2, .2),
  aspect = TRUE
)

# The rgl plot may appear in a small
# window at a strange place on your screen,
# such along the base of the screen or behind
# the RStudio window.
#
# Once located, left click on the plot lower right corner
# and drag to change its size.
#
# Left click on the top blue bar
# and drag to move the plot.
#
# Left click in the plot and drag
# to rotate the cube.
#
# Right click and move down or up
# to zoom in or out respectively.

# Look at the 8 corners of the plot.
# Are there are data points near the
# corners? If so that reduces extrapolation
# concerns a little.
#
# In linear regression, points on edges
# of the domain data cloud are more influential
# than points in the center of the cloud.
# If one or more of the points have anomolous
# dependent variable values extrapolation
# can still be bad.

# For starters we see a large empty volume
# around the high Newspaper,low Radio and
# low TV corner.
#
# We can rotate the plot to focus 2D margin
# domains and look areas with little or no
# data. Absence near the edges correspond
# to cube faces. Alternativey we make
# three scatterplots: (TV, Radio).
# (TV, Newspaper), and (Radio, Newspaper).
#
# Sometimes we find big holes in the data
# domain.

# 4.2  A look at 2D Radio and TV Domain

ggplot(adSales,aes(x = Radio,y = TV)) +
  geom_point(shape = 21,fill = 'red',
     color = 'black',size = 2.5) +
  labs(x = 'Radio Budget in $1000s',
       y = 'TV Budget in $1000s',
       title = 'Advertising Data') + hw

# This looks like a reasonably good 2D domain for
# a linear regression model. There are points close
# to the corners and near the edges. There are no
# gaping internal gaps.

# In statistics, the field of experimental design
# addresses the production of regression models
# domains.

# 4.3 Importance of the model domain

# What we can learn from the data is limited
# by the data domain.  This include the different
# kinds of input variables and their observed
# combinations.
#
# Historically many models had just input variable.
# When the phenomena of interest involve multiple
# interacting variables. The one variable at a
# time studies wer often inefficient and misleading.
#
# This inefficiency motivated the development
# experimental design methodology.  Experimental
# design addresses cost efficient selection of input
# variables and their combinations. The application
# of this methodology tremendously changed
# industry production.  In the history of statistics
# R. A. Fisher became well know for his agriculure
# experiments and analyis methods. Edward Deming work
# revolutions the automobile production industry.


# The data domain, with possible transformations,
# feeds into the linear model design  matrix. The
# design matrix impacts both model coefficient and their
# uncertainty.
#
# In the deep learning world a recurrent challenge is for
# analysts to gather data to support the learning of
# patterns. The importance of gathering data that covers the
# data domain of interest remains.

# 5.  Linear Regression

# Below  Sales is the dependent variable
# .  means all other variable are input
#    variables

adModel1 <- lm(Sales~.,data = adSales)
#Equivalent to:
adModel1 <- lm(Sales ~ TV + Radio + Newspaper, data = adSales) #lm = linear model
adModel1
summary(adModel1)

# The Multiple R-squared .897 indicates that the model accounts
# for roughiy 90% of the Sales variability
#
# The adjusted R-square is a little smaller and include a penalty for
# including more variables in a model. We include random noise as variables
# in the model and improve the fit.
#
# Assuming the standardized residuals have roughly a normal distribution we
# make can statistical inferences about the model. The probability of the
# F-statistics being so large at random is basically 0.  The F-statistic compares
# fitting all the variables to fitting just the dependent variable mean.
#
# The t-statistics are based individual variables improving the fit with the other
# variables already in the model.  Is there strong evidence that the
# regression coefficient is not zero?   What is the probability that
# improved fit is due random variation?  For TV and Radio, the probability
# close to 0. For Newspaper, the p value of .86 suggests the using white
# noise has better chance of improving the fit.

# The correlation matrix shows that Newspaper budget is substantially
# correlated (.354) with the Radio budget.
cor(adSales[,1:3])

# In the 1 variable newspaper model,  newspaper get some credit for
# high sales because it was high when Radio sales were high.
# However when TV and Radio are in the model
# the Newpaper budget has almost no impact on the fit.


# 5.1 Regression diagnostics plots

plot(adModel1)

# As indicated in the console hit Return to see the next
# plot in the set of 4 regression diagnostics plots.
#
# Residual versus fitted values plot
# In this plot residual distribution the red line should be centered.
# around zero.  They are not.  This violates a modeling assumption need
# justify making statistical inference model and it coefficients.
#
# In this plot, the points numbered  131, 6 and 179 are low value outliers.
# Removing them may will likely reduce the bend in the red line.
# We might consider deleting the points
# if we have a good reason to think that one or more of
# their value are flawed. Of course then we should also
# wonder there other flawed points are that don't stand
# out as outliers.

# When the  residuals are plotted against a variable
# and the smooth looks like a parabola, including the square
# of variable values in the model will like provide yield a better
# fitting model. Here any variable highly correlated to the
# fitted values will likely be helpful.
#
# Plot 2: the Normal Q-Q plot.
# We see the outliers and a thick
# left tail.  That is, points on the left are far below the
# reference line. The residuals do have an approximately
# normal distribution.  Statistical inference (
# hypothesis tests and confidence intervals) for the
# model as a whole and for the individual terms
# are not justified.

# The right tail is thin. The right-side points
# are on the center-of-the-plot side of the reference line.
# Thin tails are of less concerned in linear regression.

# Plot 3, the scale-location plot
# The y-axis is the square root of the absolute standardized residuals.
# The regression residuals have covariance matrix that is based on the
# design matrix. In general the correlations are ignorable. The variances
# are not.  Standardized residual have been divide by their estimated
# standard deviations.
#
# The absolute value transformation puts all the
# negative residuals on the positive side of the zero.
#
# The square-root transformation helps balance the
# small and large absolute residuals. The red smooth
# line should be y = 1.  The curved linear mean the residual
# don't have the same variance. Our independent
# identically distributed errors assumption
# fails in terms of the mean (Plot 1) and the
# variance (Plot 3)
#
# Plot 4 Standardized Residuals versus Leverage
#
# The high leverage points are on the far right.  The influence of a
# depends on its leverage and have far it would be from the regression
# line if it were omitted.  Points 131 and 6 are high influence points.
# The have substantial leverage and the large standardized residuals.

# 5.2 A TV and Radio model

adModel2 <- lm(Sales~TV+Radio, data = adSales)
summary(adModel2)
plot(adModel2)


#Removing outlier[131]
adModel1 <- lm(Sales~TV+Radio, data = adSales[-131,])
coef()
# Dropping the term didn't change much.

#5.3 Adding an interaction terms for TV and Radio

# In the R linear model syntax
# TV:Radio is an interaction term
# This multiples the TV and Radio vectors and
# includes the resulting vector in the model.
#
# TV*Radio is interpreted as TV + Radio + TV:Radio

# There are three different ways to specify the same model

adModel3 <- lm(Sales~TV*Radio, data = adSales)
summary(adModel3)

# This result is the same
adModel3a <- lm(Sales~ TV+Radio + TV:Radio, adSales)
summary(adModel3a)

# Results of direct mathmatical opertion on
# variables need to be surrounded by ()
# This result is the same
adModel3b  <- lm(Sales~TV + Radio + (TV*Radio),adSales)
summary(adModel3b)

#5.4 Adding a square term

# We can squares the TV budget vector and
# include it in the model

adModel4 <- lm(Sales~ TV*Radio+I(TV^2), adSales)
summary(adModel4)

#mode There is almost no variance left to explain.
# Is this data real or was in generated with
# two outliers included.


plot(adModel4)

# The two outliers are still present on the left but
# the curvature in the residuals has been reduced.

# 5.5 Specifying a quadratic response surface

# The polym() function can be used specify
# quadratic response surface.


adModel5 <- lm(Sales~polym(TV,Radio,degree=2),adSales)
summary(adModel5)

# 5.6  The Mean Squared Error
#
# The Q-Q plot discourages us from making claims base on p-values.
# In any case we can compare models using the MSE.

mean((adSales - fitted(adModel4))^2)
#
# Perhaps we have overfit the data.  We will soon look at 10 fold
# crossvalidation models. That will help us avoid overfitting the data.
