# Topic:  Linear Regression
#         Best Subset Selection Methods
#
# Reference:
#   Introduction to Statistical Learning with R
#    = ISLR
# R script source:
#   www.StatLearning.com script for Chapter 6
# R script comments, added graphics, and
#   redesign opportunities by Daniel Carr
#================================================

# Due: For practice
# 1.2 splom  (1)
# 1.3 splom  (1)
# 1.3 Modification
#      Use the first 4 variables and putouts
#      Change xbin to 20, border to gray(0)
#     splom (2)

# 3.3 Answer the
#     question at the end  (1)
# 6.2 Last plot            (1)
#================================================

# Sections
# 1.  Missing data removal and quick looks
#     at the Hitters data
# 1.1 A first scatterplot matrix with overplotting
# 1.2 A second scatterplot matrix with hexagon binning
#     and smooths
# 1.3 A third scatterplot matrix with fewer variables

# 2.  Finding best subsets

# 3.  Redesigning a table
# 3.1 Gathering and organizing the information
# 3.2 Producing the plot

# 4.  Best subset for 19 variables
#     and model evaluation criteria
# 4.1 Four plots per page showing 4 criteria
#     and with dots indicated best models
# 4.2 Plot mfrow() redesign comments
# 4.3 Redesign comments for one of the
#     ISLR plot designs

# 5.  Forward and backward stepwise selection

# 6.  Split cases into training and test sets
#     Model the training set and use
#     coefficients to predict test set
#     values and obtain the MSE using
#     observed - predicted values.
# 6.1 Defines a prediction function
# 6.2 10 fold cross validation and
#     best subset selection
#==========================================

# 0. Setup

library(ISLR)
library(leaps)
library(lattice)
library(grid)
library(hexbin)

# 1. Missing data remove and quick looks at
#   the Hitters data

head(Hitters)
names(Hitters) # same as colnames
dim(Hitters)  # rows and columns
# 322 20

# Check for missing data
sum(is.na(Hitters$Salary))

# Data preparation:
#   For this assignment
#   omit rows with missing data
#
#   Sometimes the missing data
#   has a story to tell

Hitters = na.omit(Hitters)

# Quick checks on removal

dim(Hitters)
# 263 20

sum(is.na(Hitters))

# Below is a default splom
# showing both continuous and
# categorical data

# 1.1 First quick scatterplot matrix

# The cex argument controls the symbol size,
# The varname.cex control the name size
# The varname.col controls is color
# The pscale = 0 turns off the diagonal
#    axis scales.

splom(Hitters, as.matrix = TRUE,
   cex = .5,varname.cex = 0.56,
   varname.col = "red", pscale = 0 )

# It may take this plot a while to appear.
# Once displayedcan quickly see there are
# three categorical variables with two values
# each. The plotting coordinates are the
# subscripts, 1 and 2, to their two factor
# levels.

# 1.2 A second scatterplot matrix with hexagon binning
#     and smoothes.
#
# We exercise more control in the plot production.
# Scatterplot binning and smoothing functions
# are not designed for categorical variables
# so we keep just the numeric variables.
#
# A data.frame is also a list structure.
# Each column is an element of the list.
# The sapply function arguments are
# a list and a function to apply to
# each element. We ask each column
# if it is numeric.

keepVar <- sapply(Hitters, is.numeric)
keepVar
HittersN <- Hitters[, keepVar]

# I choose to show the dependent variable
# at the top left corner make it the
# first variable in the data.frame.

names(HittersN)
HittersN <- HittersN[,c(17,1:16)]

# There is a lot of lattice syntax below.
# It is enough to notice few things.
#
# The as.matrix=TRUE, makes the
# plot diagonal go from the top
# left the bottom right.
#
# The panel function for off diagonal
# panels passed arguments to
# a panel.grid function,
# a panel.hexbinplot function and
# a panel.loess function.
# In general, panel functions
# from those available can be added
# or removed at will. This approach
# the thought collection of panel
# functions make lattice a really
# sophisticated graphics package.
#
# We can also change arguments inside the
# panel functions. Two that I emphasize
# in hexbinplot function are
# xbins and trans arguments.
#
# Here xbins = 12. I chose a small number
# because the panels are small.  Coarse
# binning will suffice for visual purposes.
#
# Here trans = function(x) x^1.
# (This could have been written
#      trans= function(x))
# This transforms the bin cell counts and
# the result is then encoded as gray level.
# Darker means more counts.
#
# If the cells counts are from 1 to
# 100000, a better choice might be
# trans = function(x) x^0.2
# Then values from 1 ^ .2 = 1 to
# 100000 ^ 0.2 = 10 would be converted
# to gray level and this provide some
# resolution for intermediate count
# cells.

splom(HittersN, as.matrix = TRUE,
  xlab = '',main = "Hitters Data ",
  pscale = 0, varname.col = "red",
  varname.cex = 0.56, varname.font = 2,
  axis.text.cex = 0.4, axis.text.col = "red",
  axis.text.font = 2, axis.line.tck = .5,
  panel = function(x,y,...) {
    panel.grid(h = -1,v = -1,...)
    panel.hexbinplot(x,y,xbins = 12,...,
      border = gray(.7),
      trans = function(x)x^1)
    panel.loess(x , y, ...,
      lwd = 2,col = 'purple')
  },
  diag.panel = function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm = TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col = gray(.8),lwd = 2)
    diag.panel.splom(x, ...)
 }
)

# What do we see?

# We scan the top row of smoothes.
# The first five plots show similar linear
# upward trends that rises roughly to the
# mid-point of the Salary range.  None of the
# input variables by the themselves is a good
# predictor of high salary.
#
# The domain scatterplot matrix for the
# five input variables (AtBat, Hits,
# HmRun, Runs and Walks) show pretty
# strong linear relationships. That is
# these are strongly correlated
# input variables.
#
# The domain scatterplot matrix for
# Years, CAtBats, CHits, CHmRuns,
# CRuns, CRBI and Walks have pretty
# tight linear trends. Players need
# good yearly statistics to play many
# and many year of play to accumulate
# high counts.
#
# Looking back up at top row Salary trends
# these variables show a bit of an
# increase followed by a flat line or a slight
# declining line.
#
# The last three variables are
# defense related variables.
# The PutOuts variable shows
# a continuing increase in
# salary across it range
# but number of cases at the
# top right of the plot is small.
#
# Did you find the plot with
# and unusual bifurcation?
# If not, scan for it. Might
# this indicate two populations
# players?

# 1.3 Third Scatterplot matrix with fewer variables

# This shows continuous variables from a model
# fit in Section 4.2

splom(Hitters[,c(19,1,2,6,8,11:13,16,17)],
  as.matrix = TRUE,
  xlab = '',main = "Hitters Data ",
  pscale = 0, varname.col = "red",
  varname.cex = 0.65, varname.font = 2,
  axis.text.cex = 0.4, axis.text.col = "red",
  axis.text.font = 2, axis.line.tck = .5,
  panel = function(x,y,...){
    panel.grid(h = -1,v = -1,...)
    panel.hexbinplot(x,y,xbin = 12,...,
      border = gray(.7),
      trans = function(x)x^1)
    panel.loess(x , y, ..., lwd = 2,
      col = 'purple')
  },
  diag.panel = function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm = TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col = gray(.8),lwd = 2)
    diag.panel.splom(x, ...)
 }
)

# Now we can begin to see where high and low count cells
# support the smoothes. Some parts of the smoothes may
# support by a lot of data, and some by very little.

# 2. Finding best subsets______________

# The leaps package is designed to find
# the best subsets for 1 to p variables:
# the best 1 variable model, the best 2
# variable model and so on. The mean is
# is included in the model but only data
# variables are numbered further below.
#
# For p variables there are 2**p subsets counting
# the null subset the would fit only the mean.
# The well-designed leaps algorithm identifies
# non-competitive subsets of variables so avoids
# unnecessary checking.

regfit.full = regsubsets(Salary~.,Hitters)
reg.summary <- summary(regfit.full)
reg.summary

#=============================

# 3. Redesigning a table using
#     base level R graphics
#
# The table is a candidate for redesign.
# We typically can't see the whole table
# layed out in rows and columns in the R
# console.
#
# The " "s are chart junk that make it
# harder to spot the "*"s.
#
# Here a binary heat map could work
# with variable names as rows
# number of variables in model as columns.
# However we will just plot dots in occupied rows
# and columns.
#
# Focusing and sorting:
# We can drop the unused variables
# and reorder the remaining variables
# by how often they appear in the model.
#
# The needed information for the redesign
# in the regfit.full list structure
# but it easier to obtain from the
# reg.summary$outmat matrix.

# 3.1 Collect and organize the needed information

mat <- reg.summary$outmat
mat
rownames(mat)

# All the models fit the mean.
# The row names include  "( 1 )"
# as a reminder.

# Simplify the row names
rownames(mat) <- 1:nrow(mat)
head(mat)

# Can you find the "*" in the first row?
# It is under CRBI. The "*" in the second
# row is easier to find

# Here we focus on the the best models with
# 1 to 8 variables

mat <- mat[1:8,]
mat

# convert matrix to logical values
# and transpose the matrix

matL <- ifelse(mat == " ",FALSE,TRUE)
matLT <- t(matL)
matLT

# Keep rows that have 1 or more TRUEs

usedVar <- apply(matLT,1,any)
matLT <- matLT[usedVar,]
matLT

# Above the apply function operates
# on its first argument, a data matrix.
# The 2nd argument of 1 applies
# the function in 3rd argument to
# each row of the matrix.
# The function any()
# works on a logical vector and is
# TRUE if any element is TRUE

# Sort the remaining variables by
# the number of times used

usedN <- apply(matLT,1,sum)
matLT <- matLT[order(usedN,decreasing = TRUE),]
matLT

# Find the number of rows and
# columns used to set up
# axes for the plot

nModel <- ncol(matLT)
nVar <- nrow(matLT)
rx <- c(.5,nModel + .5)
ry <- c(.5,nVar + .5)

# Construct coordinates for rows
# and columns. Omit those with
# FALSE in the matrix and
# plot the remaining points

xy <- expand.grid(list(x = 1:nModel,y = nVar:1))
xy <- xy[t(matLT),]
xy

# 3.2 Produce the plot

# Setup the plot
plot(rx,ry,type = 'n',axes = FALSE,
  xlab = '', ylab = '', cex = .85,
  main = paste("Best 8 Salary Models",
  "with a Mean and 1 to 8 Variables",sep = "\n"))

points(xy$x,xy$y,pch = 21,bg = "red",cex = 1.5)

# Add grid lines for perceptual grouping
hline <- seq(3.5,nVar,by = 3)
vline <- seq(3.5,nModel,by = 3)
abline(h = hline, col = gray(.8))
abline(v = vline, col = gray(.8))

box()  #  draw the plot border

# put labels in the plot margins
mtext(rownames(matLT),side = 2,at = nVar:1,
  line = .3,las = 1)
newColNam = substring(colnames(matLT),1,2)
mtext(newColNam,at = 1:nModel, side = 3)

#======================

# 3.3 Question

# The results look like forward stepwise
# regression through 6 variables. All the
# previously selected variables are included
#
# In comparing the 7 variable model
# to the 6 variable model,
# which variables were added and which
# were dropped?
#
#=======================

# 4.  Best subsets for 19 variables
#      and model evaluation criteria

regfit.full = regsubsets(Salary~.,data = Hitters,
  nvmax = 19)
reg.summary = summary(regfit.full)
names(reg.summary)

reg.summary$adjr2
which.max(reg.summary$adjr2)
reg.summary$adjr2[which.max(reg.summary$adjr2)]
# R-squared in percent
# The percent of variance
# "explained" by the model.
# Below rsq is R-squared

round( 100*reg.summary$rsq, 1)

# Some regression statistics notation
# rss: residual sum of squares
# n: sample size
# mse: mean square error = rss/n

# Compute model mse for the
# best 10 variable model.
model10.mse = reg.summary$rss[10]/nrow(Hitters)
model10.mse

#=======================

# 4.1 Four plots per page showing 4
#     model selection criteria
#     with dots indicating best models
#
# Uses R base level graphics
# mfrow allocates space to plot
# panels in row and columns
# Each plot() command advances to the
# next plot.  Below the points() function
# add points to the currently
# active plot

#windows()
X11()
par(mfrow = c(2,2))

xlab = "Number of Variable"
# 1st row 1st  column
plot(reg.summary$rss,xlab = xlab,
  ylab = "RSS",type = "l")

# 1st row 2nd  column
plot(reg.summary$adjr2,xlab = xlab,
  ylab = "Adjusted RSq",type = "l")
  loc <- which.max(reg.summary$adjr2)
  loc
  points(loc,reg.summary$adjr2[loc],
    col = "red",cex = 2,pch = 20)

# 2nd row 1st column
plot(reg.summary$cp,xlab = xlab,
  ylab = "Cp",type = 'l')
  loc <- which.min(reg.summary$cp)
  loc
  points(loc,reg.summary$cp[loc],
    col = "red",cex = 2,pch = 20)

# 2nd row 2nd column
plot(reg.summary$bic,xlab = xlab,
  ylab = "BIC",type = 'l')
  loc <-  which.min(reg.summary$bic)
  loc
  points(loc,reg.summary$bic[loc],
    col = "red",cex = 2,pch = 20)

# The red dot indicate the number
# of variables that is best to use
# based on the particular criterion
# chosen.
# Adjusted Rsq 11 variables
# Cp statistic 10 variables
# BIC           6 variables

# Regression comments:
# The BIC (Bayesian Information Criterion)
# has a minimum at six variables. The BIC
# is often chosen the preferred model
# selection criterion.
#
# The adjusted R-squared has a minimum at
# 11 variables but is not increasing very
# quickly after six variables.  It seems
# reasonable to consider a six variable
# model and look at the coefficients.

coef(regfit.full,6)

#==================

# 4.2  Plot mfrow() redesign comments
#
# Putting multiple panels in a plot with
# mfrow() has been convenient for many
# people over the years but often wastes
# space.  If the window height is
# small the limited y-axis resolution may
# result in unreadable y-axis tick mark
# labels.
#
# In the above example there is common x-axis.
# There could be 4 vertically aligned panels with
# common x-axis and grid lines.  Puttinggrid
# line labels at the bottom of the stack would
# suffice.
#
# We could keep the two column design and
# remove all the wasted space to better show
# the statistics.
#
#================================

# 4.3 Redesign comments for one of the
#   ISLR plot designs

dev.off()
#windows()
X11()

par(mfrow = c(2,2))
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")

# Each  panel feature a particular
# evaluation criterion.  The
# top line represent models the best model.
# and the worst is at the bottom
#
#  Plot size matters for readability!
#
#  The color encoding have poor
#  perceptual accuracy of extraction
#  The chosen mapping of values to
#  to gray provide the best gray
#  level discrimination for the
#  for the worst models.
#  Position along a scale encoding is best
#
#   Note that the BIC plot has a
#   reversed y-axis to put the
#   "best" model on the top row.

# We can show the plots individually to better
# see the intended content.

dev.off()

windows()
X11()
plot(regfit.full,scale = "bic",main = "BIC")

# The plot is best thought of as a row and
# column matrix.  The top row is the best model
# and has six variables in addition to the mean
# in the left column. The gray and black rectangles
# appear over variables in the model.
# The most negative BIC values are associated with
# best fitting models. There row labels are
# the rounded BIC values. As indicated above the most negative
# BIC value are at the top of the y-axis.
#
# The 2nd row is the 2nd best BIC model
# It has a mean and eight variables. Compared to the
# to top rwo the  black rectangles indicate
# three add variables and the droping CRBI

# Further below will look at at the same
# four model selection criteria but in the
# the context of 10 fold cross validation.
# Cross validation provides a more
# contemporary approach model selection.

# 5. Forward and backward stepwise selection

# The simple message here is that forward, backward
# and best subset selection don't always
# pick the same variables for a given number
# of selected variables in the model

regfit.full = regsubsets(Salary~.,data = Hitters,
  nvmax = 19)  # all subsets by default
# summary(regfit.fwd)

regfit.fwd = regsubsets(Salary~.,data = Hitters,
  nvmax = 19,method = "forward")
# summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~.,data = Hitters,
  nvmax = 19,method = "backward")
#summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Using the match function
# We can quickly see
# variables that are not
# present in all three models

namFull <- names(coef(regfit.full,7))
namBwd <- names(coef(regfit.bwd,7))
namFwd <- names(coef(regfit.fwd,7))

match(namBwd,namFull) # 3 differ
match(namFwd,namFull) # 3 differ
match(namBwd,namFwd)  # 1 differs

# 6. Split cases into training and test sets.
#    Model the training set and use
#    coefficients to predict test set values
#    and to obtain the MSE using
#    (observed - predicted) values.

set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters),
  replace = TRUE)
test = !train

table(train)
# Find the best training set models
regfit.best = regsubsets(Salary~.,
  data = Hitters[train,],nvmax = 19)

coef(regfit.best,7)

# Obtain the test set design matrix
test.mat = model.matrix(Salary~.,
  data = Hitters[test,])

# Reserve storage for the mean square error
# using train set coefficients for
# for test set data


val.errors = rep(NA,19)

# Run for each number of variables in the model

for (i in 1:19) {
  # obtain the training set coefficients
  coefi = coef(regfit.best,id = i)

  # predict test set values
  pred = test.mat[,names(coefi)] %*% coefi

  # Obtain the MSE
  val.errors[i] = mean(
    (Hitters$Salary[test] - pred)^2)
}

round(val.errors)

loc = which.min(val.errors)
loc

# The 10 variable model had the
# the smallest test set MSE

round(coef(regfit.best,loc), 3)

val.errors[10]

# =============================

# 6.1 Define a prediction function

# This function definition uses
# R syntax not taught in the class.
# It suffices to define the function
# and skip the details.

predict.regsubsets =
  function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object,id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

#=====================

# 6.2 10 fold cross validation
#     for best subset selection
#
# Fit again with all the data
# Get the coefficients of the
# the best 10 variable model
#
# Remember this was best for the
# Cp criterion

regfit.best = regsubsets(Salary~.,
  data = Hitters,nvmax = 19)
coef(regfit.best,10)

# Define a vector used to produce
# k=10 distinct subsets of cases.
# The subset sizes are random
# with an expected size of
# nrow(Hitters)/k

k <- 10
set.seed(1)
folds <- sample(1:k,nrow(Hitters),
  replace = TRUE)
table(folds)

# Create matrix with missing values
# to store the cross-validated
# MSE based on the k fold test sets
# for models with 1 to 19 variables

cv.errors = matrix(NA,nrow = k,ncol = 19,
  dimnames = list(NULL, paste(1:19)))

# The fold and number of variables loops

for (j in 1:k) { # fold loop

  # The 19 best models with jth fold omitted
  best.fit = regsubsets(Salary~.,
    data = Hitters[folds != j,],nvmax = 19)

  # The MSE for the fold prediction error
  for (i in 1:19) {# number of variable loop
    pred = predict.regsubsets(best.fit,
      Hitters[folds == j,],id = i)
    cv.errors[j,i] = mean(
      (Hitters$Salary[folds == j] - pred)^2)
  }
}

# Find the mean across the fold MSE for
# each model

mean.cv.errors = apply(cv.errors,2,mean)
round(mean.cv.errors)
par(mfrow = c(1,1))
plot(mean.cv.errors,type = 'b')
loc = which.min(mean.cv.errors)
loc

# The plot shows that Model 11 is barely
# better than Model 10. The difference
# in the 4th digit. I would be inclined
# to go with the simpler model.

mean.cv.errors[10]
mean.cv.errors[11]

# The average 10 fold MSE for model 10
# is much larger than model 10 MSE
# from section 2.2

model10.MSE

# The cross validation tells an overfitting story.
# It leads to a different model than the
# often used BIC criterion.
#
# There still may be better models.
# There may be non-linear models, non-parametric
# models, or even linear models with interactions
# and transformed variables.

# Of the crossvalidation linear models here
# the authors go with model 11 as best model
# Its coefficients are shown below.

reg.best = regsubsets(Salary~.,data = Hitters,
  nvmax = 19)
round(coef(reg.best,loc), 3)

