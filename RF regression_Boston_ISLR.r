# Random Forests Boston Housing ISLR.r    
# 
# Sections:
# 
# 0. Setup
# 
# 1. Learning about the data:
#    documentation, statistical summary, table and graphics views.
# 
# 2. A random forest model using all the data and defaults.   
#    Two ways to specify a random forest model
# 
# 3. Variable importance plots and 
#    picking simpler models
# 
# 3.1 Sploms of the more important variables
# 
# 3.2 Progressively removing less important variables
#     seeking a simpler model with nearly the same MSE
# 
# 4. ISLR: A tree model crossvalidated pruning
# 
# 5. ISLR: Bagging Models : Use all 13 variables for each node 
#    Training and test set examples
# 5.1 500 tree model
# 5.2  25 tree model
# 
# 6. ISLR: Random Forest Models with 6 randomly selected
#          variables at each node
#    Training and test set examples
# 
# 6.1  500 tree model
# 6.2   25 tree model
# 
# Due: 4 pts
# 3.1  Last plot
# 6.1  Compute the % of explained for the test set
#      and show the script. 
#      Compare this to the result in 5.1. 
#      Which is higher?  (2 pts)     
# 
# 6.1  Is the ordering variable in terms of 
#      variable importance different from 5.1
# 
# 
# 0. Setup

## Run

library(MASS) # Boston Housing Data
library(randomForest)
library(lattice)
library(hexbin)
  
## End

# 1. Learning about the data:
#    Documentation, statistical summary,
#    table and graphics views 
# 
# A common practice is to jump in and start fitting
# models.  This not quite the same as jumping into
# a pool before seeing if it is filled water or to
# see if the pool is pack with people or perhaps 
# zoo bears or aquarium piranhas.  
# Still I hope the image sticks.
# 
# I can be really helpful (and protective) to get
# advice from a perhaps understand the phenomena
# and frame a person who knows about the
# data gathering, recording and processing.  
# Reading about the field and about related analyses
# is addition way to get up to speed.
# 
# Below we do some token things.  We take look a brief 
# at a document with the variable names and units. We don't
# see a map of the Boston Suburbs from when the data
# collected.
#   
# A quick look at the summary statistics is a start.  We
# might be amazed at how little residences cost back then.
# Whe was back then?  When  was the date collected?  A reference
# dated 1978 likely provides an upper bound, if we don't
# have enough enthusiam to read the two reference.    
# It was a different world back then.  How was percent
# with lower state defined back then?  Do we really care
# about this data?    
# 
# Here start with a quick scatterplot look that
# that shows univariate kernel densities. 
# 
# Run Documentation

#
?Boston

summary(Boston)

# Table view
# There are interactive table views
# that can do a lot more
View(Boston)

# Scatterplot matrix view show binned density 

# Splom plotting function
#   Loess commented out below. 
#     Inadequate domain variable issues. 
#   Hexbin modifed to 12 xbins and trans power set to 1. 
 
offDiag <- function(x,y,...){
    panel.grid(h=-1,v=-1,...)
    panel.hexbinplot(x,y,xbins=12,...,border=gray(.7),
       trans=function(x)x^1)
   panel.loess(x , y, ..., lwd=2,col='purple')
  }

onDiag <- function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
 }

windows(width=9,height=9) 
X11(width=9,height=9)
splom(Boston[,-4],
  xlab='',main="Boston Housing Data",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="red",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

## End

# Given that the dependent variable of interest is medv,
# we can scan the top row as the roughly appears to a
# linear function of rm (average number of rooms
# per dwelling) and lstat(lower status of the population
# in percent).   
# 
# 
# 2. A first random forest model using all the data
#    defaults with two ways to specify the mode;
# 
# The goal is to predict the median price
# of housing (in units of $1000) using the
# first 13 variables. 
# 
# One way to specify a random forest model uses
# integer subscripts to select the dependent
# variable and predictor variables. A quick
# look at the pairs of variables names and
# numbers can be helpful when we use integers.

## Run: Specification using column numbers

varNum <- function(x){
  num <- 1:ncol(x)
  names(num) <- names(x)
  return(num)
}
varNum(Boston)
names(Boston[14])

set.seed(137)
bostonRf0 <- randomForest(x=Boston[,-14],y=Boston[,14],
 data=Boston)
bostonRf0  # Model summary


## Specification using a model formula
#  and using additional arguments

set.seed(137)
bostonRf <- randomForest(medv~., data=Boston,
  importance=TRUE, proximity=TRUE )
bostonRf

## End

# The %Var explained was a little different
# for the two models.  Since the seed was the
# same, the randomization to compute variable
# importance likely caused the change.  
# 
# 3. Variable importance plots and 
#    picking simpler models
# 
# Random forests assesses a variable's importance
# by randomizing the positions a variable's values
# in the vector. The question then is, how much
# #does the model's mean square error increase?
# 
# ## Run

varImpPlot(bostonRf)

## End

# We can see the scambling the values of one of the variables
# such as rm, lstat, nox and dis lead to substantially
# larger MSE models. 
# 
# 
# 3.1 Sploms with hexagon bin counts smooths
#     for the more important variables
# 
# The four lowest importance variables are
# zn, chas, rad, and black.  Below we removed
# these be using by omitting their numbers.

## Run 

varNum(Boston)
Boston2 <- Boston[,-c(2,4,9,12)]

# hexbin modified to 15 xbins and trans power set to 0.5.  
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
    trans=function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}

windows(width=9,height=9) 
splom(Boston2,
  xlab='',main="Boston Housing: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

## End

# We might choose to focus on the four most important
# variables  

windows(width=9,height=9) 
splom(Boston[,c(8,5,13,6,14)],
  xlab='',main="Boston Housing: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)


# We can seen a fairly strong monotone relationship
# between both rm and lstat with medv.  In the two
# plots of the predictors, rm and lstat, there are monotone 
# relationships (there is somewhat flat interval in in
# one plot).  
# 
# When both variables are candidates for partitioning cases
# at a node, if the values of one the two predictor variables
# have been randomly permuted, the other predictor is more
# likely more like to be selected for the actual
# partitioning. In some sense correlated predictor
# variables can partially mask a predictor variables importance. 
# 
# Assessing variable variable importance provide the motivation
# for alternative formulations.  The cforest() function
# in the R party package is an example.  For this class, the
# recommendation is just use randomForest().      
#        

# 3.2  Progressive removing less important variables
#      to seek a simpler model with nearly the same MSE

## Run

set.seed(137)
Boston3 <- Boston[,c(1,5,6,8,11,13,14)]
boston3Rf <- randomForest(medv~., data=Boston3,
  importance=TRUE, proximity=TRUE )
boston3Rf

# 87.9  MSE

varImpPlot(boston3Rf)


set.seed(137)
Boston4 <- Boston[,c(5,6,8,11,13,14)]
boston4Rf <- randomForest(medv~., data=Boston4,
  importance=TRUE, proximity=TRUE )
boston4Rf
# 86.25
varImpPlot(boston4Rf)


# Still pretty good 
set.seed(137)
Boston5 <- Boston[,c("rm","lstat","dis","nox","medv")]
boston5Rf <- randomForest(medv~., data=Boston5,
  importance=TRUE, proximity=TRUE )
boston5Rf
# 85.77
varImpPlot(boston5Rf)


# Drop in MSE
set.seed(137)
Boston6 <- Boston[,c("rm","lstat","nox","medv")]
boston6Rf <- randomForest(medv~.,data=Boston6,
  importance=TRUE, proximity=TRUE )
boston6Rf
# 84.79
varImpPlot(boston6Rf)

## End 

# Cross validation may suggest the models are overfitting
# a little bit.  
# 
# 4. ISLR: crossvalidation training and test sets  
# 
# While random forest out of bag samples provide
# a basis for assessing error rates, we can still
# take a look at random forest in a training
# and test set context. 

## Run

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
bostonTrain <- 
  randomForest(medv~.,data=Boston,subset=train,
  importance=TRUE)
yhat = predict(bostonTrain,newdata=Boston[-train,])
mse <- mean(Boston$medv[-train]- yhat)^2
mse

boston.test=Boston[-train,"medv"]


## End

# 5. ISLR Model 1: Try all 13 variables for each node 
#    This bags the tree models.  
# 
# 5.1 500 tree model

## Run
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,
  mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plt <- plot(yhat.bag, boston.test,las=1,)+
        abline(0,1)

                
plt
mse <- mean((yhat.bag-boston.test)^2)
mse

den <- mean( (boston.test-mean(boston.test))^2)
pVar <- 100*(1-mse/den)
pVar  

## End

#The mean squared error is higher for the test set
#than for training set.  13.9 versus 11.1
#and the percent of variance explained is smaller
#83.9% versus 86.6% 


# What is the MSE for bagging with only 25 tree2?  
# The default is 500 trees

bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

## End

#This test data has increased from 13.9 to 15.4

#6. Random forest using 6 randomly selected variables at each node

#In contrast to Section 5. above, here at each node Random forests randomly
#selects a subset of variables to use.  This is something like boosting by
#using slow learners that focus locally. In this situation local is in
#terms of the variable considered rather than the common sitution
#of focusing on cases where the residuals are large.  
 

#6.1 A 500 tree model

## Run

set.seed(1)

rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
rf.boston

yhat.rf = predict(rf.boston,newdata=Boston[-train,])
testMse <- mean((yhat.rf-boston.test)^2)
testMse

# percent of variance explained

dentest <- mean(yhat.rf-mean(boston.test))^2
pVartest <- 100*(1-testMse/den)
pVartest  

importance(rf.boston)
windows()
varImpPlot(rf.boston)

## End

# Above we see that mse for the test set has dropped
# to 11.48 

# 6.2  A 25 tree model
# 
# What happens if we use only 25 trees?

## Run 25 trees

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,
  subset=train,mtry=6,importance=TRUE, ntree=25)

# Training result
rf.boston  

# Test set result

yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)


## End

# We can see that with just 25 trees the mse has gotten
# larger for both the training and test set.  

