                  Regression: Boston Housing Data

Adapted from ISLR Chapter 3 Lab: Linear Regression

Introduces:
Model specification syntax
Model results
Model diagnostics
Shows enhances scatterplot matrix

Sections:
1.  Simple linear regression
1.1 View data with a data editor
1.2 Fit 1 predictor variable model
1.3 Look at the model summary
1.4 Extract estimates, coefficients,
    predicted values, ...
1.5 Compute confidence intervals
1.6 Make simple plots
1.7 Plot regression diagnostics
   
2.  Look as the data with an
    enhanced scatterplot matrix

3.  Multiple linear regression


0. Setup

source("hw.r")
library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(ggplot2)
library(car)

1.  Simple Linear Regression

1.1 View data with a data editor

The R data editor is primitive but
works pretty well as a scrollable
table.

## Run

View(Boston) 

## End

For this data documentation on 
variables is available along with
references tha provide more
information.

## Run
?Boston
## End 

1.2 Fit a single predictor model

## Run

lm.fit <- lm(medv~lstat,data=Boston)
lm.fit

## End

1.3 Look at the model summary

## Run
summary(lm.fit) 
## End

For the moment assume the model
distribution assumptions are satisfied. 

Is the model significantly better
than a random fit?  What does the
F-statistic indicate? 

Which regression coefficients, 
if any, are significantly
different than 0?   

What fraction of the variability is
explained by the model? 

1.4 Extracting estimates
    coefficients, predicted values, ...

Components of regression results
in R standard list structure for linear
models. We can extract results using
list structure component names but
extractor functions are handy and
some include computations.

## Run  

# component names
names(lm.fit)

coef(lm.fit)
predict(lm.fit)
residuals(lm.fit)

## End

1.5 Compute confidence intervals

Compute confidence intervals for
regression coefficients

## Run

confsint(lm.fit) # 95% is the default
confint(lm.fit,level=.99)

## End

R will also compute confidence intervals
for the mean response at given predictor
values.  

It will also compute confidence intervals
for future observations at given predictor values.
Each new value also includes random error. 
This increase the size of the confidence interval.  

## Run

predict(lm.fit,data.frame(lstat=(c(5,10,15))),
  interval="confidence")    

predict(lm.fit,data.frame(lstat=(c(5,10,15))),
  interval="prediction")

## End

1.6 Some quick base level R graphics

Note that attaching a data.frame makes
its variables accessible without reference
to the data.frame.

## Run

attach(Boston)

windows()
plot(lstat,medv)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")

## End 

1.7 Better graphics

We can add grid lines, predicted confidence
intervals for the fitted line and 
better labels. Adding color provides
some visual appeal.  

Surrounding the sequence with () causes
R to continue reading more lines.  
I prefer the appearance with + on
the left.     

  
## Run

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


## End

1.7 Plot regression diagnostics

#The following puts R's four
standard regression diagnostics
in four panels of one plots

## Run

windows()
par(mfrow=c(2,2))
plot(lm.fit)

## End

The distribution of the residuals is
a function of the fitted value.  The
violates the independent assumption.
The Q-Q plots has really thick right tail
tail.  The distribution is not a normal
distribution.  All the significance tests
are unjustified.  

We can look at studentized residuals that
in the ideal case  should look very close
to the standard normal distribution.  
That is the mean should be around zero
and the standard deviation should be
around 1.   

## Run

windows()
par(mfrow=c(2,2))
plot(predict(lm.fit), residuals(lm.fit),las=1)
plot(predict(lm.fit), rstudent(lm.fit),las=1)
plot(hatvalues(lm.fit),las=1)
y = rstudent(lm.fit)
qqnorm(y,ylab="Studentized Residuals",las=1)
qqline(y)
which.max(hatvalues(lm.fit))

par(mfrow=c(1,1))

## End

2. A better look at the data
   
The following is a splom (a scatterplot
matrix) the house median value for townships
the more important predictor variables 
in descending importance order as 
established by a random forest model.

Here we use the matrix panel organization
with the diagonal going from the top
left to the bottom right. There more obvious
part of the story is likely to be located
in the top left panels of the plot.
   
## Run

varNum(Boston)

Boston2 <- Boston[,c(14,13,6,5,8,1,7,11,10,3,12,9)]

# hexbin modifed to 15 xbins and trans power set to 0.5.  
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
    trans=function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}

onDiag <- function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
 }

windows(width=9,height=9) 
splom(Boston2,as.matrix=TRUE,
  xlab='',main="Boston Housing: Selected Variables",
  pscale=0, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5,
  panel=offDiag,
  diag.panel = onDiag
)

## End

In the top row the dependent variable, median
house price, is the as the
y-axis.  The variables lstat and rm and have
fairly tight bivariate distribution about loess smooth.
Neither smooth looks like a straight line fit. Below
was show the a fifth order polynomial in lstat fits the
data pretty well.  Perhaps fitting more variables will
lead to residuals that plausibly come from a
normal distribution     

3. Multiple Linear Regression

The "~" means is modeled by.  
The "+" syntax lets us specify 
individual predictor variables.

## Run
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
## End


The "." is a convenient notation
mean use all the other variable. 

## Run

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

## End

The variance-inflation factor
indicate have the variance of 
the variables coefficient has
increase the variable is not
linearly independent of the other
predictors.

## Run

vif(lm.fit)
?vif

## End

The "-" supporting omitting
individual variables

## Run

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

## End

An alternative to update a previous
model 

## Run

lm.fit1=update(lm.fit, ~.-age)

## End

The model syntax lstat:age is equivalent
to I(lstate*age), the R product of the
two variables as in lstate*age.  The I()
indicate the contend is not the special
model syntax. 
    
The model syntax lstat*age is equivalent
to lstat+age+lstat:age 

## Run

summary(lm(medv~lstat*age,data=Boston))

## End

3.1 Non-linear Transformations of the Predictors


The following fits a quadratic model

## Run

lm.fit=lm(medv~lstat)
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

## End

Is the quadratic model
a significantly better fit?
We can anova()to compare two 
regression models when one
model just includes additional
variables.

## Run

anova(lm.fit,lm.fit2)

## End

The fit is much better.  The p 
value is very close to zero. Still,
unless there residuals are
consistent with model assumptions
the foundation for making probability
statements is undermined. 

## Run

windows()
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1)

## End


The syntax poly() support fitting
polynomial models and ns() supports
fitting natural splines.  

## Run

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

(ggplot(Boston,aes(x=lstat,y=medv))
 + geom_point(shape=21,fill="red",
     color="black",size=2)
 + stat_smooth(method=lm,formula=y~poly(x,5),
     color="blue",fill="cyan")
 + labs(
     x="Lower Status Percent of Population",
     y="Median House Value ($1000)",
     title="Boston Housing Data") 
 + hw
)

## End


#We can make log and power transformation. This
#doesn't get confused with model syntax.

## Run

summary(lm(medv~log(rm),data=Boston))


lm.fit6 = lm(medv~poly(lstat,2)+poly(rm,2)+ptratio)
summary(lm.fit6)
plot(lm.fit6)
par(mfrow=c(2,2))
plot(lm.fit6)


## End

Now we are up to and R-squared of about 0.78

There are still some serious residual outliers.  


