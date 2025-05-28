File        Regression and smoothing 2D domain.r
Originially designed by Daniel B. Carr

Topic       Regression, smoothing, graphics, and diagnostics
See         Class companion notes             

Data        Abrasion Loss
            For a discussion of the data see the Visualizing
            Data by William Cleveland 1993
		
Sections    1.  Read the abrasion loss data
            2.  Exploratory 1-D distribution views of variables
            3.  Scatterplot matrices with smooths
                and linked point selection

            4.  High leverage points linear regression example
            4.1 Discussion about high leverage points
            4.2 Computing the hat matrix H using matrices
            5.  An introduction to linear models
                and basic output
            6.  Regression diagnostics 
            
            7.  A loess smooth model and predicted surface views 
            8.  Conditioned plots for surface examination
            9.  Conditioning for more than three variables:
                discussion  
  
Omitted     1.  Partial residual plot this time.  
            2.  Other diagnostics plots
   
0. Setup

## Run

source("classEDA.r")
source("classDensity.r")
library(lattice)
library(car)
library(rgl)
library(ade4)
library(sp)
library(ggplot2)
source("fixlights.r")

## End

1.  Read the abrasion loss data

##Run       

abrLossDat<- read.table('abrasionLoss.txt') 
colnames(abrLossDat)=c('hardness','tenStrength','abrLoss') 
abrLossDat   # look at the values

##End___________________________

There are 30 rubber specimens indexed by the
26 letters of the alphabet and the number 1-4.   

As indicated in Cleveland's book Visualizing Data,

Tensile strength:  The force per unit of cross-sectional
area required to break a specimen.  The units are
kilograms per centimeter squared, kg/cm**2

Hardness:  The bound height of a steel indenter dropped
onto a specimen.  The units are degrees Shore

Abrasion loss:  the amount of material abraded
from a specimen per unit of energy expended in the
rubbing.  The units are grams per horsepower hour,
gr/hp-hour.  

## Run: look univariate summary statisitics

summary(abrLossDat)

## End__________________________

2. Exploratory 1-D distribution views of variables

##Run ##

lab1 <- c('Hardness','Tensile Strength','Abrasion Loss')
units <-c('degrees Shore','kg/cm**2','gr/hp-hour')
 
for (i in 1:ncol(abrLossDat)){
  windows()
  classEda(abrLossDat[,i],lab1=lab1[i],units=units[i])
}

##End_______________________

The windows, one per variable, are overplotted!
Move the ones on top to see the ones below.

In the Q-Q plots note the suggestion of
Abrasion loss:    thick right tail
Hardness:         thin tails 
Tensile strength: thin tails

3. Scatterplot matrices with smooths
   and linked point selection 

## Run ##

splom(abrLossDat, type=c("g","p","smooth"),
  varnames=c("Hardness\ndegrees shore",
             "Tensile Strength\nkg/cm**2",
             "Abrasion Loss\ng/hp-hour"),
  xlab='',main="Rubber Abrasion Loss",
  pscale=4, varname.cex=0.8,axis.text.cex=0.6,
  axis.text.col="purple",axis.text.font=2,
  axis.line.tck=.5, pch=21,fill="green",
  col="black",lwd=3)

## End ##

In the top-row left-column panel with 
x = Hardness and y = Abrasion Loss.  We see close to a linear 
relationship with negative slope between hardness
and abrasion loss. The harder the rubber the
less the abrasion loss.  

In the 1st-row and 2nd-column panel with
x = Tensile Strength and y = Abrasion Loss
there is bump centered around 160 degrees shore.

Mentally assessing what will happen when
fitting both hardness and tensile strength
based on the two plots is not so easy. 

Below we select cases to show in a different color
by clicking on points. This is a primitive form
of linked brushing. The user interface for
linked brushing lets us more quickly select
of cases to distinguish with color or to mask. 

Two classes of useful alternate views include conditioned
plots based on partitioning subsets of cases into
separate panels and 3-D scatterplots that can be
enhanced in many ways.

## Run

trellis.focus("panel",1,1,highlight=FALSE)
panel.link.splom(pch=21,fill="red",col="black")
trellis.unfocus()

## End

First notice matrix stucture of the panels.
The 2nd-row and 1st-column scatterplot shows
the 2D predictor domain with x = hardness and
y = tensile strength. Transposing the matrix 
of panels about the line from the lower left to
upper right and puts the 2D domain panel in the
3rd-row and and 2nd-column. The transposed panel
has x = tensile strength and y = hardness.  


Focus again on the scatterplot in the
2nd-row and 1st-column. Note the two points of
the left with the smallest hardness values.
In the panel with x = hardness and y = abrasion loss
we see thesetwo points have the highest abrasion loss.
Also observe that in hte  2D domain plot two points
have nearly the same tensile strength. The vertical
rectangle from the bottom of the panel to the top
of the panel that contains these two points is
otherwise empty. We have don't data showing how
the variation in tensile strength will change
the amount of abrasion loss in this part of the
domain.   

A good analyst will assess a model domain
and note where there is data to support model and where
there is little or no data for some of the variables.
The absence of data limits the domain sound model
prediction and the form of the model may need to
change when data becomes for sparse or empty regions
of the domain.    
   
This said, note the three highest hardnesss points
on the lower right of the same plot.  Envision the
horizonal and verticle rectangles containing these
three points. The horizontal rectangle is empty
and the vertical rectangle has only one point. 
Its tensile strength is pretty high. For the 
smoothing curve in the this domain panel the three
points win tug of war against the one point.    
 
In terms of points present the high and low
hardness points will have a high leverage in
determein the coefficient for hardness in the
regression model. The three points with high
hardness also have low tensile strength. The
the three points will have high leverage in
determine the regression coefficient for 
tensile strength. 
  
There is more whites space (regions without data points)
Later we plot convex a hull of the points
There is white space inside the convex hull.  
This is not to be ignored but in the absence
of scientific knowledge about the data that say
otherwise, experienced analysts are typically are
more concerned about predictions at
locations outside the convex hull of domain value.

Next we interact with the scatterplot matrix to
learn more. 

In the top left panel left click on the highest
five abrasion loss points. Then right click in the plot
and select stop from the menu that appears.  
Note the track of red dots in the domain panel below. 
Thee tend to have low tensile strength.  

Now run the script below and in the top left panel
select the five lowest abrasion loss points. 

## Run
trellis.focus("panel",1,1,highlight=FALSE)
panel.link.splom(pch=22,fill="purple",col="black")
trellis.unfocus()

## End    

Four of the five points have above average tensile
strength in the plot below but one has 
the very lowest tensile strength.  Might this 
anomaly be a bad data point?  

Right click in the plot and select stop
from the menu. 
   
In the regression domain plot we see that in 
contrast to the five red points four of five purple
low abrasion loss points have middle to high tensile
strength. 

Selection high and low values point of the
dependent variable, abrasion loss in the example,
is on systematic was to look as slices the data.
The relative to a methods called sliced inverse
regression that can be quite informative. It 
can be used look at slices based on the predictor 
variables.  However we attention cases with
extreme predictor variable values.     

4. Point leverage in linear regression

The constructed 2D example is provide an extreme
example of domain point leverage.
There 50 points on a straight line yet just two
anomalous points dominate is the linear regression model.
These two points have very high leverage.    
 
##Run

set.seed(97)
x <- rnorm(50)
y <- 2*x+3  # A straight line fit. 

# Add high leverage points
# and let y = -2x+3 for them 

x <- c(x,-11,11)
y <- c(y,25,-19)
df <- data.frame(x=x,y=y)

# lm() is a linear model function
# ~ read as: is modeled by 
#   y is modeled by x
#   df is data.frame with
#   column label x and y  

model <- lm(y ~ x, data = df)
summary(model)

# model coefficients 

round( model$coef,2)
#  Round
#  (Intercept)    x 
#    2.98      -1.31 

windows()
plot(x, y,las=1,
  main="Linear regression with two bad high leverage points")
points(x[51:52],y[51:52],pch=21,col="red",bg="red")
abline(model$coef)

##End___________________________

    
Some points in the domain of predictor variables
may have much more leverage on regression coefficients
and predicted values than other points in the domain.
For linear models such as y = ax + b + e  or z = ax + by + e 
We can easily spot the high leverage points in a plot. 
They have extreme values.  

When the predictor domain has 1 to 3 variables scatterplots
can help us spot points far from the body of data that
are likely to have high leverage.  However there are often
more variables in regression models.  Also the form of the
linear regression models may vary.  That is some
predictors may be transformed.  For example x and x-squared may
may be in the model. There may be interaction terms.  For
if x and y are predictors the model may contain ax + by + cxy
where a, b, and c are to be estimated.  

Fortunately the influence of a point is readily computed
using the model matrix that is described regression courses
and in the class text Introduction to Statistical Learning.  
interactions between predictor terms expressed
in the model. The numericl measures of point leverage
often called hat values. These are the diagonal elements
of the hat matrix briefly discussed in class. We will use
a R function to calculate the hat values. For those familiar
with matrix calculation Section 4.2 shows matrix calculation
as applied to our simple extreme leverage example above.  
  
Here are fa ew facts about hat values. The sum of case
hat values is p,the number of columns in the regression
design matrix.  The  current the matrix has two columns.  
The first column is all 1s to fit the intercept and the
second has x values used in determining the slope.

Hat values are non-negative and there one for
for each of the n cases.  An average hat value
is p/n.  Values bigger than 2p/n are considered to 
have high leverage. 

Using a saved linear regression model object, the
fortify() function the ggplot2 package will
compute several regression diagnostic statistics
for each case.  Among these are hat values,
fitted values, residuals, and standardized residuals 

## Run

modelDiagnos <- fortify(model)
head(modelDiagnos) 

## End

The script below shows identifying cases (data.frame
rows that have high hat values. This script will
also work for models with more variables.   

## Run: Case identification

threshold <- 2*model$rank/length(model$res)
threshold
which(modelDiagnos$.hsat> threshold)

## End

For a domain with one or two variables we can make plot to 
x (or x and y) values and the hat values. Below he
The blue line is a threshold for large leverage points. 

## Run

ggplot(modelDiagnos,aes(x=x,y=.hat))+
geom_point(col="red")+
geom_hline(y=2*model$rank/length(y),col="blue")+
labs(y="Hat Values",title="Influence of Domain Points")

## End

4.1 Discussion about high leverage points

There can be errors in values of predictor variables as
well as the values of the response variable. In some
circumstances there is knowledge about th equality
of individual observations. When critical model-based
decisions are being made, it makes sense to check
on the quality of the high leverage points, high influence
points and extreme residual points. 

Note that a bad high leverage point may have moderate
or low residual because it distorts the model. In some
circumstances a proactive step can be to acquire more data in advance.           
 
There same methods for address the situation with there are errors
in the predictor variable. The modeling literature addresses
this under various labels such as the error in variables problem.

4.2  Computing the Hat matrix, H, using matrices

This is just for the curious.  This section implements the
the mathematical calculations shown in class and provided
in lecture notes. This uses a matrix inverse. In practice
most software (including R) uses other well established
calculation methods that are more numerically accurate than
using the inverse.  

The data.frame df is from above.   

## Run

X <- model.matrix(~x,df)
head(X)

XT <- t(X)
XTX <- XT %*% X
XTXinverse = solve(XTX)
H= X %*% XTXinverse %*% XT
diag(H) # The last two point 

## End

5. An introduction to linear models and basic output_______

In the linear model function lm() below
   data = indicates a data.frame with variables
	that can specified by name
   AbrLoss appear to the left of ~ which is read
      "is modeled by".
   AbrLoss is the response (or dependent) variable
      being modeled.
   The "." indicates that all the remaining variables
      in the data.frame are to be used as predictor
      (explanatory) variables.	
   By default the model also fits the grand mean.

## Run

# Save the modeling results in an 
abrFit <- lm(abrLoss ~ . ,data=abrLossDat) 

# Look at a summary of the fit
summary(abrFit)

## End______________________________

Abrasion Loss Model Summary ___________________

Call:
lm(formula = abrLoss ~ ., data = abrLossDat)

Residuals:
    Min      1Q  Median      3Q     Max 
-79.385 -14.608   3.816  19.755  65.981 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  885.1611    61.7516  14.334 3.84e-14 ***
hardness      -6.5708     0.5832 -11.267 1.03e-11 ***
tenStrength   -1.3743     0.1943  -7.073 1.32e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 36.49 on 27 degrees of freedom
Multiple R-Squared: 0.8402,     Adjusted R-squared: 0.8284 
F-statistic:    71 on 2 and 27 DF,  p-value: 1.767e-11

End Model Summary___________________________

Comment on the Model Summary 

Estimate column
   The fitted linear model is 
   abrLoss = 855 -6.57 * hardness - 1.37 * tenStrength

Std. error column
   Estimate standard error

t value
   Ratio of estimate to standard error
   Similar to a z-score with a theoretical mean of zero
   when the error degrees of freedom is 30 or large

The Pr(>|t|) column 
    indicate categories of statistical significance levels
    using a two-sided t test.
    All three coefficients have p < .001

Residual standard error: 36.49 on 27 degrees of freedom
    The residual standard error is 36.49. Perhaps
    fitting other deterministic variables or stochastic
    components could drive this down.      

Multiple R-Squared: 0.8402 
    The Multiple R-Squared is ratio of the model predicted
    value sum of squares about the grand mean to the data sum
    of squares about the grand men.   

    This can be computed from the residual sum of squares (RSS)
    from the two models.  It is 
    1 - full_model_RSS/mean_only_RSS 

    Modeling in the physical science often produces
    R-Squared values bigger than .5 (or 50% if converted to a percent)
    R-Squared values in the social sciences are often much less than .5.
	
    A high R-squared value simply indicates a reduction in
    sum of squares of residuals.  It indicates neither
    causality, nor the applicability of the model
    to new data.  One can often increase the R-squared
    value by generating several random variables in 
    including them in the modeling process.
    For this reason an evaluation criteria needs to include
    a penalty for the number linear predictors used in
    the model.     

F-statistic: 71 on 2 and 27 degrees of freedom,
    the p-value is 1.767e-011 
    
    The small p value for F statistics says the model
    is fitting the data a lot better than just the grand mean.

     This suggests that the model reduction in sum of squares
     is very unlikely to have happen at random which would be
     be the case if the predictor variables totally unrelated
     to abrasion loss.
   
Correlation of Coefficients:
            (Intercept) Hardness 
   hardness -0.8335             
tenStrength -0.7664      0.2992 

	The regression coefficients are almost always correlated.
	Here Hardness and Ten.strength have a correlation
      of .2992

6. Regression diagnostics  
   
To old but good references are 

R. Dennis Cook and Sanford Weisberg
  "Regression Diagnostics With Dynamic Graphics",
   Technometrics 31(3) 1989

   "Residuals and Influence in Regression", 
    Chapman Hall 1982

6.1 Built in diagnostic plots  

Today the R base functions for linear regression
include four built in diagnostic plots:

(1)  Residuals versus fitted values
     Candidate outliers are labeled 
(2)  Standardized residual normal Q-Q plot
     Standardization divides residuals by their
        estimated variance
     Asymmetry and thick tails are of concern 
(3)  Scale location plot using 
     y = sqrt(abs(standardized residuals))
     x = fitted values    
(4)  Standardized residuals versus leverage
     High leverage points strongly influence the fit
     If points have bad y values the residuals
         still may be small.
     (Sometimes it is a bad x values the produce
          high leverage.) 
     Cook's distance appears in red in some plots.
     We focus on other topics in this class.


##Run   !!! Click in the active plot to move to the next plot
windows()  
plot(abrFit)
## 

6.2 Availability of more regression diagnostics ________________________  

A linear regression class many include several
more diagnostics plots.

As indicated above we can obtain some statistics used
in regression diagnostics using the saved model
as input to the fortify() function. 

The influence.measures R functions provide a few more
diagnostics. The abrInfuence list below
as cases by diagnostics matrix and parallel
matrix of TRUE or FALSE where TRUE indicate
a typical diagnostic values.  

## Run   
abrInfluence <- influence.measures(abrFit)
infmat <-abrInfluence$infmat
head(infmat)
influential <- abrInfluence$is.inf
head(influential)

abrStRes <- rstudent(abrFit)  #studentized residuals	
## End

As discussed in briefly in class, linear regression residuals
have different standard errors and are slightly correlated.  
The standardized and studentized residuals are computed
by dividing the residuals by slightly different estimates
of their standard errors. Either can be used Q-Q normal plots.   
In terms of theory the studentized residual should be
closer to t-statistics and with a large degrees of freedom
follow a normal distribution when the model is correct. 

Linear regression has to compete with growing 
variety of modeling methods. Occasionally there
are data sets for which linear regression provides the
best model. This is true for almost all other modeling
methods. Modelers have described this situation by saying
there is no free lunch.  We can't pick a favorite
modeling approach and expect it to be the best for all the
suitable data sets we encounter.     

Here we skip over many diagnostics so there is time
for other topics.  

We will look at a 3D leverage plot and 3D surface plot
for a regular grid of values.  Then we move on to
another model for the same data, a locally quadratic
loess model.   This will serve to illustrate problems
of estimating (or extrapolating) at locations that
are not so far from observed points in the data set domain. 

6.3 A 3D look at a the leverage of hardness and tensile
    strength points using rgl.

When the plot appears you can 
1) Left click on a corner of the plot and drag to enlarge it.
2) Left click in the plot and drag to rotate the plot.
   Motion parallax provide a 3D depth effect. 
3) Right click in the plot and drag down to zoom and
   up to zoom out.


## Run
interleave=function(x,y)as.vector(rbind(x,y))

ABL <- fortify(abrFit)
x <- rep(ABL$hardness,each=2)
y <- rep(ABL$tenStrength,each=2)
z <- interleave(rep(0,length=length(x)),
       ABL$.hat)
mat <- cbind(x,y,z) 

open3d(FOV=0,windowRect=c(0,0,800,800))
fixLights()
plot3d(mat,type="s",radius=c(.8,1.8),
        col=c(rgb(0,0,0),rgb(1,0,0)),
  axes=FALSE,aspect=TRUE,
  xlab="",ylab="",zlab="")
bbox3d(color="grey50",
         emission="grey50",
         xlen=0,ylen=0,zlen=0)
rgl.material(color="black")
segments3d(mat,col=gray(.5),size=3)
axes3d(edge=c("x--","y+-","z--"),
  ntick=5,cex=.75)
mtext3d("Hardness",   edge="x--",line=2)
mtext3d("TensileStr.",edge="y+-",line=3)
mtext3d("Leverage",   edge="z--",line=3)

## End

6.4 Find the convex hull for predictor values,
    plot it in 2D and add it to the 3D plot
    above. 

Below the chull() function identifies bivariate points
on a convex hull via row subscripts.  

## Run

domain = cbind(abrLossDat$hardness,abrLossDat$tenStrength)
chSubs = chull(domain)
chSubs  # Rows where points are on the convex hull.

windows() 
plot(domain,xlab="Hardness",ylab="TenStrength",
  main="Convex Hull of Data Points",
  pch=19,col="blue",las=1)
  polygon(domain[chSubs,],density=0,border="red",lwd=2)

## End

Now add this polygon to base of the 3D plot

# Run
subs<- c(chSubs,chSubs[1])
n <-length(subs)
lines3d(cbind(domain[subs,],rep(0,n)),size=2,col="blue")
 
## End

Note that high leverage points tend to be on 
the convex hull and the low leverage points tend to 
be near the center of the convex hull.

6.5 Construct a 2D grid of predictor values to use
    in 3D graphics and use linear model to predict 
    values

To produce conventional graphics for the surfaces such a
contours, wireframes and rendered surfaces we generate a
regular grid of (x, y) points.  Then we use our model
to estimate the corresponding z values and proceed
with graphics.
 
Regular grid construction is typically based on the
maxima and minima of explanatory values. As shown below
the seq()function makes it easy to generate equally spaced
between the minimum and maximum hardness, and between the
minimum and maximum tensile strength. The choice below
creates a vector of length 30 for both variables.  
The two lengths do not have to be the same. Rectanglar grids
are common. 

After putting these two vector in the list called gridMargins,
we use expand.grid(gridMargin) to created the product set
of x and y values and returns this in a two column data.frame.  
(If we had three vectors in the list we could generate
a 3-way product set and so on.)     

When producing the product set, the expand.grid function
varies elements of the first vector in the list fastest,
the elements of the 2nd vectot in the list second fastest
and so on until are more vectors in the list.  

## Run: generate 2D grid for the explanatory variable domain 

hardness <- abrLossDat$hardness
tenStrength <- abrLossDat$tenStrength

gridMargins <- list(
  hardness=seq(min(hardness), max(hardness),length=30),
  tenStrength=seq(min(tenStrength), max(tenStrength),length=30))

grid <- expand.grid(gridMargins)
head(grid)

# obtain predicted values at grid locations

gridZ <- predict(abrFit,grid)

## End

6.6 Produce a 3D plot of observed and fitted values,
    connect them with line segments and add a wireframe
    view of the predicted surface.  

## Run

# Copy the data and add fitted data values
abrR <- abrLossDat
abrR$fit <- fitted(abrFit)

# plot data point in blue, fitted data points in red
open3d(FOV=0)
fixLights()
plot3d(abrR[,-4],type="s",radius=3,col=rgb(0,.2,1),
       aspect=TRUE) # observed points in red
spheres3d(abrR[,-3],radius=3,col="red")

## End

With segments3d we can draw line segments between observed and
predicted values by leaving as consecutive 3c points
in a 3 column matrix.  

Row 1 1st data point 
Row 2 It predict point
Row 3 2nd data point                               
Row 4 Its predicted point   
...

## Run

interleave <- function(x,y)as.vector(rbind(x,y))
x = interleave(abrR[,1], abrR[,1])
y = interleave(abrR[,2], abrR[,2])
z = interleave(abrR[,3], abrR[,4])
ObsFit <- cbind(x,y,z)
segments3d(ObsFit,size=2,color="black")
surface3d(gridMargins$hardness,
          gridMargins$tenStrength,
          gridZ,col=gray(.8),size=.2,
          front='lines',back='lines')


range(ObsFit)
range(gridZ)

## End

The plot has gotten taller to accommodate the
prediction surface.  The surface predictions higher
values and 9 lower values that are negative.
A negative abrasion loss does not make sense. 
Abrasion does not add rubber!  Predicting outside
bounds convex hull the data may not make sense.   

## Run

range(gridZ)

## End

7. A loess smooth model, grid construction,
   predicted values and surface views 

One common measure of model fit is the mean squared
error.  

## Run
mean(abrFit$res^2)

## End

We can fit other models to seek a better fit.  At the
same time overfitting is a danger.  We don't want to 
model random variation and flaws in the data.  We will soon
address overfitting by cross-validation.  

Here we will fit a locally quadratic polynomial to
the abrasion loss data obtain estimates z = f(x, y) + e.
to see if we can obtain a better fit.  

(Since surface is roughly monotone we might do better
using a locally linear model, but in the current this 
will also predicting outside the convex hull 
of the data.)
 
The script below saves the model in an object named
abrSmooth. The regression object again includes fitted values
and residuals based on the data. We can check on the
mean squared error.  

We will again use rgl to produce a 3D rendered plot
of observed and predict values and predicted values
for a grid. Then we will replace some of the grid
predicted values with more reasonable values so we can
focus attention of the surfacefor the convex hull of
domain points.  

## Run
abrSmooth <- loess(abrLoss~hardness*tenStrength,
  degree=2, span=.5, data=abrLossDat)

names(abrSmooth)

mean(abrFit$res^2)  # MSE linear model
mean(abrSmooth$res^2)  #MSE

## End

The fit is better as could be expected with local
modeling.  

7.1 Plot observed and fitted values in 3D

# Copy the data

abrS<- abrLossDat

# add fitted values
abrS$fit <- fitted(abrSmooth)
head(abrS)

# plot data points in blue, fitted data points in red
open3d(FOV=0)
fixLights()
plot3d(abrS[,-4],type="s",radius=3,col=rgb(0,.2,1),
       aspect=TRUE) # observed points in red
spheres3d(abrS[,-3],radius=3,col="red")

## End

7.2 Connect the observed and fitted values with lines
    in the 3D view

## Run: This used the letter S to stand
#  for surface

x <- interleave(abrS[,1],abrS[,1])
y <- interleave(abrS[,2],abrS[,2])
z <- interleave(abrS[,3],abrS[,4])
obsFitS <- cbind(x,y,z)
head(obsFitS)

gridS = predict(abrSmooth,grid)

open3d(FOV=0)
fixLights()
plot3d(obsFitS,type='s',radius=c(2.5,2.5),col=c(rgb(0,.2,1),'red'),
       aspect=TRUE)
segments3d(obsFitS,lwd=2,col="black")

## End

Visually verify the bounding box is a cube.

7.4 Predict z values for the grid and add the surface grid lines to the plot

See what happens to the abrasion loss scale.

## Run

gridS = predict(abrSmooth,grid)
	
open3d(FOV=0)
fixLights()
plot3d(obsFitS,type='s',radius=c(2.5,2.5),col=c(rgb(0,.2,1),'red'),
       aspect=TRUE)
segments3d(obsFitS,lwd=3,col="black")
surface3d(gridMargins$hardness,
  gridMargins$tenStrength,
  gridS, col=gray(.7),front="lines",back="lines")

range(obsFitS)
range(gridS)

## End

The better fitting model for the observed data does
not do so well for extrapolation at grid points outside
the convex hull of the data plots. It has higher values
for grid points with low hardnessand low tensile Strength
and even more extreme negative abrasion losses
(rubber additions don't make sense).

Note the loess has reputation for not doing so well on the edges
of the data domain. Smoothing splines will likely to be better.  

This time we modify the gridS values and limit them to the
extreme observed and predicted values on or within
the convex hull of the data.  We can say we are Winsorizing
the values when we replace extreme values by more moderate
value to provide better focus on the body of the data.
Often the Winsorized value are flagged in some way to signal
that they have be modified.  Here the flat grid provide
the signal and color shows the convex hull region.

7.6  Use a point in polygon function to fine
     grid points in the convex hull
       

Below status==0 mean points are outside the hull   

## Run 

status <- point.in.polygon(
   grid$hardness,grid$tenStrength,
   domain[chSubs,1],domain[chSubs,2])
color <- ifelse(status ==0,rgb(.5,.25,1),rgb(.6,.6,.6))
 
plot(grid$hardness,grid$tenStrength,las=1,
  xlab="Hardness",ylab="TenStrength",
  pch=21,bg=color,col=color,cex=.8,
  main="Grid points: Purple points outside the Convex Hull")
  polygon(domain[chSubs,],density=0,col=,lwd=2)

## End 

7.7 Winsorize the extreme predicted surface value that are
    outside convex hull or negative and add surface to
    observed and fitted values  

## Run

chullVals <- gridS[status!=0]
range(chullVals)    # convex hull grid fitted
range(obsFitS) # data observed and fitted values
zlims <- range(obsFitS,chullVals)
zlims[1] <- 0  # Negative values are not acceptable 
range(gridS)   # surface fitted values 

gridSWin = gridS       # make a copy

high=gridSWin > zlims[2]      # Logical vector
gridSWin[high] <- zlims[2]     # Replace too big valeus

low =gridSWin < zlims[1]      # Logical vector
gridSWin[low] <- zlims[1]      # Replace too low values

open3d(FOV=0)
fixLights() 
plot3d(obsFitS,type='s',radius=c(2.5,2.5),col=c(rgb(0,.2,1),'red'),
  aspect=TRUE)
segments3d(obsFitS,lwd=3,col="black")
surface3d(gridMargins$hardness,
          gridMargins$tenStrength,
          gridSWin,col=ifelse(status ==0,
          rgb(.5,.25,1),rgb(.6,.6,.6)),alpha=.9)
## End___________________________ 

Setting the alpha for the surface as .9 provides a bit of
transparency that helps use see observed and predicted point 
on the other size of the surface.  Below we address this using
grid lines

7.8  A grid lines (wireframe) view

The R Graphics Cookbook show has a surface example with
front and back arguments that can be set to lines.
to show  the grid lines. If we omit back="lines" 
and look at the top we see grid
lines and can see points between below the surface.
When we rotate the plot to look at the back of the
surface we will see the surface and only points
between us and the back of the surface.

## Run

open3d(FOV=0)
fixLights() 
plot3d(obsFitS,type='s',radius=c(2.5,2.5),col=c(rgb(0,.2,1),'red'),
  aspect=TRUE)
segments3d(residLines,lwd=3,col="black")
surface3d(gridMargins$hardness,
     gridMargins$tenStrength,
     gridSWin,col=ifelse(status ==0,
     rgb(.5,.25,1),rgb(.6,.6,.6)),front="lines",back="lines")

## End

8. Conditioned plots for surface examination

Conditioning typically partitions data into disjoint subsets
based on the values of categorical variables or intervals
of continuous variables.  This enables three tasks

1) Focus attention on the individual subsets.
   The assessment can include models or residuals
   as well as distributions.  

2) Provides the basis for organized
   comparison across subsets.

3) Restricts variation due to
   the conditioning variables.

Cleveland used conditioning to study for models such as
z = f(x,y) + e using familiar 2D graphics!  

While common practice is to use disjoint intervals
the cover the range each variable, he notes that the
conditioning intervals can overlap and calls such
overlapping intervals shingles in analogy with shingles
on a roof.  This is especially advantageous when
using loess smoothes which are less reliable at the
edges of the intervals.  With overlapping shingles
we can discount smooth estimates at interval
edges and still have good coverage of the whole domain
except for the two edges near the domain endpoints. 

The co.intervals() function below define intervals
for continuous variables. We can specify the
amount of overlap.  

## Run

windows()
hardnessIntervals <- co.intervals(hardness,number=6,overlap=.75)
tenStrIntervals <- co.intervals(tenStrength,number=6,overlap=.75)

coplot(abrLoss~hardness | tenStrength, data=abrLossDat, 
	given.values=tenStrIntervals,row=2,
	panel=function(x,y,...,degree=1) 
      panel.smooth(x,y,span=.75,lwd=2,...))

## End____________________________________

The top panel shows the conditioning intervals for tensile strength.

The three lowest bars (position and value wise) give
the conditioning intervals for bottom row of panels
in the matrix of panels below.  

The increasing intervals for bottom to top
correspond to the matrix of scatterplot panels
starting at the bottom, reading left to right
and then looking at row of panels above this.

At a first glance conditioned smooths are pretty
linear with small amount of curvature and bends. 

To appreciate shift of the line in the y direction
from panel to panel pick a grid location such as
x=60, y=250 in each panel to compare the vertical
distances to the red lines. 

In the bottom left panel the line is about 50 above the
point. In the bottom middle panel it is maybe 35 above. 
In the bottom right, the red line is maybe 5 below and so on
left to right in the next row up.  (Addition could of
reference value would help of observe this pattern quickly
instead of tediously.    

The lasts three points in the lower left plot suggest a change
in slope.  

Note that these conditioned plot shows data points and smoothed
points so suggests where smooth residuals are large,
at least conditionally.

## Run

windows()
coplot(abrLoss~tenStrength | hardness, data=abrLossDat,
	given.values=hardnessIntervals,row=2,
	panel=function(x,y,...,degree=1) 
      panel.smooth(x,y,span=.75,lwd=2,...))
## End________________

The smoothes against tensile strength given hardness intervals
show hockey stick like smoothes. There appears to be a place
where there is a change in slope.  Cleveland page 200 suggests
around  180 kg/cm**2. Just by eye I would guess 200.  

We can turn tensile strength into two variables to estimate
the two slope and possibly get a better fit.   

## Run

y <- abrLossDat$tenStrength
tenLow <- ifelse(y<200,y,0)
tenHigh<- ifelse(y>=200,y,0)

abrAug <-abrLossDat
abrAug$tenLow <- tenLow
abrAug$tehHigh <-tenHigh

abrFit2 <- lm(abrLoss~hardness+tenLow+tenHigh,
               data=abrAug)
summary(abrFit2)

mean(abrFit$res**2)

MSE <- mean(abrFit2$res**2)
MSE

##  End

The two new variables are statistically significant
but don't reduce the mean square error very much.

At the end of the day three highest harness points
have high leverage seem incompatible with a simple

9. Conditioning for more than three variables: discussion

While conditioning for 3 variable applications can be
helpful as Cleveland illustrates, the available 3-D graphs
can do pretty well.  

In Chapter 5 Cleveland addresses hypervariate
data which means four or more variables.  He has good graphics
to show the conditioning intervals along with a matrix of panels.
show 2D scatterplots with smoothes. For example see page 277.     

In my opinion conditioning is more important when there
are four or more variables.  The comments below
address the situation of continuous variables but
note that approach is also applicable to the
levels of categorical variables.  

Using a one or two-way grid of panels we can highlight
data and model fits y=f(x) or z=f(x,y) for cases with
values of one to two condition variables falling
in intervals associated with the grids.
    
The multiple panel views provided by lattice conditioning
and ggplot2 faceting are designed to address such situations.
Lattice can handle layouts for additional conditioning
variables  the collection of panels may span many printed
pages. I think the cognitive burden when looking for
patterns grows rapid as the number of panels increases
beyond 9.    

Incorporating more variables and our visual analysis broadens
context scope of our thinking.  In principle this is good
when the addition variables help the data tell a more
detail and accurate story.     

There are two major issues.
1) the resolution lost by effectively treating the
   value of the conditioning variables in a 
   conditioning interval as being equivalent.
2) the difficulty we have in obtaining a gestalt
   view and understanding from the collection of panel
   views.

That is, we may miss an important part of the story
because we may not be able to fully comprehend the
story.  Even if would through practice and know
of the phenomena many to see parts we still
may still have a difficult to communication
a complex story. We and our clients have limits.
As same time is we restrict ourselves to two
variables we can deal pretty well with four variables
impoverishes our thinking.   

Note that the CCmaps design uses conditioning to address
five "variables": two geospatial coordinates and
three attributes.  The two threshold sliders for each
of the non-spatial variables which yield three
intervals for each variable.  These partitions the cases
such as states into cells of a 3 x 3 x 3 array.
CCmaps use color to indicate the levels (or classes) of
one variable so it suffices to use a 3 x 3 set of map
panels and show the states in their respective colors. 
 
