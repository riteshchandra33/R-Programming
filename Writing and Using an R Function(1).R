---
title: "Writing and Using an R Function "
Original author: "Daniel Carr"
Changes made for this course
date: "11/5/2017 Revised 2/21/2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### 0. Setup

```{r message=FALSE}
library(tidyverse)
source('hw.R')
```

### 1. Writing a specified R function for a quiz: An example

Consider the task of writing function named myLinear with arguments
named x, a, b, and error that are to appear in the given order. The task
also specifies that:  

- The arguments x and b are not to have default values.   
- The default values for a and error are to be 1 and 0 respectively.  
- The function is to return the values ax + b + error.  
- Appropriate R syntax for the numeric operations must be used.

The myLinear1 function below has the correct arguments. The function
body consists of single complete R expression.  By changing the
function name to myLinear this would suffice to address the
task above.     

The myLinear function further below is also correct.  This illustrates
the use of {} to accomodate a function body that 
consists of a sequence of two or more R script lines.  

The myLinear function shows using the return function.  The
return function argument is typically the name of an object defined
in the function. Often a function, such as the linear model function
lm, produces several objects to return.  In this case the objects to be returned can be put in single list structure that is returned.  

```{r}
 myLinear1 <- function(x,  a = 1, b, error = 0) a * x + b + error 

 myLinear <- function(x, a = 1, b, error = 0)
 { y =  a * x + b + error
   return(y)
 }

```

### 2. Data modes and classes that can be used as the function x argument 

The function above would most likey be used when the
x argument is a vector.  However the numerical operations
are well defined when x is matrix or a higher dimensional
array. Typically the class of the returned result matches
the class of the x argument.  

A vector example
```{r}

set.seed(43)

n <- 5
vec <- rnorm(n, mean = 100, sd = 16)
vec

myLinear(vec, b = 3)

```

A matrix example
```{r}
mat <- matrix(1:6,nrow = 3)
mat

mode(mat)
class(mat)
```
```{r}
myLinear(mat, b = 3)
```
Logical vectors can  be used in numerical operations.
In this context R will convert the logical
values, TRUE and FALSE, to 1 and 0 respectively.  
```{r}

logicVec <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
myLinear(logicVec, b = 3 )
```
Character strings cannot be used in numeric operations 

```{r}

charVec <- c("Sue", "Zijing", "Radhika")
mode(charVec)
class(charVec)


# The line below generates an error
# myLinear(charVec, b = 3 )
```
#### 3. Side note on vector replication

What if the arguments a or b were numeric 
vectors longer than length 1?  
 
The binary operators such as * and + are supported
for pairs of matrices of the same size.  When
we specify a constant for a and b above, the values
are replicated to make a matrix the same size
as the x argument.  R applies the numeric operation
to the corresponding elements of the two matrices.

If arguments a or b are short vectors, the vectors
are replicate enough times fill in a matrix with 
the same number of rows and columns as the x argument.
If there are excess replicated values they are discard,
but a warning message appears.  

Below mat has 3 rows and 2 columns.
The vector c(1,5) has length 2.  
The R script below replicates this vector
3 times to provide the 6 values needed 
in need the matrix.

```{r}
aMat <- matrix(c(1,5),nrow = 3, ncol = 2)
aMat
mat*aMat
```

We get the same result with myLinear
```{r}
myLinear(mat, a = c(1,5), b = 0)
```

#### 4. R pairing of function call and definition arguments.

All arguments in the function definition have names. 
R first pairs named function call arguments with the
function definition argument names.  If there are
unnamed function call arguments they are paired in their
left to right order to the unmatch function definition 
arguments.  The pairing totally ignores function 
definition argument defaults.  


```{r}

# The line below defines the function tst
tst <- function(longName, short) longName + 2*short

# Below the function call to the tst function  
# produces the result, 13. The call argument  
# longName pairs with definition argument longName and
# the call argument short pairs with definition
# argument short/ 

tst(longName = 3, short = 5)

# Here we change the order of the named arguments in
# calling function. The arguments are still paired
# by name.  
tst(short = 5, longName = 3)

# When a function call omits argument names, R pairs the
# unnamed call arguments in left to right order with 
# unpaired function definition arguments in left to right order.

tst(5, 3)  # 5 pairs with longName and 3 pairs with short
```

The first letters of  a calling function argument name will suffice
if they uniquely identify the definition function argument name.

```{r}
tst(5,l = 3)
```
Above, the letter l suffices to identify longName
as the matching function defintion argument.  For clarity it is better
use the full argument names in the calling function.  Since R
Studio provides argument completion, this is easier to do than
in the past. 

####5.  Simulation, the myLinear function, and make a plot

  
```{r}

# We set the seed
set.seed(37)

# We sample 100 random uniform values in [0 50]
# to use as x coordinates.

n  <- 100
simX <- runif(n, min = 0, max = 50)

# We sample 100 values from a normal distribution
# to emulate residual from a model

simError <- rnorm(n, mean = 0, sd = 4)

# We compute the y coordinates use my Linear
simY <- myLinear(x = simX, a = 2, b = 4, error = simError)

simDat <- tibble(simX,simY)

ggplot(simDat,aes(x = simX,y = simY)) + 
  geom_point(shape = 21,size = 3.5, fill = 'cyan',color = "black") +
  geom_smooth(method = 'lm',size = 1.5, col = 'blue') +
  labs(x = "X = Simulated Uniform Values From [0, 50]",
       y = "Y = 2 * X + 4 + N(mean=0, sd=4)",
      title = "Simulated Data") + hw

```