# Introduction to ggplot

# Due =======================================
#
# Plot from 4. after changing the fill color to cyan
# Plot from 9.
#  after changing the point shape to 16 and the color to red
#  after changing the contour line size (thickness) to 1.5
#  Note that functions arguments must be separated by a comma
# 3 points
# ===============================================

# Old faithful geyser info for the curious
# https://en.wikipedia.org/wiki/Old_Faithful
# https://www.livescience.com/4957-secret-faithful-revealed.html
# https://www.livescience.com/28699-old-faithful-hidden-cavern.html

# Addresses
#   head(), summary()
#   library()
#   The help system
#   ggplot
#     aesthetics: x and y position,
#       shape, size, fill and color
#     geometric objects
#       geom_point, geom_smooth,
#       geom_histogram, geom_line,
#       geom_freqpoly, geom_density.
#     statistical calculations: stat_density2d()
#     superposing geoms
#     adding labels with labs()
#     controlling plot themes such as
#       grid lines
#   Kernel versus histogram density plots
#   A bivariate density contour plot

# Sections
# 0. Setup
# 1. The old faithful geyser data
# 2. The R help system
# 3. Producing a scatterplot
#      Adding a smooth to suggest
#      a functional relationship
# 4. Plotting labels
# 5. Themes that control plotting
# 6. Univariate data density
#    Kernel versus histogram density plots
#    Save plots and modifyiny them

#==================================================

# 0. Setup

# uncomment and run if tidyverse is not yet installed
# install.packages("tidyverse")

library(tidyverse)
source('hw.R') # Instructor provide ggplot theme

library(MASS) # This has the faithful data set

# 1. The old faithful geyser data

# The data set, faithful, is in the MASS package.
# The MASS package comes with R
# so does not need to be installed.
#
# The MASS package still needs to be put
# on R's search vector to provide access
# the data.  The library(MASS)function
# has adds the MASS package "folder" to the
# second position in R's search list.

search()


# When R looks for a specified object,
# such as faithful, it follows the search
# vector order and uses the first object
# found with the specified name.
#
# Objects can be masked.
# We address this in a later class.
#
# R writes newly created objects in position 1,
# .GlobalEnv., so will not overwrite
# package objects.
#
# Entering an object name will invoke
# a default print function for that type
# of object.

faithful

# 1.1  A scrollable view is available

View(faithful)

# 1.2 Quick partial views
#
# To get quick look at part of the data
# we can use the head() and tail()
# functions.

head(faithful)
tail(faithful)
tail(faithful,10) # Shows the last 10 rows

# 1.3 A tibble variation of a data.frame
#     indicates the type variable in each
#     column and limits the number of rows and
#     columns shown. This is helpful for large
#     datasets.

faithful2 <- as_tibble(faithful)
faithful2

# 1.4 We can often get information
#     about R provide datasets
#     by using ? as a prefix.
#     with ?

?faithful

# Here we see the eruption duration
# and the wait time to the next eruption
# are measured in minutes.

# Knowning a variable's units of measure is
# is often important for analysis, interpretation of
# results and communication. Below we will put the
# units of measure in the plots.

# 1.5 We can get descriptive statistics for
#     variables in data.frames, tibbles, and
#     other data structures

summary(faithful)
summary(faithful2)

# 2. The R help system for functions

# A ? followed by function name
# accesses help information.

?summary

?head

# The R function documentation has sections entitled
# Description, Arguments, Detail, Value,
# Authors()and Examples
#
# Examples are often suggestive and their results
# may clarify what the function does.

?seq

# try the first and third examples

seq(0, 1, length.out = 11)
seq(0, 1, by=.2)

# R functions always have matching parentheses.
# The parentheses may contain 0 or more
# arguments. Arguments are separated by
# commas.

# 3. Producing a scatterplot using ggplot

ggplot(faithful2,aes(x=waiting,y=eruptions))+
  geom_point()

# The first argument to ggplot
# must be a data.frame or tibble.
# The second argument, aes(), is
# itself a function.
#
# Function arguments are often written as
# key word and object pairs separate by "=".
# In x=waiting above, x is keyword for encoding
# a variable using the x-axis and waiting
# is the column in faithful tibble to be used
# as that variable.
#
# As you likely assume, y is the keyword for encoding
# a variable using the y-axis and eruptions is the
# tibble variable to be used.
#
# The ggplot function by itself just sets up the plot.
# The + geom_point() function specifies that points
# are to be plotted and causes the plot to be produced.

# 4.  The ggplot() function development
# followed the design and terminology
# described in Leland Wilkinson's illuminating
# book, "The Grammar of Graphics".
#
# In the book, the term aesthetics refers to
# the Greek word that means perception.
# In the syntax above,the aes() function
# pairs aesthetic key words to tibble variables.
#
# The most commonly used aesthetics are
# x and y position, size, shape, fill and color.
# ggplot uses these aesthetics when rendering
# geometric objects that we can see.
#
# Functions with prefix "geom_" specify
# geometric objects.
# Three examples are geom_point(), geom_line(), and
# geom_polygon().
#
# We use geom function arguments
# to change aesthetic constants.

ggplot(faithful2,aes(x=waiting,y=eruptions))+
  geom_point(shape=21, size=3.7,
    fill="green", color="black")

# The x and y aesthetics specified in the ggplot()
# function were passed to the geom_point function.
#
# Notes
# 1) There is no default "geom_" function.
# 2) We can add multiple geom layers to a plot using "+"
# 3) Each geom function can have it own
#    tibble or data.frame,
#    aes() function that pairs aesthetics with
#      tibble or data frame variables, and
#    aesthetic key words paired with constants.

# 5. Adding a smooth of form y = f(x)+ e
#    to suggest a possible functional relationship
#    in the presence of noise, e.
#

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point() +
  geom_smooth(method=loess, color="blue",size=.5)

# ggplot2 supports computing with stat_functions
# and plotting the results. For geom_smooth(),
# stat_smooth() is the default and vice versa.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point() +
  stat_smooth(color="purple",size=.8)

# We can choose other data fitting methods.
# Below lm, specifies a linear regression model.
# Below we also change the geom layer plotting
# order and aesthetic value constants.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_smooth(method=lm, color="red",size=1.1)+
  geom_point()

# Now black points appear on top of the red line.


# 6. Plot labeling

# In this class, graphics are to include variable names
# and the units of measure.  The units of measure may
# appear in variable labels or perhaps in the title.
#
# When the graphics are in the class project report, the
# labeling can appear in figure caption.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2, fill="green", color="black")+
  geom_smooth(method=loess, size=1.2) +
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes",
       title="Old Faithful Geyser Eruptions")

# Plotting space constraints may motive the use of
# shorter labels. We might use abbreviations such
# min. for minutes.  For a long text string another
# option is to use two lines. The text "\n" in a
# character string signals the start a new line of text.
# Note the x-axis label in the example below.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2.,fill="green",color="black")+
  geom_smooth(method=loess,size=1.2, span=.6)+
  labs(x="Waiting Time Between Eruptions\nIn Minutes",
       y="Eruption Duration in Minutes",
       title="Old Faithful Geyser Eruptions")

# If both variables are in the same units,
# the units might be specifed in the title.

# 7. Themes that control plot appearance

# A few themes that come with ggplot2 that control
# plot features such as plot panel fill color and grid
# lines.  This default theme is theme_gray() that provides
# light gray fill and white grid lines.  I have
# long advocated this fill to increase the plot presence
# as an object while background gridlines are
# unobtrusive and support higher perception
# accuracy extraction. I have provided my modification
# of theme_gray() in the hw.R script.  R read this in
# Section 1 above.  This centers the title. It removes
# the superfluous tick marks, recovers their wasted space,
# and repurposes tick labels as in gridline labels.
# The script removes the secondary grid lines,
# and outlines the plot with a gray line dark enough
# to proved more contrast with the background.
#
# The purpose of the contrast is to draw the plot as
# entity into the foreground! Previously the high contrast
# black ticks marks and text pushed the plot into the
# background and the white grid lines running into
# white background glued it to the screen (page).
#
#
# Recently the plot title default location was changed
# in theme_gray to be at the top left of the plot panel
# rather than at the top center.  The English reading
# convention is left to right and then top down.  Those
# with this convention may well look first at the top
# left first. A can be made for this change.
# However I am used to more symmetry in title,
# so I re-centered the title.

# Adding + hw to script below invokes the script.  Use
# of this in homework is expected for now. Specific
# examples later in class will motive theme
# modification.

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2, fill="green", color="black")+
  geom_smooth(method=loess, size=1.2, span=.6) +
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes",
       title="Old Faithful Geyser Eruptions") + hw

# 8. Kernel versus histogram density plots

# In this class the instructor's prefers
# showing kernel density plots to histogram
# density plots. Both have visual artifacts.
# Since many people are familiar with histograms
# they can be useful for communication
# purposes.

ggplot(faithful,aes(x=waiting)) +
  geom_histogram()+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions") + hw

# We can control the binwidth,the
# rectangle fill color and the line color.

ggplot(faithful,aes(x=waiting)) +
  geom_histogram(binwidth=2,
    fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Counts",
       title="Old Faithful Geyser Eruptions")+hw

# Histograms typically encode one of three different
# quantities on the y axis:
# counts, percent of total count, or data density.
# The little used ggplot syntax below specifies
# using density for the y axis. This enables
# comparison with a superposed kernel density estimate.

ggplot(faithful,aes(x=waiting,..density..)) +
  geom_histogram(binwidth=2,
    fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions") + hw

# We can save a graphic object, plot it and access it by
# name, change specification and a plot layers.
# Before for superposing a density
# curve, we superpose scaled frequency polygon
# which is similar to a density plot.  Some statistic
# reasons some consider frequency polygons to be
# better density estimates than histogram estimate.

# Save the plot
histPlot <- ggplot(faithful,aes(x=waiting,..density..))+
  geom_histogram(binwidth=2, fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Density",
       title="Old Faithful Geyser Eruptions")+hw

# Plot it
histPlot

# Add a frequency polygon and plot
histPlot+ geom_freqpoly(binwidth=2,color="red",size=1.2)


# Add a computed kernel density, extend the x axis and plot
histPlot+ geom_line(stat="density",color="blue",size=1.2)+
  xlim(33,104)

# Above we superpose a density curve using geom_line
# and extend the plot limits because the kernel
# density estimates are positive over a larger interval
# than the data minimum and maximum.
# Yes, the extension can be obviously wrong when tails
# go outside the limits of possible values.

# Below, we fill the area under the density curve.
# A alpha argument controls fill transparency
# The alpha argument can have values
# ranging from 0 to 1 with 0 being completely transparent.

histPlot +
  geom_density(adjust=.4,fill="cyan",color="black",alpha=.4)+
  xlim(35,102)


# We still need to settle on kernel smoothing parameter
# It is controlled above by adjust=.4.  Decades ago
# we used our opinion and picked between densities that
# looked too rough and too smooth.  Today, cross validation
# algorithms can often suffice. More knowledge about the data,
# the task and the audience might guide us toward a different
# choices. Knowledge may be indicate there is more real
# structure than suggest by peaks and valley of the sample
# at hand.

# 9. Bivariate Density, a contour example

# With a scatterplot, interest may focus on
# functional relationships of form y = f(x)+ noise
# or the bivariate density of x and y.
#
# The bivariate density script below is a simple example
# from the R Graphics Cook Book Section 2.12.
# This invokes the ggplot2 stat_density2d function
# to compute the bivariate density.  The example
# illustrate the contour approach representing
# bivariate density.
#
# The class will return to the topic of showing bivariate
# densities. We can easily show 3D rendered surfaces
# in rotatable view.

ggplot(faithful,aes(x=waiting,y=eruptions))+
  geom_point()+
  stat_density2d(aes(color=..level..)) +
  labs(x="Waiting Time Between Eruptions in Minutes",
    y="Eruption Duration in Minutes",
    title=paste("Old Faithful Geyser Eruptions:",
      "Bivariate Density"))+ hw


ggplot(faithful,aes(x=waiting,y=eruptions))+ geom_point(shape=16,color="red") + stat_density2d(aes(color=..level.., size=1.5)) +
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes", title=paste("Old Faithful Geyser Eruptions:",
                                                     "Bivariate Density"))+ hw


ggplot(faithful,aes(x=waiting,y=eruptions))+ geom_point(shape=16, size=1.5, color="red") + stat_density2d(aes(color=..level..)) +
  labs(x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes", title=paste("Old Faithful Geyser Eruptions:",
                                                     "Bivariate Density"))+ hw
# Note the grey level and color saturation are poor
# encoded for a continuous variable. This will be
# discussed later in class.

# A reference above indicated there are two major
# underground chambers at different depths.








