# Mothers at Risk from Childbirth
#
# The old Washington Post article
# motivated the graph redesign.
# The article cited
# the Population Reference Bureau
#
# The bureau has 2014 data on
# a variety of topics:
# See http://www.prb.org/

# Due=========================
#
# The plot in 4 after
# adding Afganistan to the list of labeled countries
#
# 1 point
#=================================

# 0. Setup

library(tidyverse)

source('hw.R')
source('hwLeft.R')
source('hwRight.R')
library(gridExtra)
# library(plotly)


# 1. Enter data as vectors

attend <- c(25,8,12,31,2,17,15,30,8,8,6,16,24,26,24,15,47,38,44,20,31)
deaths <- c(18,17,16,16,16,15,15,15,15,14,14,14,13,13,12,12,12,12,11,10,10)

nams <- c('Sierra Leone','Afghanistan','Bhutan',
'Guinea',
'Somalia',
'Angola',
'Chad',
'Mozambique',
'Nepal',
'Ethiopia',
'Eritrea',
'Yemen',
'Burundi',
'Rwanda',
'Mali',
'Niger',
'Senegal',
'Uganda',
'Gambia',
'Haiti',
'Nigeria')

# 2. Make a tibble

motherDeaths = tibble(Countries = nams,
  DeathRates = deaths, Attended = attend)

write.csv(motherDeaths,file = "motherDeaths.csv",
  row.names = FALSE)

# 3. Make a scatterplot with a smooth

plt <-  ggplot(motherDeaths,
  aes(x = Attended, y = DeathRates)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
      size=1.5,color="blue") +
  geom_point(shape=21,size=4,color="black",fill="red") +
  labs(x="Percent Births Attended By Trained Personnel",
       y="Maternal Deaths Per 1000 Live Births",
       title="Maternal Risk 1990-1996") + hw
plt

# 4. Filtering: Select Countries and Add labels to Points

# Below %>% pipes the motherDeaths data.frame into
# the first argument of filter(). The filter function
# selects rows of the data.frame.

ptLabs <- motherDeaths %>% filter(Countries %in%
     c('Sierra Leone','Guinea','Haiti','Nigeria'))
ptLabs

# Below we use the selected Country
# names as labels. We have nudged the labels
# upward by 45 y-axis units.

plt + geom_label(data = ptLabs,
      aes(label = Countries),
      nudge_y = .45)

# Address mousehovers with ggplotly later
# ggplotly(plt)

# 5. Make row labeled plot with juxtaposed columns for two
#    different variables

# The ggplot facet_grid and facet_wrap are often useful
# for producing multiple panel layouts. We will make
# frequent use of this latter.
#
# The script below shows an alternative way to produce
# multiple panel plots. It uses grid.arrange() from the
# gridExtra package.
#
# We save the plots as named objects and
# put them the panels. We can control the number rows and
# columns of panels.  As as far as I know we cannot control
# the horizonal spacing between the panels using grid.arrange
# Likely this can be done with ggtable.
#
# The micromap examples in the Carr and Pickle text were produced
# using are panelLayout functions that provided extensive control
# for base level R graphics.

# 5.1 Test the left column dot plot

ggplot(motherDeaths,aes(x = Attended,
  y = reorder(Countries,-Attended))) +
  geom_point(shape = 21,fill = "cyan",
    color = 'black',size = 3) +
  labs(x = "Percent Births Attended\nBy Trained Personnel",
     y = "Countries") +  hw

# 5.2 Make the Left column Dot Plot with the Country Names
#     Omit the title and the y axis label use the theme in hwLeft

pltLeft <- ggplot(motherDeaths,aes(x = Attended,
  y = reorder(Countries,-Attended))) +
  geom_point(shape = 21,fill = "cyan",
    color = 'black',size = 3.5) +
  labs(y = "",
     x = "Percent Births\n Attended By Trained Personnel") +
  hwLeft

pltLeft

# 5.3 Make the Right column Dot Plots without the Country names
#     Omit the title


pltRight <- ggplot(motherDeaths,aes(x = DeathRates,
  y = reorder(Countries,-Attended))) +
  geom_point(shape = 21,fill = "red",
    color = 'black',size = 3.5) +
  labs(y = "",
     x = "Maternal Deaths\nPer 1000 Live Births") +  hwRight
pltRight

# 5.4 put the two together and add a title using grid.arrange


grid.arrange(pltLeft,pltRight,ncol = 2,widths = c(2.6,2),
  top = "Maternal Risk Giving Birth\n For Selected Countries 1990-1965")

# The large gap between the panels wastes space and increases the
# visual traversal distance.

# 6. Creating Perceptual Groups of size 3

# Here we modify the data.frame
# and give it a new name.
# The construction
# 1) Arranges the data.frame rows so the
#    countries are in the top down order that we
#    want in the plot.
# 2) Change the country factor level to be
#    in the reverse order of the top down order
#    consistent with plot y-axis convention.
# 3) Adds a row perceptual row grouping factor
#    with levels the keep groups of rows
#    in the same top down order


#6.1 Sort the data frame rows so the Country
#    order match that in 5. above
#    Revise the factor level match.

# Get the topDown order
newLevels <- levels( with(motherDeaths,reorder(Countries,-Attended)))
topDown <- rev(newLevels)
topDown

# Reaarrange rows
subs <- match(topDown,motherDeaths$Countries)
mdSort <- motherDeaths[subs,]  # rearrange rows

# Make factor levels provide the top down order
mdSort$Countries <- factor(topDown,rev(topDown))

# Sort the rows in the desired order
ord <- order(motherDeaths$Attended)
mdSort <- motherDeaths[ord,]
mdSort

# 6.2  Add a grouping factor

# We have 21 rows.
# Here we use 7 groups (labeled G1 to G7) of size 3
# Another reasonable choice is a 5-5-1-5-5 pattern
#   The facet grid argument space="free_y"
#   will give less space to the group of size 1.

nam <- paste0("G",7:1)
Grp <- factor(rep(nam,each = 3),levels=nam)
mdSort$Grp <- Grp

windows()
pLeft <- ggplot(mdSort,aes(x = Attended, y = Countries)) +
  geom_point(shape = 21,fill = "cyan",color = 'black',size = 3.5)+
  facet_grid(Grp~.,scales = "free_y") +
  labs(y = "",
     x = "Percent Births\n Attended By Trained Personnel") + hwLeft
pLeft

pRight <- ggplot(mdSort,aes(x = DeathRates, y = Countries)) +
  geom_point(shape = 21,fill = "red",color = 'black',size = 3.5) +
  facet_grid(Grp~.,scales = "free_y") +
  labs(y="", x="Maternal Deaths\nPer 1000 Live Births") + hwRight
pRight

windows()
grid.arrange(pLeft,pRight,ncol=2,widths=c(2.6,2),
  top="Maternal Risk Giving Birth\n For Selected Countries 1990-1965")


# 7. Comment on breaking ties and function
#    relationships
#
# In this example the attended percents
# and the death rates have a negative
# correlation.  We have sorted the countries
# in ascending order based on their attended
# percents. The attended percents have
# ties.  We could use the decreasing order
# of death rates to break the ties. This would
# call a little more attention to the negative
# correlation.
#
# The scatterplot provides a better to look for
# a functional relationship of form:
# deathRate = f(Attendance percent) + error



