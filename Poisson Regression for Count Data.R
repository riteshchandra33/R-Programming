#Poisson Regression
#Poisson regression is used to model count variables
Example 1. The number of persons killed by horse kicks in the Prussian army per year. 
Example 2. The number of people in line in front of you at the grocery store. 
Predictors may include the number of items currently offered at a special discounted price 
and whether a special event (e.g., a holiday, a big sporting event) is three or fewer days away.

Example 3. The number of accidents in Lee Highway between 4 and 5 PM in a given day.
Example 4. The number of awards earned by students at one high school. 
Predictors of the number of awards earned include the type of program in which the student was enrolled (e.g., vocational, general or academic) and the score on their final exam in math.

#For the purpose of illustration, we use data simulated for example 4 from the UCLA website. 
#In this example, num_awards is the outcome variable and indicates the number of awards earned by students at a high school in a year, math is a continuous predictor variable and represents students' scores on their math final exam, and prog is a categorical predictor variable with three levels indicating the type of program in which the students were enrolled. 
#It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
attach(p)
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

PoissonReg<-glm(num_awards ~ prog + math, family="poisson", data=p)
summary(PoissonReg)

-5.24+1.08*Program(academic vs general)+0.37*Program(Vocational vs general)+0.07*math

The coefficient for math is .07. 
This means that the expected log count for a one-unit increase in math is .07.

The indicator variable progAcademic compares between prog = "Academic" and prog = "General", 
the expected log count for prog = "Academic" increases by about 1.1. 
The indicator variable prog.Vocational is the expected difference in log count ((approx .37)) 
between prog = "Vocational" and the reference group (prog = "General").
