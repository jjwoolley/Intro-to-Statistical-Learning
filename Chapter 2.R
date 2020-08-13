#Finished
#Follows An Introduction to Statistical Learning
  #with Applications in R
  #By Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani

#http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf

#good notes
  #https://tdg5.github.io/stats-learning-notes/

library(MASS)
library(ISLR)
library(tidyverse)
library(QuantPsyc)


# Conceptual-------------------------------------------------------------------

#1a) More flexible, since we have a large n and aren't as worried about overfitting
#b) Inflexible, because it is easy to overfit with so few observations
#c) More flexible since it can adjust for the non-linearity better
#d) Inflexible because a more flexible model would fit to the noise in the error terms



#2a) Regression, inference, n = 500, p = 3
#b) Classification, prediction, 20, 13
#c) Regression, prediction, 52, 3



#3a) Hand drawn
#b) Bias: It decreases from left to right with deceleration. As flexibility increases,
#     the model becomes closer and closer to the training data points
#   Variance: It increases from left to right exponentially. As flexibility increases,
#     the model starts to overfit when flexibility is too high
#   Irreducible error curves: Flat horizontal line. Does not change based on flexibility
#   Test Error: U shaped curve above irreducible error line minimized where bias and var intersect
#   Training Error: Decreases from left to right since the model gets closer and closer to sample points



#4a) Whether someone makes a college soccer team based on speed and goals scored in prior year
#         response = whether they make the college team, predictors = speed and goals
#         goal = prediction
#b) What factors are associated with an increase in wage
#         response = wage, predictors = education, industry, age, etc
#         goal = inference
#c) States income based off of median GDP, poverty level, etc


#5) A more flexible model has the advantages if the true data follows a non-linear relationship
# Disadvantage: Can lead to overfitting and overfollowing the errors
# Use a more flexible approach if we have many observations and fewer variables, 
#   care more about prediction
# Use a less flexible approach if we have few observations and many variables, 
#   care more about inference


#6) A parametric approach has you reduce the problem of estimating f down to one of
#     estimating a set of parameters because it assumes a form for f
#   A non parametric approach does not assume a functional form of f so needs many observations
#   A parametric approach has the advantage of easier calculation and needing less observations
#   The disadvantage of a parametric model is if your functional form assumption is incorrect,
#     then your model will be very far off


#7a) Euclidean distance
#     Observation 1: sqrt((0 - 0)^2 + (3 - 0)^2 + (0 - 0)^2) = sqrt(9) = 3
#     Observation 2: sqrt(4 + 0 + 0) = 2
#     Observation 3: sqrt(0 + 1 + 9) = sqrt(10) ~ 3.16
#     Observation 4: sqrt(0 + 1 + 4) = sqrt(5)  ~ 2.23
#     Observation 5: sqrt(1 + 0 + 1) = sqrt(2)  ~ 1.41
#     Observation 6: sqrt(1 + 1 + 1) = sqrt(3)  ~ 1.73
#b) When K = 1, the one closest neighbor is Obs 5, so Green
#c) When K = 3, the three closest neighbors are Obs 5, 6, 2; so Red
#d) If the Bayes decision boundary in this problem is highly non-linear, then 
#     the best value for K would be relatively small since this follows a less-linear
#     bayes boundary




# Applied-----------------------------------------------------------------------

#8a)

# Bring college into the environment for viewing
data(College)
str(College)

#8b)
# These steps had already been done on dataset
# rownames(college) <- college[,1]
# college <- college[,-1]

#8c)
summary(College)
pairs(College[,1:10])
College <- College %>%
  mutate(Elite = as.factor(ifelse(Top10perc > 50, "Yes", "No")))
summary(College)

#boxplots
plot(College$Elite, College$Outstate)
ggplot(College, aes(Elite, Outstate)) + 
  geom_boxplot() +
  theme_bw()



#histogram

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

h1 <- ggplot(College, aes(x = Apps)) + 
  geom_histogram(bins = 40)
h2 <- ggplot(College, aes(x = F.Undergrad)) + 
  geom_histogram(bins = 40)
h3 <- ggplot(College, aes(x = Grad.Rate)) + 
  geom_histogram(bins = 40)
h4 <- ggplot(College, aes(x = Books)) + 
  geom_histogram(bins = 40)
multiplot(h1, h2, h3, h4, cols = 2)




#9
rm(College,h1, h2, h3, h4)
data(Auto)
Auto <- na.omit(Auto)

#9a)
str(Auto)
# quantitative:mpg, cylinders, displacement, horsepower, weight, acceleration, year
# qualitative:origin, name

#9b)
map(Auto[,1:7], range)
#9c)
map(Auto[,1:7], mean)
map(Auto[,1:7], sd)
#9d)
Auto_del<-Auto[-c(10:85),]
map(Auto_del[,1:7], range)
map(Auto_del[,1:7], mean)
map(Auto_del[,1:7], sd)
#9e)
ggplot(Auto, aes(cylinders, horsepower, col = origin)) + 
  geom_point() +
  theme_bw()
#9f)
ggplot(Auto, aes(horsepower, mpg)) +
  geom_point() +
  theme_bw()
# negative relationship between horsepower and mpg, could be driven through weight
lm.mpg <- lm(mpg ~ horsepower + weight + displacement + cylinders + acceleration + year, 
             data = Auto)
summary(lm.mpg)
# standardized coefficients
lm.beta(lm.mpg)
# weight is very important due to results from lm and standardized coefficients



#10a)
# rm(list=ls())
data(Boston)
# there are 506 rows (one for each obs) and 14 columns (one for each variable)
#10b)
h5 <- ggplot(data = Boston, aes(x = lstat, y = crim)) + 
  geom_point() +
  theme_bw()
h6 <-ggplot(data = Boston, aes(x = nox, y = rad)) + 
  geom_point() +
  theme_bw()
multiplot(h5, h6)
# positive relationship between higher proportion or poorer residents and crime rate
# seems a positive relationship between nox and rad
#10c)
lm.crim <- lm(crim ~ zn + indus + chas + age + dis + tax + ptratio + black + medv, 
              data = Boston)
summary(lm.crim)
lm.beta(lm.crim)
# tax and dis seem to be the most important variables
#10d)
ggplot(Boston, aes(crim)) + 
  geom_histogram() +
  theme_bw()
ggplot(Boston, aes(tax)) + 
  geom_histogram() +
  theme_bw()
ggplot(Boston, aes(ptratio)) + 
  geom_histogram() +
  theme_bw()
# reasonably high range for all of these values
#10e) 
sum(Boston$chas)
# 35 suburbs are bound to the Charles River
#10f)
median(Boston$ptratio)
# 19.05 median pupil-teacher ratio
#10g)
medv.min <- Boston[which(Boston$medv == min(Boston$medv)),]
medv.min
#10h)
count(Boston[which(Boston$rm > 7),])
count(Boston[which(Boston$rm > 8),])
#64 suburbs with rm > 7 while only 13 with rm > 8





