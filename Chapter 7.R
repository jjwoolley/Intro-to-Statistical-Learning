# Follows An Introduction to Statistical Learning
#   with Applications in R
# By Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani

# http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf

# good notes
# https://tdg5.github.io/stats-learning-notes/


library("MASS")
library("ISLR")
library("tidyverse")
library("tidymodels")
library("GGally")
library("boot")
library("leaps")
library("glmnet")


### Conceptual------------------------------------------------------------------

#1a)
# a1 = B0, b1 = B1, c1 = B2, d1 = B3
# B4 does not matter since it is multiplied by zero

#1b,c,d, e)
# done on paper

#2a)
# g-hat = 0 since the first term does not matter and the derivative of g goes to 0

#2b)
# g-hat = some constant

#2c)
# g-hat = straight line (mx + b)

#2d)
# g-hat = quadratic curve (like x^2)

#2e)
# g-hat equals are regression line since the second term does not matter


#3)
# b1(X) = X
# b2(X) = (X - 1)^2 * I(X >= 1)
b0.3 <- 1
b1.3 <- 1
b2.3 <- -2
X.3 <- seq(from = -2, to = 2, length.out = 40)
Y.3 <- b0.3 + b1.3 * X + b2.3 * (X.3 - 1)^2 * I(X.3 >= 1)
ggplot(data = NULL, aes(x = X.3, y = Y.3)) + 
  geom_line() + 
  geom_point()

#4)
b0.4 <- 1
b1.4 <- 1
b2.4 <- 3
X.4 <- seq(from = -2, to = 2, length.out = 40)
Y.4 <- b0.4 + b1.4 *(I(X.4 <= 2 & X.4 >= 0) - (X.4 - 1) * I(X.4 >= 1 & X.4 <= 2)) +
  b2.4 * ((X.4 - 3) * I(X.4 >= 3 & X.4 <= 4) + I(X.4 > 4 & X.4 <= 5))
ggplot(data = NULL, aes(x = X.4, y = Y.4)) +
  geom_line() +
  geom_point()

#5a)
# g2 will be more flexible and therefore will have a smaller training RSS

#5b)
# we do not know, it is dependent on the data generating process

#5c)
# they will be the same




### Applied---------------------------------------------------------------------


























