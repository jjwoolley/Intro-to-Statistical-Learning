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
library("gam")


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

#6a)
data(Wage)
# plynomial regression to predict wage using age
# use cross-validation to select optimal degree d for the polynomial

set.seed(3)

deltas <- rep(NA, 10)
for(i in 1:10) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
deltas
which.min(deltas)

fit.6a.poly1 <- lm(wage ~ poly(age, 1), data = Wage)
fit.6a.poly2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.6a.poly3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.6a.poly4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.6a.poly5 <- lm(wage ~ poly(age, 5), data = Wage)
fit.6a.poly6 <- lm(wage ~ poly(age, 6), data = Wage)
fit.6a.poly7 <- lm(wage ~ poly(age, 7), data = Wage)
fit.6a.poly8 <- lm(wage ~ poly(age, 8), data = Wage)
fit.6a.poly9 <- lm(wage ~ poly(age, 9), data = Wage)
fit.6a.poly10 <- lm(wage ~ poly(age, 10), data = Wage)
anova(fit.6a.poly1, fit.6a.poly2, fit.6a.poly3, fit.6a.poly4, fit.6a.poly5, 
      fit.6a.poly6, fit.6a.poly7, fit.6a.poly8, fit.6a.poly9, fit.6a.poly10)

# the cross-validation says the poly(5) model is the best
# anova says that the poly(3) or poly(4) models are the best

agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])

pred.6a.poly4 <- predict(fit.6a.poly4, data.frame(age=age.grid))
tx <- cbind(pred.6a.poly4, age.grid)


ggplot(data = NULL, aes(x = 18:80, y = pred.6a.poly4)) + 
  geom_line(color = "red") +
  geom_point(data = Wage, aes(x = age, y = wage), alpha = 0.1) +
  theme_bw() +
  xlab("Age") +
  ylab("Wage")

#6b)

set.seed(1)


cvs <- rep(NA, 10)
for(i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
cvs
which.min(cvs)
# 8 cuts is optimal

fit.6b.step8 <- glm(wage ~ cut(age, 8), data = Wage)
pred.6b.step8 <- predict(fit.6b.step8, data.frame(age=age.grid))
ggplot(data = NULL, aes(x = 18:80, y = pred.6b.step8)) +
  geom_line(color = "blue") +
  geom_point(data = Wage, aes(x = age, y = wage), alpha = 0.1) +
  geom_line(data = NULL, aes(x = 18:80, y = pred.6a.poly4), color = "red") +
  theme_bw() +
  xlab("Age") +
  ylab("Wage")


#7)
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)
plot(Wage$education, Wage$wage)

fit.7.cut <- lm(wage ~ cut(age, 4) + maritl + jobclass + education, data = Wage)
fit.7.gam <- gam(wage ~ s(age, 4) + maritl + jobclass + education, data = Wage)
summary(fit.7.cut)
summary(fit.7.gam)






