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
fit.7.gam2 <- gam(wage ~ s(age, 2) + maritl + jobclass + education, data = Wage)
fit.7.gam3 <- gam(wage ~ s(age, 3) + maritl + jobclass + education, data = Wage)
fit.7.gam4 <- gam(wage ~ s(age, 4) + maritl + jobclass + education, data = Wage)
fit.7.gam5 <- gam(wage ~ s(age, 5) + maritl + jobclass + education, data = Wage)

summary(fit.7.cut)
summary(fit.7.gam2)
anova(fit.7.gam2, fit.7.gam3, fit.7.gam4, fit.7.gam5)
# should go with either the df of 2 or 3
par(mfrow = c(2, 2))
plot(fit.7.gam3, se = T)


pred.7.cut <- predict(fit.7.cut, Wage)
pred.7.gam3 <- predict(fit.7.gam3, Wage)
pred.7.gam2 <- predict(fit.7.gam2, Wage)

mean((pred.7.cut - Wage$wage)^2)
mean((pred.7.gam3 - Wage$wage)^2)
mean((pred.7.gam2 - Wage$wage)^2)

ggplot(data = Wage, aes(x = age, y = wage)) + 
  geom_point(alpha = 0.1) +
  geom_line(data = NULL, aes(x = age, y = pred.7.cut), color = "red") +
  geom_line(data = NULL, aes(x = age, y = pred.7.gam3), color = "blue")
# this graph does not tell us much since there are other vars not included


#8)
data(Auto)
ggpairs(Auto[,-9])
# appears to be some non linearity between mpg and horsepower/weight
fit.8.gam4 <- gam(mpg ~ s(horsepower, 4) + s(weight, 4) + displacement + year + origin, 
                 data = Auto)
fit.8.gam3 <- gam(mpg ~ s(horsepower, 3) + s(weight, 4) + displacement + year + origin, 
                  data = Auto)
anova(fit.8.gam3, fit.8.gam4)
# no reason to go from 3 to 4

fit.8.poly3 <- lm(mpg ~ poly(horsepower, 3, raw = T) + poly(weight, 3, raw = T) + 
                    displacement + year + origin, data = Auto)
fit.8.lm <- lm(mpg ~ horsepower + weight + displacement + year +origin, data = Auto)
summary(fit.8.lm)
summary(fit.8.poly3)
plot(fit.8.gam3)




#9a)
data(Boston)
fit.9.poly <- lm(nox ~ poly(dis, 3, raw = T), data = Boston)
summary(fit.9.poly)

pred.9.poly <- predict(fit.9.poly, Boston)

par(mfrow = c(2, 2))
plot(fit.9.poly)
ggplot(data = NULL, aes(x = Boston$dis, y = pred.9.poly)) +
  geom_line() +
  theme_bw() +
  xlab("dis") +
  ylab("nox-hat")

#9b)
rss.9.poly <- rep(NA, 10)
for(i in 1:10) {
  fit <- lm(nox ~ poly(dis, i, raw = T), data = Boston)
  rss.9.poly[i] <- sum(fit$residuals^2)
}
ggplot(data = NULL, aes(x = 1:10, y = rss.9.poly)) +
  geom_line() +
  theme_bw()
# sharp decreases around 3 and again around 8


#9c)
cv.9c.fit <- rep(NA, 10)

for(i in 1:10){
  fit <- glm(nox ~ poly(dis, i, raw = T), data = Boston)
  cv.9c.fit[i] <- cv.glm(Boston, fit, K = 10)$delta[2]
}

ggplot(data = NULL, aes(x = 1:10, y = cv.9c.fit)) +
  geom_line() +
  theme_bw()
# this makes it look like degree of 3 is optimal, and then it starts to overfit

#9d)
fit.9d.splines <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(fit.9d.splines)
pred.9d.splines <- predict(fit.9d.splines, Boston)
ggplot(data = NULL, aes(x = Boston$dis, y = pred.9d.splines)) +
  geom_line() +
  geom_point(data = Boston, aes(x = dis, y = nox), alpha = 0.1) + 
  theme_bw()
# this seems to be a good fit, although it is hard to tell at high values of dis


#9e)
rss.9e.bs <- rep(NA, 10)

for(i in 3:10) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss.9e.bs[i] <- sum(fit$residuals^2)
}
rss.9e.bs

ggplot(data = NULL, aes(x = 3:10, y = rss.9e.bs[3:10])) +
  geom_line() +
  xlab("Degrees of Freedom") +
  ylab("RSS") +
  theme_classic()
# RSS decreases sharply until 5 DF, then another sharp decrease at 10

#9f)
cv.9f.bs <- rep(NA, 10)
for(i in 3:10){
  fit <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv.9f.bs[i] <- cv.glm(data = Boston, fit, K=10)$delta[2]
}
cv.9f.bs
which.min(cv.9f.bs)

ggplot(data = NULL, aes(x = 3:10, y = cv.9f.bs[3:10])) +
  geom_line() +
  xlab("Degrees of Freedom") +
  ylab("CV Error") +
  theme_bw()
# 5 degrees of freedom seems like a good choice


#10a)
data("College")
college.split <- initial_split(College)
college.train <- training(college.split)
college.test <- testing(college.split)

reg.10a.full <- regsubsets(Outstate ~., nvmax = 18, method = "forward",
                           data = college.train)
reg10a.summ <- summary(reg.10a.full)
which.min(reg10a.summ$bic)
which.min(reg10a.summ$cp)
which.max(reg10a.summ$adjr2)

vars.10a <- names(coef(reg.10a.full, 11))[-1]

lm.10a <- lm(Outstate ~ Private + Apps + Accept + Top10perc + F.Undergrad +
               Room.Board + PhD + Terminal + perc.alumni + Expend + Grad.Rate,
             data = college.train)
summary(lm.10a)
pred.10a.lm <- predict(lm.10a, newdata = college.test)
mean((pred.10a.lm - college.test$Outstate)^2)


#10b)
fit.10b.gam <- gam(Outstate ~ Private + s(Apps, 2) + s(Accept, 2) + s(Top10perc, 2)
                   + s(F.Undergrad, 2) + s(Room.Board, 2) + s(PhD, 2) 
                   + s(Terminal, 2) + s(perc.alumni, 2) + s(Expend, 5) + s(Grad.Rate, 2),
                   data = college.train)
summary(fit.10b.gam)

par(mfrow = c(3, 3))
plot(fit.10b.gam, se = T, col = "red")


#10c)
pred.10b.gam <- predict(fit.10b.gam, newdata = college.test)
mean((pred.10b.gam - college.test$Outstate)^2)
# it has lower mse than the linear model


#10d)
summary(fit.10b.gam)
# Expend, Accept, and Apps appear to have a nonlinear relationship with Outstate


#11a)
X1.11a <- rnorm(100)
X2.11a <- rnorm(100)
B0 <- -2
B1 <- 7
B2 <- 5
eps <- rnorm(100)

Y.11a <- B0 + B1 * X1.11a + B2 * X2.11a + eps

#11b)
B1.11a <- 1
B2.11a <- 1

#11c)
a.11c <- Y.11a - B1.11a * X1.11a
B2.11c <- lm(a.11c ~ X2.11a)$coef[2]
B2.11c

#11d)
a.11d <- B2.11c * X2.11a
B1.11d <- lm(a.11d ~ X1.11a)$coef[2]
B1.11d

#11e)
b.hat.0 <- rep(0, 1000)
b.hat.1 <- rep(0, 1000)
b.hat.2 <- rep(0, 1000)

for(i in 1:1000){
  a <- Y.11a - b.hat.1[i] * X1.11a
  b.hat.2[i] <- lm(a ~ X2.11a)$coef[2]
  
  a <- Y.11a - b.hat.2[i] * X2.11a
  b.hat.0[i] <- lm(a.2 ~ X1.11a)$coef[1]
  
  b.hat.1[i+1] <- lm(a ~ X1.11a)$coef[2]
}

ggplot(data = NULL, aes(x = 1:1000, y = b.hat.0)) +
  geom_line(color = "blue") + 
  geom_line(aes(x = 1:1000, y = b.hat.2), color ="red") +
  geom_line(aes(x = 1:1000, y = b.hat.1[-1]), color ="brown") +
  xlab("Iteration") +
  ylab("B-hat Value")

#11f)
fit.11f.lm <- lm(Y.11a ~ X1.11a + X2.11a)
summary(fit.11f.lm)
# the numbers perfectly match up

#11g)
# It took only 2 iterations to get extremely close to the multiple regression estimates

#12)
# I'll come back to this one






