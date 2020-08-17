# Follows An Introduction to Statistical Learning
# with Applications in R
# By Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani

# http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf

# good notes
# https://tdg5.github.io/stats-learning-notes/

library("MASS")
library("ISLR")
library("tidyverse")
library("QuantPsyc")
library("tidymodels")
library("ggfortify")
library("GGally")


##### CONCEPTUAL---------------------------------------------------------------

#1) The p-values correspond to the null hypothesis that TV, radio, and newspaper are each
#     individually related to the dependent variable in the presense of the other two 
#     covariates

#2) In KNN regression, the dependent variable is a continuous variable while in classification
#     it is a categorical variable. KNN regression averages out the values of the N nearest 
#     neighbors while KNN classification takes the mode value.

#3a) GPA (20) IQ (.07) Gender (35) GPA:IQ (0.01) GPA:Gender (-10)
# iii is correct because of the last interaction term between GPA and 
# Gender with a negative coefficient
#b) (Salary | Female, IQ=110, GPA=4.0) = 50 + 4*20 + 110*0.07 + 1*35 + 4*110*0.01 + 4*1*(-10)
#     = 137.1
#c) False, it is dependent upon the p-value or t-statistic. Also, although the coefficient is 
#     relatively small, GPA and IQ can have relatively high variable values which are then 
#     multiplied by the coefficient


#4a) I would expect the training RSS to be lower in the cubic regression since it includes
#       more parameters and can therefore more closely align with the training data
#b) I would expect the test RSS to be lower in the linear regression since the true
#       underlying relationship is linear
#c) Once again, in the training RSS I would expect the cubic regression to fit better
#       since it contains more parameters
#d) There is not enough information to tell since it is dependent on the specifics
#       of the true underlying relationship

#5) Answered on paper

#6) In simple linear regression there is only one regressor. Therefore
#     passing through the point (x-bar, y-bar) would give beta-hat1 a value of 0. 
#     This leads to y equal to the intercept (beta-hat0)

#7) Answered on paper


##### APPLIED-------------------------------------------------------------------

#8a)
data("Auto")
lm.fit2 <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(mpg ~ horsepower, data = Auto)
tidy(lm.fit2)
# horsepower has a negative coefficient that is statistically significant
new_point <- expand.grid(horsepower = 98)
predict(lm.fit2, new_data = new_point)
predict(lm.fit2, new_data = new_point, type = "conf_int")
predict(lm.fit2, new_data = new_point, type = "pred_int")

#b)
ggplot(data = Auto, aes(x = horsepower, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw()

#c)
ggplot2::autoplot(lm.fit2$fit)
# There is some evidence of non-linearity due to the residuals vs fitted graph
# There is also some evidence of outliers and a few high leverage points


#9a)
ggpairs(data = Auto, columns = 1:8)

#b)
auto.cor <- cor(Auto[,1:8])

#c)
lm.fit3 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ . -name, data = Auto)
tidy(lm.fit3)
glance(lm.fit3)
summary(lm.fit3$fit)
# (i) yes there is a relationship between the predictors and the response due to the F-statistic
# (ii) displacement, weight, year, and origin are statistically sign at a 5% level
# (iii) holding all other variables the same, newer cars will have better mpg

#d) 
autoplot(lm.fit3$fit)
plot(lm.fit3$fit)
# There is some evidence of non-normality. There are a few large outliers
# The leverage plot shows one extreme high leverage point

#e)
lm.fit4 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ cylinders*displacement + displacement*weight, data = Auto)
tidy(lm.fit4)
glance(lm.fit4)
summary(lm.fit4$fit)
# In this case, only the interaction between displacement and weight appear to be SS

#f)
lm.fit5 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ poly(displacement,3) + weight + year + origin, data = Auto)
tidy(lm.fit5)
glance(lm.fit5)
summary(lm.fit5$fit)
# It is interesting to compare the p-values of the different displacement polynomials

#10a)
data(Carseats)
lm.fit6 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Sales ~ Price + Urban + US, data = Carseats)
tidy(lm.fit6)
glance(lm.fit6)

#b)
# Holding all else constant (HAEC), increasing price leads to a decrease in sales
# HAEC, urban stores will sell ~$50 less than rural stores
# HAEC US stores will sell ~$1,200 more than international stores

#c)
# Sales = 13 + (-0.0545 * Price) + (-0.0219 * UrbanYes) + (1.2 * USYes)

#d) 
# Looking at the test statistic and p-value at a 5% level, i can reject the null
#   hypothesis for Price and US

#e)
lm.fit7 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Sales ~ Price + US, data = Carseats)
tidy(lm.fit7)
glance(lm.fit7)

#f)
# There is basically no change in the models fitting ability between the two

#g)
tidy(lm.fit7, conf.int = T)

#h)
ggplot(data = NULL, aes(x = predict(lm.fit7$fit), y = rstudent(lm.fit7$fit))) + 
  geom_point() + 
  theme_bw()
autoplot(lm.fit7$fit)
# There is no evidence of outliers, but there is evidence of high leverage points


#11a)
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

lm.fit8 <- lm(y ~ x + 0)
summary(lm.fit8)
# It is very statistically significant and has a coefficient around 2

#b)
lm.fit9 <- lm(x ~ y + 0)
summary(lm.fit9)
# similar to above, just with a smaller coefficient

#c) 
# The coefficients should be inverses of each other, and they are relatively close

#d)
# first part on paper
n <- length(x)
(sqrt(n - 1) * sum(x * y)) / (sqrt(sum(x^2) * sum(y^2) - (sum (x * y)^2)))

#e)
# by visually inspecting the formula from (10d), you can see that x and y are completely
# interchangeable in the formula and will not change the result

#f)
lm.fit10 <- lm(x ~ y)
summary(lm.fit10)

lm.fit11 <- lm(y ~ x)
summary(lm.fit11)


#12a)
# It is only when the sum of x^2 is equal to the sum of y^2

#b)
set.seed(2)
xx <- rnorm(100)
yy <- runif(100)
summary(lm(xx ~ yy + 0))
summary(lm(yy ~ xx + 0))

#c)
xxx <- c(rep(5, 4), rep(0, 96))
yyy <- c(10, rep(0, 99))
summary(lm(xxx ~ yyy + 0))
summary(lm(yyy ~ xxx + 0))


#13a)
set.seed(1)
x <- rnorm(100)

#b)
eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

#c)
y <- -1 + 0.5 * x + eps
length(y)
# y is the same length as X. Beta0 is -1 and Beta1 is 0.5

#d)
ggplot(data = NULL, aes(x = x, y = y)) + 
  geom_point() + 
  theme_bw()
#it is a straight line where slope = Beta1 

#e)
summary(lm(y ~ x))
#the estimates of the coefficients are close to the parameter values

#f)
ggplot(data = NULL, aes(x = x, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_abline(intercept = -1, slope = 0.5, color = "red") +
  theme_bw() 

#g)
lm.fit12 <- lm(y ~ x + I(x^2))
summary(lm.fit12)  
#there is not evidence that the polynomial improves the model fit

#h & i)
eps1 <- rnorm(100, mean = 0, sd = sqrt(0.1))
eps2 <- rnorm(100, mean = 0, sd = sqrt(0.5))
y1 <- -1 + 0.5 * x + eps1
y2 <- -1 + 0.5 * x + eps2
ggplot(data = NULL, aes(x = x, y = y1)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_abline(intercept = -1, slope = 0.5, color = "red") +
  theme_bw() 
ggplot(data = NULL, aes(x = x, y = y2)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_abline(intercept = -1, slope = 0.5, color = "red") +
  theme_bw() 
lm.fit13 <- lm(y1 ~ x + I(x^2))
summary(lm.fit13)  
lm.fit14 <- lm(y2 ~ x + I(x^2))
summary(lm.fit14)  
# The model with the least variance looks like it fits the dataset the best which
#   intuitively makes sense
# However, the coefficients are all roughly the same values and it is the standard 
#   errors that change the mose

#j)
confint(lm.fit12)
confint(lm.fit13)
confint(lm.fit14)
# The lower the epsilon variance, the narrower the confidence intervals in general


#14a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
# y = B0 + B1 * x1 + B2 * x2 + epsilon
# B0 = 2, B1 = 2, and B2 = 0.3

#b)
cor(x1,x2)
ggplot(data = NULL, aes(x = x1, y = x2)) + 
  geom_point() + 
  theme_bw()

#c)
lm.fit15 <- lm(y ~ x1 + x2)
summary(lm.fit15)
# only x1 is statistically significant
# B0 = 2.13, B1 = 1.44, B2 = 1.01
# These are relatively close to the parameters, but definitely has some diviation
# At a 5% level, we can only reject the null hypothesis for B1 = 0

#d)
lm.fit16 <- lm(y ~ x1)
summary(lm.fit16)
# Now it is much closer to the parameters. We can still reject H0 for B1 = 0

#e)
lm.fit17 <- lm(y ~ x2)
summary(lm.fit17)
# Now the results are far away from the true parameters, 
# but now we can reject H0 for B2 = 0

#f) 
# No, because there is high correlation between x1 and x2 which leads to OVB

#g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
lm.fit18 <- lm(y ~ x1 + x2)
summary(lm.fit18)
lm.fit19 <- lm(y ~ x1)
summary(lm.fit19)
lm.fit20 <- lm(y ~ x2)
summary(lm.fit20)
plot(lm.fit18)
plot(lm.fit19)
plot(lm.fit20)
# there are some outliers in x1 + x2
# x1 only: high leverage from the added point, but no outlier
# x2 only: high leverage from new point but falls close to regression line



#15a)
data(Boston)
lm.bos1 <- lm(crim ~ zn, data = Boston)
lm.bos2 <- lm(crim ~ indus, data = Boston)
lm.bos3 <- lm(crim ~ chas, data = Boston)
lm.bos4 <- lm(crim ~ nox, data = Boston)
lm.bos5 <- lm(crim ~ rm, data = Boston)
lm.bos6 <- lm(crim ~ age, data = Boston)
lm.bos7 <- lm(crim ~ dis, data = Boston)
lm.bos8 <- lm(crim ~ factor(rad), data = Boston)
lm.bos9 <- lm(crim ~ tax, data = Boston)
lm.bos10 <- lm(crim ~ ptratio, data = Boston)
lm.bos11 <- lm(crim ~ black, data = Boston)
lm.bos12 <- lm(crim ~ lstat, data = Boston)
lm.bos13 <- lm(crim ~ medv, data = Boston)
# there seems to be statistical significance in all regressions except for chas
plot(Boston$tax, Boston$crim)
plot(Boston$medv, Boston$crim)

#b)
lm.all <- lm(crim ~ ., data = Boston)
summary(lm.all)
# we now find that the only variables with statistical significance at a 5% level are
#   zn, dis, rad, black, and medv
# This makes me believe there is lots of omitted variable bias beforehand

#c)
simp_reg <- c(lm.bos1$coefficients[2], lm.bos2$coefficients[2], lm.bos3$coefficients[2], 
              lm.bos4$coefficients[2], lm.bos5$coefficients[2], lm.bos6$coefficients[2], 
              lm.bos7$coefficients[2], lm.bos8$coefficients[2], lm.bos9$coefficients[2], 
              lm.bos10$coefficients[2], lm.bos11$coefficients[2], lm.bos12$coefficients[2], 
              lm.bos13$coefficients[2])
ggplot(data = NULL, aes(x = simp_reg, y = lm.all$coefficients[-1])) + 
  geom_point() + 
  theme_bw()

#d)
lm.nla1 <- lm(crim ~ poly(zn, 3), data = Boston)
summary(lm.nla1)
# yes there is evidence for zn of the squared term

lm.nla2 <- lm(crim ~ poly(indus, 3), data = Boston)
summary(lm.nla2)
# yes there is evidence for indus for all three terms

lm.nla4 <- lm(crim ~ poly(nox, 3), data = Boston)
summary(lm.nla4)
#yes there is evidence for nox for all three terms

lm.nla5 <- lm(crim ~ poly(rm, 3), data = Boston)
summary(lm.nla5)
# yes there is evidence for rm of the squared term

lm.nla6 <- lm(crim ~ poly(age, 3), data = Boston)
summary(lm.nla6)
#yes there is evidence for age for all three terms

lm.nla7 <- lm(crim ~ poly(dis, 3), data = Boston)
summary(lm.nla7)
# yes there is evidence for dis for all three terms

lm.nla8 <- lm(crim ~ poly(rad, 3), data = Boston)
summary(lm.nla8)
# yes there is evidence for rad of the squared term

lm.nla9 <- lm(crim ~ poly(tax, 3), data = Boston)
summary(lm.nla9)
# yes there is evidence for tax of the squared term

lm.nla10 <- lm(crim ~ poly(ptratio, 3), data = Boston)
summary(lm.nla10)
# yes there is evidence for ptratio for all three terms

lm.nla11 <- lm(crim ~ poly(black, 3), data = Boston)
summary(lm.nla11)
#no, there is not any evidence for non linear association

lm.nla12 <- lm(crim ~ poly(lstat, 3), data = Boston)
summary(lm.nla12)
# yes there is evidence for lstat of the squared term

lm.nla13 <- lm(crim ~ poly(medv, 3), data = Boston)
summary(lm.nla13)
# yes there is evidence for medv for all three terms


