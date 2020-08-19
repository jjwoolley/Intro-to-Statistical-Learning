# Follows An Introduction to Statistical Learning
#   with Applications in R
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
library("class")
library("discrim")
library("klaR")

##### CONCEPTUAL----------------------------------------------------------------

#1)
# Done on paper

#2)
# Done on paper

#3)
# Done on paper

#4a)
# 0.1^(1) = 10%

#4b)
# 0.1^(2) = 1%

#4c)
# 0.1^100 = really small

#4d)
# the more dimensions of p there are, the fewer points that will be relatively "near"
# each other in all dimensions

#4e)
# for p = 1, the length of each side of the hypercube is 0.1
# for p = 2, the length of each side of the hypercube is 0.1^(1/2)
# for p = 100, the length of each side of the hypercube is 0.1^(1/100)

#5a)
# QDA will perform better on the training set, but LDA will perform better on the test set

#5b)
# QDA will perform better on both training and test sets

#5c)
# I would expect the QDA prediction accuracy to increase relatively to that of LDA's
# This is because in the bias-var tradeoff, the larger sample size helps reduce the var

#5d)
# false, it could easily lead to overfitting 

#6a)
exp(-6 + 0.05 * 40 + 1 * 3.5)/(1 + exp(-6 + 0.05 * 40 + 1 * 3.5))
# this gives a probablity of about 37.8%

#6b)
# 50 (done on paper using calculator)

#7)
# issued dividend:      x-bar = 10
# not issued dividend:  x-bar = 0
# var-hat = 36
# 80% of companies issued dividends
# predict the probability that a company will issue a dividend this year given 
#   that its percentage profit was X = 4 last year
p_yes_x <- (0.8 * exp((-1/72)*(4-10)^2)) / 
  ((0.2 * exp((-1/72)*(4-0)^2)) + (0.8 * exp((-1/72)*(4-10)^2)))
p_yes_x
# 0.752

#8)
# with K = 1 there is 0% error rate in the training set since its 1 nearest neighbor
# would be itself. Therefore the test error rate would have to be 36% 
# I would go with the logistic regression since it has a smaller test error rate

#9a)
0.37/(1 + 0.37)
# 27%

#9b)
# 0.16 / (1 - 0.16)
# this is equal to odds of 0.19




##### APPLIED-------------------------------------------------------------------

#10a)
data(Weekly)
ggpairs(data = Weekly, columns = 1:8)
# The only correlations above a magnitude of 0.1 are between year and volume

#10b)

lr.fit1 <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
      data = Weekly,
      family = "binomial")
summary(lr.fit1$fit)
# only Lag2 is SS at a 5% level

#10c)
logit.pred <- ifelse(lr.fit1$fit$fitted.values > 0.5, "Up", "Down")
table(logit.pred, Weekly$Direction)
# The true positive/Sensitivity is 557/605 = 92%
# The Specificity is 1 - 48/484 = 90.08%

#10d)
train1 <- filter(Weekly, Year <= 2008)
test1 <- filter(Weekly, Year > 2008)
lr.fit2 <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Direction ~ Lag2, data = train1, family = "binomial")
log.pred2 <- predict(lr.fit2, test1)

table(as.character(unlist(log.pred2)), test1$Direction)
mean(as.character(unlist(log.pred2)) == test1$Direction)

#10e)
lda.fit3 <- discrim_linear() %>%
  set_engine("MASS") %>%
  fit(Direction ~ Lag2, data = train1, family = "binomial")
lda.fit3
lda.pred3 <- predict(lda.fit3, test1)

table(as.character(unlist(lda.pred3)), test1$Direction)
mean(as.character(unlist(lda.pred3)) == test1$Direction)

#10f)
qda.fit4 <- qda(Direction ~ Lag2, data = train1, family = "binomial")
qda.fit4
qda.pred4 <- predict(qda.fit4, test1)
table(qda.pred4$class, test1$Direction)
mean(qda.pred4$class == test1$Direction)
#every prediction is "Up"

#10g)

train.X <- as.matrix(train1$Lag2)
test.X <- as.matrix(test1$Lag2)
knn.fit5 <- knn(train = train.X, test = test.X, train1$Direction, k = 1)
table(knn.fit5, test1$Direction)
mean(knn.fit5 == test1$Direction)

#10h)
# The logistic regression and lqa appear to work the best

#10i)
# Messed around and it still seems like lda and logistic are the two best



#11a)
data(Auto)
mpg_med <- median(Auto$mpg)
Auto1 <- Auto %>%
  mutate(mpg01 = as.factor(ifelse(mpg > mpg_med, 1, 0)))

#11b)
ggpairs(Auto1, columns = c(1:8,10))
# many of these variables are highly correlated with each other

#11c)
set.seed(1)
index <- sample(1:length(Auto1$mpg), size = trunc(0.5 * length(Auto1$mpg)))
train.auto <- Auto1 %>%
  filter(row_number() %in% index)
test.auto <- Auto1 %>%
  filter(!(row_number() %in% index))

#11d)
fit6.lda <- discrim_linear() %>%
  set_engine("MASS") %>%
  fit(mpg01 ~ cylinders + displacement  + weight,
      data = train.auto, 
      family = "binomial")
fit6.lda
pred6.lda <- predict(fit6.lda, test.auto)

table(as.character(unlist(pred6.lda)), test.auto$mpg01)
mean(as.character(unlist(pred6.lda)) == test.auto$mpg01)
# 88.27% correct

#11e)
fit7.qda <- qda(mpg01 ~ cylinders + displacement  + weight,
                data = train.auto, 
                family = "binomial")
fit7.qda
pred7.qda <- predict(fit7.qda, test.auto)$class
table(pred7.qda, test.auto$mpg01)
mean(pred7.qda == test.auto$mpg01)
# 88.78% correct 

#11f)
fit8.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(mpg01 ~ cylinders + displacement  + weight,
                data = train.auto, 
                family = "binomial")
fit8.log
pred8.log <- predict(fit8.log, test.auto)
table(as.character(unlist(pred8.log)), test.auto$mpg01)
mean(as.character(unlist(pred8.log)) == test.auto$mpg01)
# 87.76% correct

#11g)
# need to get rid of name variable
fit9.knn <- knn(train.auto[, -9], test.auto[, -9], cl = test.auto$mpg01, k=50)
table(fit9.knn, test.auto$mpg01)
mean(fit9.knn == test.auto$mpg01)
# 87.24% correct
# all models were relatively close, but in the end qda was the best



#12a)
Power <- function(){
  print(2^3)
}

#12b)
Power2 <- function(x, a){
  print(x^a)
}

#12c)
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

#12d)
Power3 <- function(x, a){
  result <- x^a
  return(result)
}

#12e)
x <- 1:10
ggplot(data = NULL, aes(x = x, y = Power3(x, 2))) +
  geom_point() + 
  theme_bw() +
  xlab("X") +
  ylab("X^2") +
  ggtitle("Power3!")

PlotPower <- function(x, a){
  ggplot(data = NULL, aes(x = x, y = Power3(x, a))) +
    geom_point() + 
    theme_bw() +
    xlab("X") +
    ylab("X^2") +
    ggtitle(paste0("Power", a))
}
PlotPower(1:10, 3)

#13)
data(Boston)
crim_med <- median(Boston$crim)
Boston01 <- Boston %>%
  mutate(crim01 = ifelse(crim > crim_med, 1, 0))

set.seed(1)
index.bos <- sample(1:length(Boston01$crim01), size = trunc(0.5 * length(Boston01$crim01)))
train.bos <- Boston01 %>%
  filter(row_number() %in% index.bos)
test.bos <- Boston01 %>%
  filter(!(row_number() %in% index.bos))

ggpairs(Boston01, columns = 2:15)

# i will include variables with corr > 0.5 with crim01
# indus, nox, age, dis, rad, tax


# LOGISTIC REGRESSION
fit10.log <- glm(crim01 ~ indus + nox + age + dis + rad + tax,
                 data = train.bos,
                 family = "binomial")
summary(fit10.log)
pred10.log <- predict(fit10.log, test.bos, type = "response")
pred10.log <- ifelse(pred10.log > 0.5, 1, 0)
table(pred10.log, test.bos$crim01)
mean(pred10.log == test.bos$crim01)
# 88.54% accuracy

# LDA
fit11.lda <- lda(crim01 ~ indus + nox + age + dis + rad + tax,
                 data = train.bos,
                 family = "binomial")
pred11.lda <- predict(fit11.lda, test.bos)$class
table(pred11.lda, test.bos$crim01)
mean(pred11.lda == test.bos$crim01)
# 84.19% accuracy

# QDA
fit12.qda <- qda(crim01 ~ indus + nox + age + dis + rad + tax,
                 data = train.bos,
                 family = "binomial")
pred12.qda <- predict(fit12.qda, test.bos)$class
table(pred12.qda, test.bos$crim01)
mean(pred12.qda == test.bos$crim01)
# 90.11% accuracy

# KNN
fit13.knn <- knn(train.bos[,c(3, 5, 7:10, 15)],
                 test.bos[,c(3, 5, 7:10, 15)], 
                 cl = test.bos$crim01,
                 k = 20)
table(fit13.knn, test.bos$crim01)
mean(fit13.knn == test.bos$crim01)
# 78.66% accuracy


# The best model based off of its testing sample accuracy would be the QDA


