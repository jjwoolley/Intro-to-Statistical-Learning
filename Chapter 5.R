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



##### CONCEPTUAL----------------------------------------------------------------

#1)
# done on paper


#2a)
# (n-1)/n

#2b)
# (n-1)/n since there is replacement

#2c)
# due to replacement, with each drawn sample there is a (n-1)/n chance that
#   the jth observation will not be drawn. For example, if you were to draw
#   boostrap, then there is a [(n-1)/n]^2 chance that you will not be drawn
#   and this extends out to when B=n

#2d)
# 1 - [(5-1)/5]^5 = 1 - 0.32768 = 0.67232

#2e)
# 1 - [(100-1)/100]^100 = 0.63397

#2f)
# 0.632139

#2g)
x_data <- 1:10000
y_data <- 1 - ((x_data - 1) / x_data)^x_data
ggplot(data = NULL, aes(x = x_data, y = y_data)) + 
  geom_point() + 
  theme_bw()

#2h)
store <- rep (NA , 10000)
for (i in 1:10000) {
  store[i] <- sum(sample(1:100 , rep=TRUE) == 4) > 0
}
mean(store)
# this is very close to the result we got in f, since sample is random, the
#   actual number will change every time



#3a)
# k-fold cross validation is implemented by dividing the total data set into
#   k equally sized groups. Then select one group as the test set and use the 
#   remaining k-1 groups as the training set. Then calculate the mean squared error
#   You repeat this process k times until each group has been the test set once

#3b)
# (i) relative to the validation set approach, k-fold will have less variance but more bias
#       as well as more computationally intensive
# (ii) relative to LOOCV, k-fold will have more bias but less variance

#4)
# We would use the bootstrapping method. This involves taking B subsamples of our
#   sample and then use the resulting Y values to get a mean(Y) and sd(Y)




##### APPLIED-------------------------------------------------------------------

#5a)
data(Default)
fit1.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = Default)

#5b)
set.seed(1)
data1 <- initial_split(Default, prop = 0.5)
train1.default <- training(data1)
test1.default <- testing(data1)

fit2.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = train1.default)
pred2.log <- predict(fit2.log, test1.default) 

table(as.character(unlist(pred2.log)), test1.default$default)
mean(as.character(unlist(pred2.log)) == test1.default$default)
# 97.14% accurate

#5c)
set.seed(2)
data3 <- initial_split(Default, prop = 0.5)
train3.default <- training(data3)
test3.default <- testing(data3)

fit3.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = train3.default)
pred3.log <- predict(fit3.log, test3.default) 

table(as.character(unlist(pred3.log)), test3.default$default)
mean(as.character(unlist(pred3.log)) == test3.default$default)
# 97.12% accurate

set.seed(3)
data4 <- initial_split(Default, prop = 0.5)
train4.default <- training(data4)
test4.default <- testing(data4)

fit4.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = train4.default)
pred4.log <- predict(fit4.log, test4.default) 

table(as.character(unlist(pred4.log)), test4.default$default)
mean(as.character(unlist(pred4.log)) == test4.default$default)
# 97.32% accurate


set.seed(5)
data5 <- initial_split(Default, prop = 0.5)
train5.default <- training(data5)
test5.default <- testing(data5)

fit5.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = train5.default)
pred5.log <- predict(fit5.log, test5.default) 

table(as.character(unlist(pred5.log)), test5.default$default)
mean(as.character(unlist(pred5.log)) == test5.default$default)
# 97.66% accurate


#5d)
set.seed(6)
data6 <- Default %>%
  mutate(bin_stud = ifelse(student == "Yes", 1, 0)) %>%
  initial_split(prop = 0.5)
train6.default <- training(data6)
test6.default <- testing(data6)

fit6.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance + bin_stud, data = train6.default)
pred6.log <- predict(fit6.log, test6.default) 

table(as.character(unlist(pred6.log)), test6.default$default)
mean(as.character(unlist(pred6.log)) == test6.default$default)
# 97.2% accuracy
# adding a dummy variable for student decreased


#6a)
set.seed(7)
fit7.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(default ~ income + balance, data = Default)
summary(fit7.log$fit)

#6b)
boot.fn <- function(d_set, obs_index){
  reg <- glm(default ~ income + balance, 
             data = d_set, 
             subset = obs_index,
             family = "binomial")
  return(coef(reg)[-1])
}
boot.fn(Default, 1:length(Default$default))

#6c)
boot(Default, boot.fn, 1000)

#6d)
# The estimated standard errors are very close between the two methods


#7a)
fit8.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Direction ~ Lag1 + Lag2, data = Weekly)

#7b)
fit9.log <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Direction ~ Lag1 + Lag2, data = Weekly[-1,])

#7c)
predict(fit9.log, Weekly[1,])
# this was not predicted correctly

#7d)
check_pred <- vector(length = length(Weekly$Direction))
for(i in 1:length(Weekly$Direction)) {
  fit <- logistic_reg() %>%
    set_engine("glm") %>%
    fit(Direction ~ Lag1 + Lag2, data = Weekly[-i,])
  
  check_pred[i] <- unlist(predict(fit, Weekly[i,])) != Weekly$Direction[i]
  
  
  # check if prediction is correct
  
}

#7e)
mean(check_pred)
# We got an error rate of 45%


#8a)
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
# n is 100 observations
# p is 2
# y = x - 2x^2 + e

#8b)
ggplot(data = NULL, aes(x = x, y = y)) + 
  geom_point() +
  theme_bw()
# appears to have a quadratic relationship. Upside down U

#8c)
set.seed(1)
df <- tibble(y = y, x = x) %>%
  mutate(x2 = x^2,
         x3 = x^3,
         x4 = x^4)
fit10.lm <- glm(y ~ x, data = df)
err1.cv <- cv.glm(df, fit10.lm)
err1.cv$delta
# 7.288162 7.284744

set.seed(1)
fit11.lm <- glm(y ~ x + x2, data = df)
err2.cv <- cv.glm(df, fit11.lm)
err2.cv$delta
# 0.9374236 0.9371789

set.seed(1)
fit12.lm <- glm(y ~ x + x2 + x3, data = df)
err3.cv <- cv.glm(df, fit12.lm)
err3.cv$delta
# 0.9566218 0.9562538

set.seed(1)
fit13.lm <- glm(y ~ x + x2 + x3 + x4, data = df)
err4.cv <- cv.glm(df, fit13.lm)
err4.cv$delta
# 0.9539049 0.9534453

#8d)
set.seed(2)
df <- tibble(y = y, x = x) %>%
  mutate(x2 = x^2,
         x3 = x^3,
         x4 = x^4)
fit14.lm <- glm(y ~ x, data = df)
err5.cv <- cv.glm(df, fit14.lm)
err5.cv$delta
# 7.288162 7.284744

set.seed(2)
fit15.lm <- glm(y ~ x + x2, data = df)
err6.cv <- cv.glm(df, fit15.lm)
err6.cv$delta
# 0.9374236 0.9371789

set.seed(2)
fit16.lm <- glm(y ~ x + x2 + x3, data = df)
err7.cv <- cv.glm(df, fit16.lm)
err7.cv$delta
# 0.9566218 0.9562538

set.seed(2)
fit17.lm <- glm(y ~ x + x2 + x3 + x4, data = df)
err8.cv <- cv.glm(df, fit17.lm)
err8.cv$delta
# 0.9539049 0.9534453

# the results are identical since LOOCV tests every observation

#8e)
# the quadratic model had the lowest LOOCV error, this makes sense since the
#     original dependent variable was created using a quadratic function

#8f)
summary(fit14.lm)
summary(fit15.lm)
summary(fit16.lm)
summary(fit17.lm)
# these results agree with the conclusions drawn from the cross-validation results
#   in that only x and x^2 are SS at a 5% level. Also the AIC is lowest in the quadratic


#9a)
data(Boston)
mu.hat <- mean(Boston$medv)

#9b)
se.mu.hat <- sd(Boston$medv) / sqrt(length(Boston$medv))
# 0.40886

#9c)
set.seed(1)
boot.fn <- function(d_set, obs_index){
  return(mean(d_set[obs_index]))
}
boot.bos <- boot(Boston$medv, boot.fn, 1000)
boot.bos
# 0.41066 which is very close to the SE i got in (9b)

#9d)
low_CI  <- boot.bos$t0 - 2 * 0.41066
high_CI <- boot.bos$t0 + 2 * 0.41066
t.test(Boston$medv)
# These two results are extremely close to each other

#9e)
med.medv <- median(Boston$medv)

#9f)
set.seed(1)
boot.fn <- function(d_set, obs_index){
  return(median(d_set[obs_index]))
}
boot(Boston$medv, boot.fn, 1000)
# 0.3778075 which is relatively close to the SE of the mean

#9g)
quantile(Boston$medv, 0.1)
# 12.75

#9h)
set.seed(1)
boot.fn <- function(d_set, obs_index){
  return(quantile(d_set[obs_index],0.1))
}
boot(Boston$medv, boot.fn, 1000)
# 0.47675 
# this is small relative to the value of the quantile, but slightly larger than the
#   SE of the mean and median
