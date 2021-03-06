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
library("pls")

##### CONCEPTUAL----------------------------------------------------------------

#1a)
# They could all be tied, but "best subset" will for sure be either tied for the
#   best or on its own as the best since it includes every combination of variables

#1b)
# It could be any of the models as best subset could overfit

#1c)
# (i)   True
# (ii)  True
# (iii) False
# (iv)  False
# (v)   False


#2a)
# (iii) True, it is less flexible and decreases variance at the price of an 
#             increase in bias

#2b)
# (iii) same as in 2a

#2c)
# (ii) True, this one is more flexible but has less bias as a tradeoff for an
#             increase in variance


#3a)
# (iv) increasing S givs us a more flexible model which will allow the model to 
#        fit the data closer

#3b)
# (ii) When s = 0, all betas are 0 so when betas are allowed to increase then the model
#       overall will start to fit better until it leads to overfitting

#3c)
# (iii) Variance will always increase as the model becomes more flexible

#3d)
# (iv) Bias will always decrease as the model becomes more flexible

#3e)
# (v) This is unrelated to S (it is constant)


#4a)
# (iii) As lambda increases, the model becomes more restrictive which reduces training RSS

#4b)
# (ii) As lambda increases, test RSS will decrease and then start to increase when it overfits

#4c)
# (iv) As lambda increases and goes to infinity, all betas will reduce to 0

#4d)
# (iii) Opposite of 4c

#4e)
# (v) This is unrelated to S (it is constant)


#5)
# Done on paper


#6a)
# p=1, x axis b1, choice of y1 and lambda >0
#plot prove 6.12 is solved by 6.14

lambda <- 2

B1 <- -10:10
y.point <- 3
y <- (y.point - B1)^2 + lambda * B1^2

beta.est <- y.point / (1 + lambda)

sixA <- tibble(cbind(y, B1, beta.est))


ggplot(data = sixA, aes(x = B1, y = y)) +
  geom_line() + 
  geom_point(data = sixA, 
             aes(x = beta.est, y = (y.point - beta.est)^2 + lambda * beta.est^2),
                           color = "red")

#6b)

B2 <- -10:10

y <- (y.point - B2)^2 + lambda * abs(B1)

beta.est <- ifelse(y.point > lambda / 2, y.point - lambda / 2,
                   ifelse(y.point < - lambda / 2, y.point + lambda / 2,
                          ifelse(abs(y.point) <= lambda / 2), 0, "error"))


sixB <- tibble(cbind(y, B2, beta.est))

ggplot(data = sixB, aes(x = B2, y = y)) + 
  geom_line() +
  geom_point(data = sixB, 
             aes(x = beta.est, y = (y.point - beta.est)^2 + lambda * abs(beta.est)),
             color = "red")

#7)
# Done on paper

#8a)
X <- rnorm(100)
e <- rnorm(100)

#8b)
Y <- 10 + 4 * X + 3 * X^2 + 7 * X^3 + e


#8c)
data1 <- data.frame(y = Y, x = X)
select1.model <- regsubsets(y ~ poly(x, 10, raw = T), 
                            nvmax = 10,
                            data = data1)
select1.summ <- summary(select1.model)
select1.summ$bic
# this shows the three var model as the best (although it is close)
select1.summ$adjr2
# this shows four var as the best
select1.summ$cp

data1.subs <- tibble(bic = select1.summ$bic,
                     adjr2 = select1.summ$adjr2,
                     cp = select1.summ$cp)
ggplot(data = data1.subs) +
  geom_line(data = data1.subs, aes(x = 1:10, y = bic), color = "red") + 
  geom_line(data = data1.subs, aes(x = 1:10, y = cp), color = "blue") +
  geom_line(data = data1.subs, aes(x = 1:10, y = adjr2 * 1000), color = "yellow")

# this shows the three var model as the best
coef(select1.model, 3)
# gave coefficients of 10.12, 3.89, 2.99, and 7.02 which is very close


#8d) 
# BACKWARD
select2.model <- regsubsets(y ~ poly(x, 10, raw = T), 
                            nvmax = 10,
                            data = data1,
                            method = "backward")
select2.summ <- summary(select2.model)
select2.summ$bic
# this shows the three var model as the best (although it is close)
select2.summ$adjr2
# this shows the three var as the best
select2.summ$cp
# shows three var the best
data2.subs <- tibble(bic = select2.summ$bic,
                     adjr2 = select2.summ$adjr2,
                     cp = select2.summ$cp)
ggplot(data = data2.subs) +
  geom_line(data = data2.subs, aes(x = 1:10, y = bic), color = "red") + 
  geom_line(data = data2.subs, aes(x = 1:10, y = cp), color = "blue") +
  geom_line(data = data2.subs, aes(x = 1:10, y = adjr2 * 1000), color = "yellow")

coef(select2.model, 3)



# FORWARD 
select3.model <- regsubsets(y ~ poly(x, 10, raw = T), 
                            nvmax = 10,
                            data = data1,
                            method = "forward")
select3.summ <- summary(select3.model)
select3.summ$bic
# this shows the three var model as the best (although it is close)
select3.summ$adjr2
# this shows the four var as the best
select3.summ$cp
# shows three var the best
data3.subs <- tibble(bic = select3.summ$bic,
                     adjr2 = select3.summ$adjr2,
                     cp = select3.summ$cp)
ggplot(data = data3.subs) +
  geom_line(data = data3.subs, aes(x = 1:10, y = bic), color = "red") + 
  geom_line(data = data3.subs, aes(x = 1:10, y = cp), color = "blue") +
  geom_line(data = data3.subs, aes(x = 1:10, y = adjr2 * 1000), color = "yellow")

coef(select3.model, 3)

# The forward and backward selection methods were very close to the best selection



#8e)

# Y <- 10 + 4 * X + 3 * X^2 + 7 * X^3 + e

xmat4 <- model.matrix(y ~ poly(x, 10, raw = T), data = data1)[, -1]

#run cv.glmnet with an alpha =1 to do lasso (alpha = 0 for ridge)
set.seed(1234)
mod.lasso = cv.glmnet(x = xmat4, y = Y, alpha = 1)
lambda.best = mod.lasso$lambda.min
lambda.best

plot(mod.lasso)

predict(mod.lasso, s = lambda.best, type = "coefficients")
# this lasso regression is close, but should not be selecting any x^4


#8f)

# best subset selection
X <- rnorm(100)
B0 <- 12
B7 <- 5
eps <- rnorm(100)
Y <- B0 + B7*X^7 + eps


data5 <- data.frame(y = Y, x = X)
select5.model <- regsubsets(y ~ poly(x, 10, raw = T),
                            nvmax = 10,
                            data = data5)
select5.summ <- summary(select5.model)
which.min(select5.summ$bic)
# this shows the one var model as the best with the X^7 var
which.max(select5.summ$adjr2)
# this shows two var as the best
which.min(select5.summ$cp)
# this shows the one var model as the best

data5.subs <- tibble(bic = select5.summ$bic,
                     adjr2 = select5.summ$adjr2,
                     cp = select5.summ$cp)
ggplot(data = data5.subs) +
  geom_line(data = data1.subs, aes(x = 1:10, y = bic), color = "red") + 
  geom_line(data = data1.subs, aes(x = 1:10, y = cp), color = "blue") +
  geom_line(data = data1.subs, aes(x = 1:10, y = adjr2 * 1000), color = "yellow")

# this shows the one var model as the best
coef(select5.model, 1)
# very close to our chosen values

# Lasso
xmat6 <- model.matrix(y ~ poly(x, 10, raw = T), data = data5)[, -1]

#run cv.glmnet with an alpha =1 to do lasso (alpha = 0 for ridge)
set.seed(1234)
mod6.lasso = cv.glmnet(x = xmat6, y = Y, alpha = 1)
lambda6.best = mod6.lasso$lambda.min
lambda6.best

plot(mod6.lasso)

predict(mod6.lasso, s = lambda6.best, type = "coefficients")
# this lasso regression is close, but not quite as close as best subset selection


#9a)

data("College")
College_data <- initial_split(College)

train.college <- training(College_data)
test.college <- testing(College_data)

#9b) LINEAR MODEL
fit7.lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Apps ~. , data = train.college)
tidy(fit7.lm)
lm7.pred <- predict(fit7.lm, test.college)

# get mean squared error
mse7 <- map((test.college[, "Apps"] - lm7.pred)^2, mean)
mse7
# mse = 1,844,524


#9c) RIDGE
train.ridge.college <- model.matrix(Apps ~., data = train.college)[,-1]
test.ridge.college <- model.matrix(Apps ~., data = test.college)[,-1]

fit8.ridge <- cv.glmnet(x = train.ridge.college, 
                        y = train.college$Apps,
                        alpha = 0)

lambda8.best <- fit8.ridge$lambda.min
pred8.ridge <- tibble(predict(fit8.ridge, s = lambda8.best, newx = test.ridge.college))
mse8 <- map((test.college$Apps - pred8.ridge)^2, mean)
mse8
# mse = 3,324,966


#9d) LASSO

fit9.lasso <- cv.glmnet(x = train.ridge.college, 
                        y = train.college$Apps,
                        alpha = 1)

lambda9.best <- fit9.lasso$lambda.min
pred9.lasso <- tibble(predict(fit9.lasso, s = lambda9.best, newx = test.ridge.college))
mse9 <- map((test.college$Apps - pred9.lasso)^2, mean)
mse9
# mse = 1,967,434

coef9.lasso <- predict(fit9.lasso, type = "coefficients", s = lambda9.best)
coef9.lasso
length(coef9.lasso[coef9.lasso != 0])
# 15 non-zero coefficients including the intercept


#9e) PCR
fit10.pcr <- pcr(Apps ~., data = train.college, scale = T, validation = "CV")
summary(fit10.pcr)
validationplot(fit10.pcr, val.type = "MSEP")
pred10.pcr <- tibble(predict(fit10.pcr, test.college, ncomp = 17))
mse10 <- map((test.college$Apps - pred10.pcr)^2, mean)


#9f) PLS
fit11.pls <- plsr(Apps ~., data = train.college, scale = T, validation = "CV")
summary(fit11.pls)
validationplot(fit11.pls, val.type = "MSEP")
pred11.pls <- tibble(predict(fit11.pls, test.college, ncomp = 9))
mse11 <- map((test.college$Apps - pred11.pls)^2, mean)

#9g)
# MSE's from each method
mse7  #lm
mse8  # ridge:  
mse9  # lasso:  
mse10 # PCR:     (same as lm since we use all M's/ncomp)
mse11 # PLS:    



#10a)
X <- matrix(rnorm(1000*20), 1000, 20)
eps <- rnorm(1000)
b <- rnorm(20)
b[2:5] <- 0
y <- X %*% b + eps

#10b)
data10 <- data.frame(y, X)
data10.split <- initial_split(data10, prop = 0.9)
data10.train <- training(data10.split)
data10.test <- testing(data10.split)

#10c) best subset selection on the training set
fit12.best <- regsubsets(y~., data10.train,nvmax = 20)
fit12.summ <- summary(fit12.best)
which.max(fit12.summ$adjr2)
which.min(fit12.summ$cp)
which.min(fit12.summ$bic)

train12.mat <- model.matrix(y ~., data10.train, nvmax = 20)
ts.results <- rep(NA, 20)
for(i in 1:20) {
  cf <- coef(fit12.best, id = i)
  pred <- train12.mat[, names(cf)] %*% cf
  ts.results[i] <- mean((pred - data10.train$y)^2)
}

ggplot(data = NULL, aes(x = 1:20, y = ts.results)) +
  geom_point() +
  geom_line() +
  xlab("Number of predictors") + 
  ylab("Mean Squared Error (MSE)") + 
  theme_bw()

#10d)
test12.mat <- model.matrix(y ~., data10.test, nvmax = 20)
test.mse <- rep(NA, 20) 
for(i in 1:20) {
  cf <- coef(fit12.best, id = i)
  pred <- test12.mat[, names(cf)] %*% cf
  test.mse[i] <- mean((pred - data10.test$y)^2)
}

ggplot(data = NULL, aes(x = 1:20, y = test.mse)) +
  geom_point() +
  geom_line() +
  xlab("Number of predictors") + 
  ylab("Mean Squared Error (MSE)") + 
  theme_bw()

#10e)
# the test mse is smallest in the 12 predictor model
# this is about where the training mse is close to its lowest and levels off

#10f)
coef(fit12.best, id = 12)
b
# they are very close, the model did a great job in estimating the coefficients

#10g)

err <- rep(NA, 20)
x_cols <- colnames(X, do.NULL = F, prefix = "X")
for (i in 1:20) {
  cf <- coef(fit12.best, id = i)
  err[i] <- sqrt(sum((b[x_cols %in% names(cf)] 
                      - cf[names(cf) %in% x_cols])^2)
                 + sum(b[!(x_cols %in% names(cf))])^2)
}
err

ggplot(data = NULL, aes(x = 1:20, y = err)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Nu. of Coefficients") +
  ylab("Error b/w estimated coefficients and true")


#11a)
# predict per capita crime rate (crim)
data(Boston)
Boston <- Boston %>%
  mutate(chas = factor(chas))
boston.split <- initial_split(Boston, prop = 0.5, strata = crim)
boston.train <- training(boston.split)
boston.test <- testing(boston.split)

# best subset selection
fit13.bss <- regsubsets(crim ~., data = boston.train, nvmax = 13)
fit13.summ <- summary(fit13.bss)


# code from textbook
predict.regsubsets <- function (object ,newdata ,id ,...){
   form <- as.formula(object$call[[2]])
   mat <- model.matrix(form, newdata )
   coefi <- coef(object, id = id)
   xvars <- names (coefi)
   mat[, xvars] %*% coefi
}

err.bss <- rep(NA, ncol(boston.train) - 1)
for(i in 1:(ncol(boston.test) - 1)) {
  pred.bss <- predict(fit13.bss, boston.test, id = i)
  err.bss[i] <- mean((boston.test$crim - pred.bss)^2)
}
err.bss


# ridge
x.train.ridge <- model.matrix(crim ~., data = boston.train)[,-1]
x.test.ridge <- model.matrix(crim ~., data = boston.test)[,-1]
fit14.ridge <- cv.glmnet(x = x.train.ridge, y = boston.train$crim, alpha = 0)
fit14.ridge$lambda.min

pred14.ridge <- predict(fit14.ridge, 
                        s = fit14.ridge$lambda.min, 
                        newx = x.test.ridge)
err.ridge <- mean((boston.test$crim - pred14.ridge)^2)
err.ridge

# lasso
fit15.lasso <- cv.glmnet(x = x.train.ridge, y = boston.train$crim, alpha = 1)
fit15.lasso$lambda.min

pred15.lasso <- predict(fit15.lasso, 
                        s = fit15.lasso$lambda.min, 
                        newx = x.test.ridge)
err.lasso <- mean((boston.test$crim - pred15.lasso)^2)
err.lasso

# pcr
fit16.pcr <- pcr(crim ~., data = boston.train, scale = T, validation = "CV")
summary(fit16.pcr)
validationplot(fit16.pcr, val.type = "MSEP")

pred16.pcr <- predict(fit16.pcr, boston.test, ncomp = 13)[1:252]
err.pcr <- mean((pred16.pcr - boston.test$crim)^2)

err.pcr

err.bss
min(err.bss)
err.ridge
err.lasso
err.pcr

#11b)
# I would use the lasso model because it consistently has the lowest mse
predict(fit15.lasso, s = fit15.lasso$lambda.min, type = "coefficients")

#11c)
# no, it does not include the variables nox, age, or tax


