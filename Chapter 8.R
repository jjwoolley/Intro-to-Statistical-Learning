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
library("tree")
library("randomForest")
library("gbm")
library("glmnet")


### CONCEPTUAL------------------------------------------------------------------

#1)
# done on paper

#2)
# It is mentioned in Section 8.2.3 that boosting using depth-one trees (or stumps) 
# leads to an additive model: that is, a model of the form
# f(X) =  SUM[  fj(Xj) ]
# Explain why this is the case. You can begin with (8.12) in Algorithm 8.2.

# ri <- ri - lambda f(xi)
# 


#3)
Pm <- seq(from = 0, to = 1, length.out = 10000)
class_error <- 1 - pmax(Pm, 1 - Pm)
gini <- 2 * Pm * (1 - Pm) 
entropy <- - (Pm * log(Pm) + (1 - Pm) * log(1 - Pm))

ggplot(data = NULL, aes(x = Pm, y = class_error)) +
  geom_line(color = "red") +
  geom_line(aes(x = Pm, y = gini), color = "blue") +
  geom_line(aes(x = Pm, y = entropy), color = "springgreen4") +
  ylab("Pm Index") +
  theme_classic()

#4)
# done on paper

#5)
# Majority vote: classified as Class is Red
# Average probability
mean(c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75))
# since the mean is < 0.5, Classified as Class is Green

#6)
# Regression tree algorithm
# 1: Create large tree using taining data, and stop when all terminal nodes have fewer than
# certain number of observations
# 2: Prune the large tree to obtain sequence of subtrees as function of alpha
# 3: Use k-fold cross validation to choose optimal alpha and return the corresponding subtree


### APPLIED---------------------------------------------------------------------

#7)

# In the lab, we applied random forests to the Boston data using mtry=6
# and using ntree=25 and ntree=500. Create a plot displaying the test
# error resulting from random forests on this data set for a more comprehensive
# range of values for mtry and ntree. You can model your
# plot after Figure 8.10. Describe the results obtained
data("Boston")
boston.split <- initial_split(Boston, prop = 1/2)
boston.train <- training(boston.split)
boston.test <- testing(boston.split)

p.7 <- 13
p2.7 <- p.7/2
psq.7 <- sqrt(p.7)

set.seed(1)
fit.7.rf.p <- randomForest(boston.train[, -14],boston.train[, 14], 
                           xtest = boston.test[,-14], ytest = boston.test[, 14], 
                           mtry = p.7, ntree = 500)

fit.7.rf.p2 <- randomForest(boston.train[, -14],boston.train[, 14], 
                            xtest = boston.test[, -14], ytest = boston.test[, 14], 
                            mtry = p2.7, ntree = 500)

fit.7.rf.psq <- randomForest(boston.train[ , -14],boston.train[, 14], 
                             xtest = boston.test[, -14], ytest = boston.test[, 14], 
                             mtry = psq.7, ntree = 500)

ggplot(data = NULL, aes(x = 1:500, y = fit.7.rf.p$test$mse)) +
  geom_line(color = "red") + 
  geom_line(data = NULL, aes(x = 1:500, y = fit.7.rf.p2$test$mse), color = "blue") +
  geom_line(data = NULL, aes(x = 1:500, y = fit.7.rf.psq$test$mse), color = "springgreen4") +
  theme_bw() +
  ylab("MSE") +
  xlab("Trees") +
  ylim(8, 14)


#8a)
data("Carseats")
carseats.split <- initial_split(Carseats, prop = 1/2)
carseats.train <- training(carseats.split)
carseats.test <- testing(carseats.split)

#8b)
fit.8b.tree <- tree(Sales ~., data = carseats.train)

plot(fit.8b.tree)
text(fit.8b.tree, pretty = 0)

pred.8b.tree <- predict(fit.8b.tree, newdata = carseats.test)
mse.8b <- mean((pred.8b.tree - carseats.test$Sales)^2)
mse.8b

#8c)
cv.carseats <- cv.tree(fit.8b.tree, FUN = prune.tree)
plot(cv.carseats)
plot(cv.carseats$k, cv.carseats$dev, type = "b")

#deviation min appears to be at k = 6

prune.8c <- prune.tree(fit.8b.tree, best = 14)
pred.8c.prune <- predict(prune.8c, carseats.test)
mse.8c <- mean((pred.8c.prune - carseats.test$Sales)^2)
mse.8c
# in this case, pruning does not appear to have a large impact on test mse
# in some cases it is better, in some it is worse


#8d)
fit.8d.bagg <- randomForest(Sales ~., mtry = 10, 
                            data = carseats.train,
                            importance = T,
                            ntree = 500)
pred.8d.bagg <- predict(fit.8d.bagg, carseats.test)

mse.8d <- mean((pred.8d.bagg - carseats.test$Sales)^2)
mse.8d
# bagging decreased the test mse by about 40%
importance(fit.8d.bagg)
# Price and ShelveLoc appear to be the most important vars


#8e)
fit.8e.rf <- randomForest(Sales~., mtry = 3, data = carseats.train,
                          importance = T,
                          ntree = 500)
pred.8e.rf <- predict(fit.8e.rf, carseats.test)
mse.8e <- mean((pred.8e.rf - carseats.test$Sales)^2)
mse.8e
# the mse is larger than from 8d, but less than from 8b
importance(fit.8e.rf)
# Price and ShelveLoc are still the most important


#9a)
data("OJ")
oj.split <- initial_split(OJ, prop = 800/1070)
oj.train <- training(oj.split)
oj.test <- testing(oj.split)

#9b)

fit.9b.tree <- tree(Purchase ~., data = oj.train)
summary(fit.9b.tree)
# it uses the vars LoyalCH, PriceDiff, and ListPriceDiff to construct the model
# there is a training error of 14%
# there are 8 terminal nodes

#9c)
fit.9b.tree


#9d)
plot(fit.9b.tree)
text(fit.9b.tree, pretty = 0)
# LoyalCH divides individuals in the first two branches
# individuals with low CH loyalty appear to be more likely to select MM


#9e)
pred.9e.tree <- predict(fit.9b.tree, oj.test, type = "class")
table(oj.test$Purchase, pred.9e.tree)
err.9e <- 1 - mean(pred.9e.tree == oj.test$Purchase)
err.9e
summary(fit.9b.tree)
# 19% test error rate
# 17% training error rate

#9f)
cv.9f.tree <- cv.tree(fit.9b.tree, FUN = prune.tree, K = 10)
cv.9f.tree

#9g)
plot(cv.9f.tree)


#9h)
# optimal tree size is when dev is the smallest which corresponds to size = 5


#9i)
prune.9i.tree <- prune.tree(fit.9b.tree, best = 5)


#9j)
summary(prune.9i.tree)
# training error rate of 18%
# about the same as the original model


#9k)
pred.9k.prune <- predict(prune.9i.tree, oj.test, type = "class")
err.9k <- 1 - mean(pred.9k.prune == oj.test$Purchase)
err.9k
# test error of 20%
# this is higher than the original model (unpruned)
plot(prune.9i.tree)
text(prune.9i.tree, pretty = 0)


#10a)
data(Hitters)
Hitters <- Hitters %>%
  drop_na(Salary) %>%
  mutate(Salary_log = log(Salary)) %>%
  select(-Salary)

#10b)
hitters.train <- Hitters[1:200,]
hitters.test <- Hitters[-c(1:200),]

#10c)

shrink.test <- seq(0.001, 0.6, length.out = 50)
mse.train <- rep(NA, length(shrink.test))
mse.test <- rep(NA, length(shrink.test))

set.seed(1)

for(i in 1:length(shrink.test)) {
  fit <- gbm(Salary_log ~., 
            data = hitters.train,
            distribution = "gaussian",
            n.trees = 1000,
            shrinkage = shrink.test[i])
  t.mse.shrink[i] <- fit$train.error[1000]
  p.train <- predict(fit, hitters.train, n.trees = 1000)
  mse.train[i] <- mean((p.train - hitters.train$Salary_log)^2)
  # t == t.mse.shrink
  
  p.test <- predict(fit, hitters.test)
  mse.test[i] <- mean((p.test - hitters.test$Salary_log)^2)
}

ggplot(data = NULL, aes(x = shrink.test, y = mse.train)) +
  geom_line() +
  xlab("shrinkage value") +
  ylab("training mse") + 
  theme_bw()

# this tells that the minimum training mse was .2%
min(mse.train)
shrink.test[which.min(mse.train)]

#this gives us a minimum test mse of 25%
min(mse.test)
shrink.test[which.min(mse.test)]

#10d)
ggplot(data = NULL, aes(x = shrink.test, y = mse.test)) +
  geom_line() +
  xlab("shrinkage value") +
  ylab("testing mse") + 
  theme_bw() +
  ylim(0.2, 0.6)


#10e)
# linear regression
fit.10e.lm <- lm(Salary_log ~., data = hitters.train)
pred.10e.lm <- predict(fit.10e.lm, hitters.test)
mse.10e.lm <- mean((pred.10e.lm - hitters.test$Salary_log)^2)
mse.10e.lm
# test mse of 49%

# lasso regression
mat.10e <- model.matrix(Salary_log ~., hitters.train)[,-1]
fit.10e.lasso <- cv.glmnet(x = mat.10e, y = hitters.train$Salary_log, alpha = 1)
lambda.best = fit.10e.lasso$lambda.min
lambda.best

mat.10e.test <- model.matrix(Salary_log ~., hitters.test)[,-1]
pred.10e.lasso <- predict(fit.10e.lasso, s = lambda.best, newx = mat.10e.test)[,1]
mse.10e.lasso <- mean((pred.10e.lasso - hitters.test$Salary_log)^2)
mse.10e.lasso
# test mse of 47%


#10f)
fit.10f.best <- gbm(Salary_log ~., data = hitters.train, distribution = "gaussian",
                    n.trees = 1000, shrinkage = shrink.test[which.min(mse.test)])
summary(fit.10f.best)
# Runs, at bats, and hits seem to be the most important 


























