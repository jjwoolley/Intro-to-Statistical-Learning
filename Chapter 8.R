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
# in this case, pruning does not appear to have a large impact on test mse
# in some cases it is better, in some it is worse


#8d)
fit.8d.bagg <- randomForest(Sales ~., mtry = 10, 
                            data = carseats.train,
                            importance = T,
                            ntree = 500)
pred.8d.bagg <- predict(fit.8d.bagg, carseats.test)

mse.8d <- mean((pred.8d.bagg - carseats.test$Sales)^2)
# bagging decreased the test mse by about 40%
importance(fit.8d.bagg)
# Price and ShelveLoc appear to be the most important vars


#8e)











