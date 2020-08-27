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

sd# this shows the three var model as the best
coef(select1.model, 3)
















