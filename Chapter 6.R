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
# 