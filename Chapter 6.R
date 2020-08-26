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
# 

#3b)
#

#3c)
#

#3d)
#

#3e)
#