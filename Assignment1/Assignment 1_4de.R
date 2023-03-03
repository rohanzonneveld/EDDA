#Assigment 4:d
# The proportionality test suggests that there is no significant difference between 
# the sample proportions of the two groups, which means that there is no significant difference
# between the mean male birthweight and mean female birthweight. Hence, the expert's claim
# is false.

library(MASS)
data(npk)

full_model = lm(yield ~ N + P + K + block + N:P + N:K + P:K, data = npk)
summary(full_model)

# This model shows that there is only a significant effect of the use of nitrogen, as discussed
# in the previous exercise, but also for the use of block 3, which means that this block
# produces different yields than the other blocks, even when taking into account the 
# different uses of nitrogen, phosphorus and potassium. There are no effects for the use of
# phosphorus and potassium. There are also no interaction effects between nitrogen, phosphorus
# and potassium

# Assignment 4:e
full_model <- lmer(yield ~ N + (1|block), data=npk, REML = FALSE) 
partial_model <- lmer(yield ~ (1|block), data=npk, REML = FALSE) 

anova(partial_model,full_model) 
# The ANOVA shows that there is a main effect for Nitrogen using a mixed model, 
# with variable block modelled as a random effect
