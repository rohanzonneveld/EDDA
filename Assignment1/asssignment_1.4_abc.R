library(ggplot2)
library(readr)
library(gridExtra)
library(viridis)
library(reshape2)   
library(dplyr)
library(MASS)


#View(npk)

# 4. a
a <- c(1,1,0,0) # apply soil additive twice per block
soil_df <- data.frame(block = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4)), 
                      plot = c(1:24), # sample randomizes application per block
                      N = c(sample(a),sample(a),sample(a),sample(a),sample(a),sample(a)), 
                      P = c(sample(a),sample(a),sample(a),sample(a),sample(a),sample(a)),
                      K = c(sample(a),sample(a),sample(a),sample(a),sample(a),sample(a)))

#4. b

# It appears that, for some blocks, the yield is higher with nitrogen and for other blocks without nitrogen. 
# The mean yield for the plots with nitrogen is higher (57.68 vs 52.07). However, the yield per block varies widely. 
# For some blocks the yield is higher with, and for some without nitrogen. 
# It is therefore important to take block into account, as these results indicate that applying nitrogen will not always lead to a higher yield. 

npk_N <- npk[npk$N == 1,]
npk_NN <- npk[npk$N == 0,]

N_bxplt <- ggplot(npk_N, aes( y=yield, group = block)) + geom_boxplot(color="seagreen", fill="lightgreen") + labs(title='with nitrogen') + theme_light()
NN_bxplt <- ggplot(npk_NN, aes( y=yield, group = block)) + geom_boxplot(color="seagreen", fill="lightgreen") + labs(title='without nitrogen') + theme_light()

grid.arrange(N_bxplt, NN_bxplt, ncol=2)

mean(npk_N$yield)
mean(npk_NN$yield)

# 4.c 
# The full-two way ANOVA shows a main effect for block and nitrogen, but no interaction effect between the two. 
# It was sensible to include block in this model, because, as the previous boxplots already implied, results vary per block. 
# The Friedman test is not applicable in this situation because there are two, not one observations per cell. 
# Specifically, in each block the condition nitrogen and no nitrogen appears twice. 

npk$block <- as.factor(npk$block)
npk$N <- as.factor(npk$N)
npk_lmm <- lm(yield ~ block + N, data=npk) # main effects
npk_lmi <- lm(yield ~ block * N, data=npk) # interaction effects

anova(npk_lmm) # main effect for both block and N
anova(npk_lmi) # no interaction effect 


