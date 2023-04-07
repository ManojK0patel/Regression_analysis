library(MPV)  #Library where all data sets from the book are stored
data(table.b1) 
attach(table.b1)
#(a) 

#y=b_0+b_1x_2+b_2x_7+b_3x_8 The Required model for part (a)
y.lm <- lm(y ~ x2 + x7 + x8)
summary(y.lm)
coef(y.lm) # Coefficient in the model

#(b)
y.null <- lm(y ~ 1)   #y=Beta_0  Null model
summary(y.null)
anova(y.null, y.lm)  # Anova Table for significance of the model
#(c)
#Hypothesis H_0:Beta_2=0
y2.lm <- lm(y ~ x7 + x8)
summary(y2.lm)
anova(y2.lm, y.lm)

#Hypothesis H_0:Beta_7=0
y7.lm <- lm(y ~ x2 + x8)
summary(y7.lm)
anova(y7.lm, y.lm)

#Hypothesis H_0:Beta_8=0
y8.lm <- lm(y ~ x2 + x7)
summary(y8.lm)
anova(y8.lm, y.lm)
detach(table.b1)
