### Examining Relations Among Variables #### 

## Correlation ##


# Types :
# Relationships with its own datapoints (past) - Autocorrelation
# Relation with others - Bivariate , multivariate
# Multivariate - partial , semi-partial , part-correlation 

# Dataset:
anx <- read.csv("D:/Mine/Outline/WD/D3.1/relation.csv")
str(anx)

View(anx)

anx1 <- anx[,-1]
round(cor(anx1),2)


# Description :
# Revision - no. of hours ;  
# Anxiety - out of 100 anxiety score (prior to examination) ; 
# marks - performance in the examination



# Approach to analyse:

## Data Visualisation
# Examining distrinution of data:

plot(anx$Anxiety, anx$Marks, xlab = "Anxiety", 
     ylab = "Score", col= c(10,30), pch=8, 
     main = "Score and Anxiety")

plot(anx$Revision,anx$Marks, xlab = "Revision", 
     ylab = "Score", col = c(10,20), pch =10,
     main = "Score and Revision")

boxplot(anx$Revision ~ anx$Sex, 
        main = "Revision Hours & Gender", 
        xlab = "Gender", ylab = "Revision Hours" )

boxplot(anx$Anxiety ~ anx$Sex,
        main = " Anxiety Score & Gender",
        xlab = "Gender", ylab = "Anxiety Scores",
        col = c(10,12))



boxplot(anx$Marks ~ anx$Sex, 
        main = "Performance and Gender",
        xlab = "Gender", ylab = "Marks",
        col = c(100,200))

coplot(Marks ~ Revision|factor(Sex),data = anx, 
       panel = panel.smooth)

coplot(Marks ~ Revision|cut(Anxiety,2),data = anx,
       panel = panel.smooth)



# 2. Descreptive Analysis

table(anx$Sex)

tapply(anx$Marks, anx$Sex,mean)

0
# 3. exmaming normality
# 

windows(10,10)
par(mfrow=c(3,1))

hist(anx$Marks,main = "Distribution of Marks")
hist(anx$Anxiety, main = "Distribution of Anxiety Score")
hist(anx$Revision, main = "Distribution of Revision Hours")

qqnorm(anx$Anxiety) # what does it mean?

anx1 <- anx[, -c(1,5)]
View(anx1)

round(cor(anx1),3)

# Estimation of Bivariate Correlation & Its strength


# t statistics = r(N - 2)^.5/((1-r^2)^0.5)


# z statistics = 1/2* log{(1+r)/(1-r)}, SE = 1 /(N-3)^.5


cor(anx$Marks, anx$Anxiety) # coeff = - 0.441 , R^2 = 0.1945
cor.test(anx$Marks, anx$Anxiety) # p-value < alpha , null hypothesis can not be accepted ??


cor(anx$Marks, anx$Revision) # coeff = - 0.3967 , R^2 = 0.1574
cor.test(anx$Marks, anx$Revision)# null hypothesis can not be accepted ?? 

cor(anx$Anxiety, anx$Revision) # coeff = -0.7092, R^2 = 0.5031 # 


# Gender - Correlation 


g1 <- subset(anx, anx$Sex == 1)
g2 <- subset(anx, anx$Sex == 2)

cor(g1$Marks, g1$Anxiety)
cor(g2$Marks, g2$Anxiety)

cor.test(g1$Marks, g1$Anxiety)
cor.test(g2$Marks, g2$Anxiety)


# z - score (g1) =  1/2 * log{(1+r)/(1-r)}

 log((1-.505705)/(1+.505705))*.5 # z1 = -0.5569
 log((1-.3813839)/(1+.3813839))*.5 # z2 = -0.4017

 # z statistics for groupwise comparison , z = (z1-z2)/sqrt(1/(N1-3) +1/(N2-3) 
 # N1 = no of observation of group1, N2 = no of observation of group2
  
 z = (-0.5569 +0.4017)/(1/(nrow(g1)-3)+1/(nrow(g2)-3))^0.5 # statistic for testing difference in correlation
 z

# For a two tailed test @ 5% - 2.5% each tail
 
 qnorm(.025) 
 qnorm(.975) # this is higher than the z value , can Null hypothesis be accepted 
 
 
 
 # Correlation - regression 

summary(lm(anx$Anxiety~anx$Revision))
summary(lm(anx$Marks~anx$Revision))
summary(lm(anx$Marks~anx$Anxiety))
summary(lm(anx$Marks~anx$Anxiety+anx$Revision)) # R^2 = 0.2087


# Semi-partial / part correlation - Partial Correlation #### 

# variance of exam performance explained by revision and anxiety 20.87%
# variance shared by performance and anxiety is 19.45%
# variance shared by performance and revision time 15.74%
# variance shared revision time and anxiety is 50.31%

# So, variance in marks uniquely shared by revision time = 20.87 - 19.45 = 1.42% 
# variance in marks uniquely shared by anxiety   = 20.87 - 15.74 = 5.14%
# variance in marks uniquely shared by anxiety & revision = 20.87% - 1.42% - 5.14% = 14.31%
# variance in marks not shared above variables (error) = 100% - 20.87% = 79.13%


# Now R^2 (marks & revision) = 26.42% , r = 5.14^.5 = 26.42%
# this r , is known as semi-partial or part correlation


# Partial Correlation
# Now one must mote that part correlation has been estimated on the total variance of the marks
# Now let us restrict the variation of marks for revision and error (unexplained)
# so the variance = 79.13 + 5.14 = 84.27
# thus the contribution of revision time = 5.14 / 84.27 = 6.09% 
# 6.09^ 0.5 is known Partial Correlation between marks and revision 


# Computation - Partial Correlaiton ####


library(ppcor)
library(MASS)

anx1 <- as.data.frame(cbind(anx$Marks,anx$Anxiety, anx$Revision))
pcor(anx1)


## Moderation & Mediation ####



# Now We are aware of the individual effects of specific variables.
# There can be "moderation" effets from two variables - "interaction effect"

agg <- read.csv("E:/Outline/WD/D3.1/moderation/aggression.csv")
str(agg)


# Dataset:
# the dataset wants explore the impact of the violent video games on agressive behaviour
# So it wants to explore the relation between the variables "Aggress" and "Vid_Game" (hours spent on games).
# It also captured Emaotional responsiveness of the respondents (CaUnTs - Causal Unemotional)



summary(agg$CaUnTs)

windows(10,10)

# Option -1 
coplot(agg$Aggress~agg$Vid_Game|agg$CaUnTs, main = "Aggression & Time on Video Games", 
     xlab = "Hours - Video games", ylab = "Agreesiveness", 
     data = agg, panel = panel.smooth, overlap = 0, number = 2)

# Option - 2

quantile(agg$CaUnTs, seq(0,1,.25))

coplot(agg$Aggress~agg$Vid_Game|cut(agg$CaUnTs,breaks = c(0,11,18,26,43)), xlab = "Hours - Video games", 
       ylab = "Agreesiveness", data = agg, panel = panel.smooth, overlap = .5)

agg[1:3,]
which(agg$CaUnTs==0)


plot(agg$Aggress~agg$Vid_Game, main = "Aggression & Time on Video Games", 
  xlab = "Hours - Video games", ylab = "Agreesiveness") 

abline(lm(agg$Aggress~agg$Vid_Game), lwd=3)
abline(lm(agg$Aggress~agg$Vid_Game, data = agg, 
          subset = agg$CaUnTs<=11), col="red")

abline(lm(agg$Aggress~agg$Vid_Game, data = agg, 
          subset = agg$CaUnTs > 11 & agg$CaUnTs<=18.6), 
       col="blue", lwd=2)

abline(lm(agg$Aggress~agg$Vid_Game, data = agg, 
          subset = agg$CaUnTs > 18.6 & agg$CaUnTs <= 26), 
       col="green", lwd=2.5)

abline(lm(agg$Aggress~agg$Vid_Game, data = agg, 
          subset = agg$CaUnTs > 26), 
       col="cyan4", lwd=2.5)

## Alternative Models

summary(lm(agg$Aggress~agg$Vid_Game))

summary(lm(agg$Aggress~agg$CaUnTs))

summary(lm(agg$Aggress~agg$Vid_Game+agg$CaUnTs))


# interaction variable

agg$interact = agg$Vid_Game*agg$CaUnTs

summary(lm(agg$Aggress~agg$Vid_Game+agg$CaUnTs+agg$interact))

summary(lm(agg$Aggress~agg$Vid_Game+agg$interact))


coplot(agg$Aggress~agg$Vid_Game|cut(agg$CaUnTs,4), data = agg, 
       panel = panel.smooth, overlap = 0)



## Moderation Continued ####

# Dataset - Disaster

d <- read.csv("E:/Outline/WD/D3.1/moderation/disaster.csv")
str(d)
# Frame = 1 -> drouht due to climate change , 0 -> drought due to natural cause

# Pertinent Descriptives

F0 = subset(d, d$frame==0)
F1 = subset(d, d$frame==1)

summary(F0)
summary(F1)


# Visual Exploration

boxplot(d$donate~d$frame)
boxplot(d$justify~d$frame)
boxplot(d$skeptic~d$frame)

# Regression Models

summary(lm(d$justify~d$frame)) # model - 1

summary(lm(d$justify~d$frame+d$skeptic)) # model - 2


summary(lm(d$justify~d$frame+d$skeptic+d$frame*d$skeptic)) # model -3
m3 <- lm(d$justify~d$frame+d$skeptic+d$frame*d$skeptic)
m3$

n_data<- as.data.frame(cbind(fit.values= m3$fitted.values,
                             frame=d$frame, skeptic=d$skeptic))

plot(n_data$skeptic,n_data$fit.values, ylim = c(2,5), 
     col=ifelse(n_data$frame==0, "red", "blue"), pch=20,
     xlab = "skepticism", ylab = "Justification",
     main = "Moderating Effects of Skepticism on Framing & Justification")

coplot(n_data~n_data$skeptic|n_data$frame)

# Probing Moderating Effect ##

library(probemod)

agg_mod = lm(agg$Aggress~agg$Vid_Game+agg$CaUnT+agg$interact, data = agg)

nd = as.data.frame(cbind(f_values= agg_mod$fitted.values,Video=agg$Vid_Game,
                         nature=agg$CaUnTs))

plot(nd$Video, nd$f_values,col=ifelse(nd$nature<=18,"red", "blue"), pch=20,
     xlab = "Video", ylab = "Aggression")

agg_mod$coefficients

mod_effect <- lm(agg$Vid_Game~agg$CaUnTs)
mod_effect$coefficients



library(probemod)

pickapoint(model = lm(agg$Aggress ~ agg$Vid_Game + agg$CaUnTs, data = agg), 
           dv='Aggress', iv= 'Vid_Game',mod = 'CaUnTs')



# Mediation ####

# In moderation,independent & interaction variables both have directs realtion with dependent relation.
# Interaction variable (moderating variable) inpacts the severity of the relation.

## In mediation, there are two effects - direct and indirect
## Direct effect - between dependent and independent
## Indirect effect - between dependent and indenpendent through mediator variables

mf <- read.csv("E:/Outline/WD/D3.1/mediation/pmi.csv")
str(mf)

# Dataset:
# The study is about respondents reaction to a news article, which predicts shortage of supply of sugar.
# Cond: an article to be published in the front page or interior pages 
# reaction: whether the respondent would like to buy the item
# pmi :  presumed media impact on the readers 



# Examining relationship:

summary(mf) # examine the mean of the variables , ignore import, gender and age


# Grouped Means - grouped on the basis on influencing variable

mf0 = subset(mf, mf$cond==0)
mf1 = subset(mf, mf$cond==1)

summary(mf0)
summary(mf1)


# Examining Relation

summary(lm(mf$reaction~mf$cond)) # total effect

summary(lm(mf$pmi~mf$cond))

summary(lm(mf$reaction~mf$cond+mf$pmi)) # indirect effect

m1 <- lm(mf$reaction~mf$cond+mf$pmi)



## Understanding the Effects ####

library(psych)

mediate("reaction","cond", m = "pmi", data = mf)

# examine diff b/w - pmi(0) & pmi(1)
# total effect (TE) = direct effect (DE) + indirect effect (IE)

TE = .495
IE = .48*.51 
IE

DE = TE - IE  
DE

# Total Effect - Another perspective

m1 <- lm(mf$reaction~mf$cond+mf$pmi)



m1$coefficients

d0 = m1$coefficients[1]+m1$coefficients[3]*mean(mf$pmi)+ m1$coefficients[2]*0
d0

d1 = m1$coefficients[1]+m1$coefficients[3]*mean(mf$pmi)+ m1$coefficients[2]*1
d1

d1 - d0 # what does it mean

mean(mf1$pmi) - mean(mf0$pmi)

tt = (d1 - d0) + m1$coefficients[3]*(mean(mf1$pmi)-mean(mf0$pmi))

tt # this is equal to total effect 


summary(m1)


# Another example - Mediation (Continuous var)

sts <- read.csv("E:/Outline/WD/D3.1/mediation/e_stress.csv")

str(sts)
names(sts)

# Datatset: mental stress among enterprenuers
# estress - mental stress
# ese - economic stress
# affect - impact on emotional stability (i.e. depression) 
# withdraw - disengage from entreprenurial activity

library("psych")

mediate("withdraw", "estress", "ese", "estress",m = "affect", data = sts)
