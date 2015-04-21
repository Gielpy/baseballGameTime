library(pgirmess)

#anova to determine effects of year, league, and home team
gameTime.anova<- aov(gameTime~as.factor(Year)*League*Home, data=without2015)
summary(gameTime.anova)
TukeyHSDs(TukeyHSD(gameTime.anova, "Home"))