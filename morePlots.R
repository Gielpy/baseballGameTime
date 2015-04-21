allYears<-read.csv("baseballTime.csv", header=T)
allYears<-subset(allYears, gameTime>'0')
without2015<-subset(allYears, Year<'2015')

library(ggplot2)

#plot average game time by year with SEM
library(bear)
without2015.summary<-summarySE(without2015, measurevar='gameTime', groupvars=c('Year'))
ggplot(without2015.summary, aes(x=Year, y=gameTime))+
  geom_line()+
  geom_errorbar(aes(ymin=gameTime-se, ymax=gameTime+se), width=0.2)+
  geom_point(size=5)+
  theme_classic(base_size=20, base_family="Helvetica")+
  ylab("Average Game Time (minutes)")+
  theme(legend.position="none")+
  scale_y_continuous(limits=c(170, 190), breaks=seq(160,200, by=5))


#plot running average for each year to look for stabilization
ggplot(without2015, aes(x=gameNumber, y=runningAvg, group=Year))+
  geom_line(aes(color=Year), size=0.75)+
  scale_color_gradient(low="red")+
  theme_classic(base_size=20, base_family="Helvetica")+
  ylab("Average Game Time (minutes)")+
  xlab("Game Number of Season")+
  xlim(0,300)+
  scale_y_continuous(limits=c(125,250), breaks=seq(0,300, by=25))

#box and whiskers by year, by league
ggplot(without2015, aes(x=as.factor(Year), y=gameTime))+
  geom_boxplot(aes(fill=League))+
  theme_classic(base_size=20, base_family="Helvetica")+
  ylab("Average Game Time (minutes)")+
  xlab("Year")


#box and whiskers by year, by team
ggplot(without2015, aes(x=as.factor(Year), y=gameTime, group=as.factor(Year)))+
  geom_boxplot(aes(fill=Year))+
  scale_fill_gradient(low="red")+
  theme_classic(base_size=20, base_family="Helvetica")+
  ylab("Average Game Time (minutes)")+
  xlab("Year")+
  facet_wrap(~Home, nrow=7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))