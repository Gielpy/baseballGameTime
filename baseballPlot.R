library(ggplot2)
library(bear)

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

test<-load_data('~/GitHub/baseballGameTime')

summarySE(test, measurevar='gameTime', groupvars=c('Year'))

#line plot of running average game time vs. game number
ggplot(test, aes(x=gameNumber, y=runningAvg, group=Year, color=Year))+
  geom_line()

#box plot of average game time by year
yearAvg<-summarySE(test, measurevar='gameTime', groupvars=c('Year'))
ggplot(test, aes(factor(Year), gameTime))+
  geom_boxplot()

#box plot of average game time by year by team
ggplot(test, aes(factor(Home), gameTime))+
  geom_boxplot()+
  facet_wrap('Year', nrow=8)

#box plot of average game time by league
summarySE(test, measurevar='gameTime', groupvars=c('League'))
ggplot(test, aes(factor(League), gameTime))+
  geom_boxplot()