library(readxl)
library(dplyr)

#Leandra's working directory
setwd("~/git_data/marchMadness23")
#Hildana's directory
setwd("~/Desktop/DATA-332/Project2")


tournGameData <- read.csv('Tournament Game Data.csv')

gonzagaData <- tournGameData %>%
  dplyr::filter(TEAM == "Gonzaga")

#Mar 15
library(readxl)
library(rpart)
library(rpart.plot)
rm(list=ls())

setwd("~/Desktop/DATA-332/Project2")


gameEssentials <- read.csv('Tournament Game Data.csv')
saveRDS(game,'Tournament Game Data.rds')
#clean data
table1 <- gameEssentials %>%
  select(YEAR,TEAM,SEED,TEAM.ROUND,FREE.THROW..,)
table2 <- table1%>%
  filter(SEED>7, TEAM.ROUND>10)

#Decision tree
Results <- rpart()
rpart.plot(Results, type=3, fallen.leaves=F, cex=.5 )





#stuff he wrote in class to call functions
df<-mtcars

region1 <- runCalc(df,23)
region2 <- runCalc(df,25)

runCalc <- function(df,x){
  df<- df %>%
    filter(mpg >x)
  return(df)
}

