library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)

rm(list=ls())


#Leandra's working directory
setwd("~/git_data/marchMadness23")
#Hildana's directory
setwd("~/Desktop/DATA-332/Project2")
#Mansi's directory
setwd("~/Desktop/DATA-331/Project2")


#Mar 15

tournGameData <- read.csv('Tournament Game Data.csv')
tournGameDataClean <- tournGameData[!duplicated(tournGameData), ]
trendData <- read.csv('Tournament Trends.csv')
saveRDS(game,'Tournament Game Data.rds')
#clean data
table1 <- tournGameDataClean %>%
  select(YEAR,TEAM,SEED,TEAM.ROUND,FREE.THROW..,)
table2 <- table1%>%
  filter(SEED>7, TEAM.ROUND>10)
table3 <- trendData%>%
  select(TEAM,YEAR, NEUTRAL.WIN..)
  group_by(YEAR)
table4 <- merge(x=table2, y=table3,
                  by= "TEAM", all.x = TRUE)
table4<-table4[complete.cases(table4), ]

freeThrowReasoning <- tournGameDataClean%>%
  dplyr::filter(TEAM == "Furman" | TEAM == "Virginia", YEAR == "2023")%>%
  select(YEAR, TEAM, FREE.THROW..)



#make this example reproducible
set.seed(1)


#use 70% of dataset as training set and 30% as test set 
samp= sample(1:nrow(iris), size = round(0.7*nrow(iris)),replace=FALSE)
train<-iris[samp,]
test<-iris[-samp,]










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

