library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(tidyverse)
library(na.action =na.warn)
library(ggplot2)

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
tournData23 <- read.csv('2023 Tournament Data.csv')
trendData <- read.csv('Tournament Trends.csv')
saveRDS(trendData,'Tournament Game Data.rds')
#clean data
table1 <- tournGameDataClean %>%
  select(YEAR,TEAM,SEED,TEAM.ROUND, FREE.THROW..,)
tournGameData23 <- table1%>%
  filter(YEAR == 2023)%>%
  select(TEAM,SEED,FREE.THROW..,)
tournGameData23 <- tournGameData23[!duplicated(tournGameData23), ]
tournGameData23$SEED[tournGameData23$TEAM == "Maryland"] <- 8
tournGameData23$SEED[tournGameData23$TEAM == "Furman"] <- 13
tournGameData23$SEED[tournGameData23$TEAM == "Baylor"] <- 3
tournGameData23$SEED[tournGameData23$TEAM == "Missouri"] <- 7
tournGameData23$SEED[tournGameData23$TEAM == "Duke"] <- 5
tournGameData23$SEED[tournGameData23$TEAM == "Tennessee"] <- 4
tournGameData23$SEED[tournGameData23$TEAM == "Marquette"] <- 2
tournGameData23$SEED[tournGameData23$TEAM == "Auburn"] <- 9
tournGameData23$SEED[tournGameData23$TEAM == "Pittsburgh"] <- 11
tournGameData23$SEED[tournGameData23$TEAM == "Xavier"] <- 3
tournGameData23$SEED[tournGameData23$TEAM == "TCU"] <- 6
tournGameData23$SEED[tournGameData23$TEAM == "Northwestern"] <- 7
tournGameData23$SEED[tournGameData23$TEAM == "West Virginia"] <- 9
tournGameData23$SEED[tournGameData23$TEAM == "North Carolina State"] <- 11
tournGameData23$SEED[tournGameData23$TEAM == "Memphis"] <- 8
tournGameData23$SEED[tournGameData23$TEAM == "Providence"] <- 11
tournGameData23$SEED[tournGameData23$TEAM == "USC"] <- 10
tournGameData23$SEED[tournGameData23$TEAM == "Illinois"] <- 9
tournGameData23$SEED[tournGameData23$TEAM == "Iona"] <- 13
tournGameData23$SEED[tournGameData23$TEAM == "Nevada"] <- 11
tournGameData23$SEED[tournGameData23$TEAM == "NC-Asheville"] <- 15
table2 <- table1%>%
  filter(SEED<7, TEAM.ROUND>10)
table3 <- trendData%>%
  select(TEAM,YEAR, NEUTRAL.WIN..)%>%
  group_by(YEAR)
table3<-table3[complete.cases(table3), ]
table4 <- tournData23%>%
  select(TEAM, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., X3PT..)

mergedData <- merge(tournGameData23, table4, by = "TEAM", all.x = TRUE)
#Correcting the data to match the tournament accurately
mergedData <- mergedData %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, FREE.THROW.. = 71.5, X2PT.. = 53.6, X3PT.. = 33.4)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, FREE.THROW.. = 73.9, X2PT.. = 53.1, X3PT.. = 38.7)

upsetReasoning1 <- mergedData%>%
  dplyr::filter(TEAM == "Furman" | TEAM == "Virginia")

upsetReasoning2 <- mergedData%>%
  dplyr::filter(TEAM == "Michigan State" | TEAM == "Marquette")

#make this example reproducible
set.seed(1)


#use 70% of dataset as training set and 30% as test set 
samp= sample(1:nrow(table4), size = round(0.7*nrow(table4)),replace=FALSE)
train<-table4[samp,]
test<-table4[-samp,]


#Building a model

ggplot(table4,aes(NEUTRAL.WIN..,FREE.THROW..))+
  geom_point()

models<-tibble(a1 = runif(250,-20,40),
               a2=runif(250,-5,5)
               )

ggplot(table4,mapping=aes(x=NEUTRAL.WIN..,y=FREE.THROW..))+
         geom_abline(
           aes(intercept=a1, slope = a2),
           data= models, alpha = 1/4
         )+
         geom_point()
       
model1 <-function(a,data){
  a[1]+data$x*a[2]
}
model1(c(7,1.5),table4)

measure_distance<-function(mod,data){
  diff<-data$y- model1(mod,data)
  sqrt(mean(diff^2))
}
measure_distance(c(7,1.5),table4)

table4_dist<-function(a1,a2){
  measure_distance(c(a1,a2),table4)
}
models<-models%>%
  mutate(dist = purr::map2_dbl(a1, a2,table4_dist ))
       
# Train the model
model <- lm(SEED ~ DEFENSIVE.REBOUND.. + OFFENSIVE.REBOUND.., data = train)

# Evaluate the model # linear regression 
predictions <- predict(model, newdata = test)
rmse <- RMSE(predictions, test$DEFENSIVE.REBOUND..)
rsquared <- R2(predictions,test$DEFENSIVE.REBOUND.. )


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

