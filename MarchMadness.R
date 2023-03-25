library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(tidyverse)
library(na.action =na.warn)
library(ggplot2)
library(corrplot)
library(purrr)
library(shiny)
library(DT)
library(rsconnect)

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
  select(TEAM,SEED)
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
  select(TEAM, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., X3PT.., WIN.., FREE.THROW..)

mergedData <- merge(tournGameData23, table4, by = "TEAM", all.x = TRUE)
#Correcting the data to match the tournament accurately
mergedData <- mergedData %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, X2PT.. = 53.6, X3PT.. = 33.4, WIN.. = 72.4, FREE.THROW.. = 71.5)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, X2PT.. = 53.1, X3PT.. = 38.7, WIN.. = 62.2, FREE.THROW.. = 73.9)

#mergedData <- cbind(mergedData, "AVERAGE")
#mergedData$`"AVERAGE"` <- rowMeans(mergedData$FREE.THROW.., mergedData$OFFENSIVE.REBOUND.., mergedData$DEFENSIVE.REBOUND.., mergedData$X2PT.., mergedData$X3PT..)
mergedData$Average <- rowSums(mergedData[,3:8])
#mergedData$Average <- mergedData$Average / 5
mergedData$REGION[mergedData$TEAM == "Alabama" | mergedData$TEAM == "Texas A&M-Corpus Christi" | mergedData$TEAM == "Maryland" | mergedData$TEAM == "West Virginia" |
                    mergedData$TEAM == "San Diego State" | mergedData$TEAM == "College of Charleston" | mergedData$TEAM == "Virginia" |
                    mergedData$TEAM == "Furman" | mergedData$TEAM == "Creighton" | mergedData$TEAM == "North Carolina State" | mergedData$TEAM == "Baylor" |
                    mergedData$TEAM == "Missouri" | mergedData$TEAM == "Utah State" | mergedData$TEAM == "Arizona" | mergedData$TEAM == "Princeton"] <- "South"
mergedData$REGION[mergedData$TEAM == "Purdue" | mergedData$TEAM == "Fairleigh Dickinson" | mergedData$TEAM == "Memphis" | mergedData$TEAM == "Florida Atlantic" |
                    mergedData$TEAM == "Duke" | mergedData$TEAM == "Oral Roberts" | mergedData$TEAM == "Tennessee" |
                    mergedData$TEAM == "Louisiana" | mergedData$TEAM == "Kentucky" | mergedData$TEAM == "Providence" | mergedData$TEAM == "Kansas State" |
                    mergedData$TEAM == "Montana State" | mergedData$TEAM == "Michigan State" | mergedData$TEAM == "USC" | mergedData$TEAM == "Marquette" | mergedData$TEAM == "Vermont"] <- "East"
mergedData$REGION[mergedData$TEAM == "Houston" | mergedData$TEAM == "Iowa" | mergedData$TEAM == "Auburn" | mergedData$TEAM == "Miami (FLA.)" |
                    mergedData$TEAM == "Indiana" | mergedData$TEAM == "Iowa State" | mergedData$TEAM == "Pittsburgh" |
                    mergedData$TEAM == "Xavier" | mergedData$TEAM == "Texas A&M" | mergedData$TEAM == "Penn State" | mergedData$TEAM == "Texas" |
                    mergedData$TEAM == "Colgate"] <- "Midwest"
mergedData$REGION[mergedData$TEAM == "Kansas" | mergedData$TEAM == "Howard" | mergedData$TEAM == "Arkansas" | mergedData$TEAM == "Illinois" |
                    mergedData$TEAM == "St. Mary's (CA)" | mergedData$TEAM == "VCU" | mergedData$TEAM == "Connecticut" |
                    mergedData$TEAM == "Iona" | mergedData$TEAM == "TCU" | mergedData$TEAM == "Arizona State" | mergedData$TEAM == "Gonzaga" |
                    mergedData$TEAM == "Northwestern" | mergedData$TEAM == "Boise State" | mergedData$TEAM == "UCLA" | mergedData$TEAM == "NC-Asheville" | mergedData$TEAM == "Nevada"] <- "West"


df_cortable<-mergedData%>%
  select(SEED, FREE.THROW.., OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., X3PT.., WIN..)
head(df_cortable)
B<-cor(df_cortable)
head(round(B,2))
corrplot(B, method="color")

table5 <- tournData23%>%
  select(TEAM, SEED, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X3PT.., WIN..)
table5 <- table5 %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, X3PT.. = 33.4, WIN.. = 72.4)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, X3PT.. = 38.7, WIN.. = 62.2)
table5$Average <- rowSums(table5[,3:6])
table5$Average <- table5$Average / 5

table6 <- tournData23%>%
  select(TEAM, SEED, OFFENSIVE.REBOUND..,, X3PT..)
table6 <- table6 %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, SEED = 15, X3PT.. = 33.4)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, SEED = 10, X3PT.. = 38.7)
table6$Average <- rowSums(table6[,3:4])
table6$Average <- table6$Average / 5

table7 <- tournData23%>%
  select(TEAM, SEED, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., WIN..)
table7 <- table7 %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, X2PT.. = 53.6, WIN.. = 72.4)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, X2PT.. = 53.1, WIN.. = 62.2)
table7$Average <- rowSums(table7[,3:6])
table7$Average <- table7$Average / 5


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

ggplot(mergedData,aes(SEED, WIN..,FREE.THROW..))+
  geom_point()

models<-tibble(a1 = runif(250,-20,40),
               a2=runif(250,-5,5)
               )

ggplot(mergedData,mapping=aes(x=WIN..,y=FREE.THROW..))+
         geom_abline(
           aes(intercept=a1, slope = a2),
           data= models, alpha = 1/4
         )+
         geom_point()
       
model1 <-function(a,data){
  a[1]+data$x*a[2]
}
model1(c(7,1.5),mergedData)

measure_distance<-function(mod,data){
  diff<-data$y- model1(mod,data)
  sqrt(mean(diff^2))
}
measure_distance(c(7,1.5),mergedData)

mergedData_dist<-function(a1,a2){
  measure_distance(c(a1,a2),mergedData)
}
models<-models%>%
  mutate(dist = purrr::map2_dbl(a1, a2,mergedData_dist ))
models      

ggplot(mergedData,aes(x=WIN..,y=FREE.THROW..))+
  geom_point(size = 2, color = "grey30")+
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )
ggplot(models,aes(a1,a2))+
  geom_point(
    data = filter(models,rank(dist)<= 10),
    size = 4, color = "red")+
  geom_point(aes(colour= -dist))

# Train the model
model <- lm(SEED ~ DEFENSIVE.REBOUND.. + OFFENSIVE.REBOUND.., data = train)

# Evaluate the model # linear regression 
predictions <- predict(model, newdata = test)
rmse <- RMSE(predictions, test$DEFENSIVE.REBOUND..)
rsquared <- R2(predictions,test$DEFENSIVE.REBOUND.. )


#Decision tree
Results <- rpart()
rpart.plot(Results, type=3, fallen.leaves=F, cex=.5 )


#shiny app



ui<-fluidPage( 
  
  titlePanel(title = "March Madness 2023"),
  
  fluidRow(
    column(2,
           selectInput('X', 'Team 1',mergedData$TEAM)),
    column(2,
           selectInput('Y', 'Team 2',mergedData$TEAM)),
           
    
    column(4,plotOutput('plot_01')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  ),
  
  

server<-function(input,output){
  
  output$plot_01 <- renderPlot({
    ggplot(mergedData, aes_string(x=input$X, y=input$Y, colour=input$Splitby))+ geom_col(aes(colour = factor(color)))
    
    
    
    
    
  })
  
  output$table_01<-DT::renderDataTable(mergedData[,c(input$X,input$Y,input$Splitby)],options = list(pageLength = 4))
}

shinyApp(ui=ui, server=server)





#stuff he wrote in class to call functions
df<-mtcars

region1 <- runCalc(df,23)
region2 <- runCalc(df,25)

runCalc <- function(df,x){
  df<- df %>%
    filter(mpg >x)
  return(df)
}

