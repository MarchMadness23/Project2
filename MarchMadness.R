library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(purrr)
library(shiny)
library(DT)
library(rsconnect)
library(tidyverse)

rm(list=ls())


#Mar 15

#Reading the files into data frames
tournGameData <- read.csv('data/Tournament Game Data.csv')
tournGameDataClean <- tournGameData[!duplicated(tournGameData), ]
tournData23 <- read.csv('data/2023 Tournament Data.csv')
trendData <- read.csv('data/Tournament Trends.csv')
saveRDS(trendData,'data/Tournament Game Data.rds')

#clean data and putting it into tables
table1 <- tournGameDataClean %>%
  select(YEAR,TEAM,SEED,TEAM.ROUND, FREE.THROW..)
tournGameData23 <- table1%>%
  filter(YEAR == 2023)%>%
  select(TEAM,SEED)

#removing duplicates from the table
tournGameData23 <- tournGameData23[!duplicated(tournGameData23), ]

#correcting the seeds for teams where it was wrong
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

#filtering the seed and rounds
table2 <- table1%>%
  filter(SEED<7, TEAM.ROUND>10)

#creating more tables and removing non complete cases
table3 <- trendData%>%
  select(TEAM,YEAR, NEUTRAL.WIN..)%>%
  group_by(YEAR)
table3<-table3[complete.cases(table3), ]
table4 <- tournData23%>%
  select(TEAM, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., X3PT.., WIN.., FREE.THROW..)

#merging the data into one table
mergedData <- merge(tournGameData23, table4, by = "TEAM", all.x = TRUE)
#Correcting the data to match the tournament accurately
mergedData <- mergedData %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, X2PT.. = 53.6, X3PT.. = 33.4, WIN.. = 72.4, FREE.THROW.. = 71.5)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, X2PT.. = 53.1, X3PT.. = 38.7, WIN.. = 62.2, FREE.THROW.. = 73.9)

#creating a new column to predict based off of
mergedData$Sum <- rowSums(mergedData[,3:8])

#adding the regions to the table
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


#creating a correlation model
df_cortable<-mergedData%>%
  select(SEED, FREE.THROW.., OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X2PT.., X3PT.., WIN..)
head(df_cortable)
B<-cor(df_cortable)
head(round(B,2))
corrplot(B, method="color")

#testing out which variables give us the most ideal outcome
table5 <- tournData23%>%
  select(TEAM, SEED, OFFENSIVE.REBOUND.., DEFENSIVE.REBOUND.., X3PT.., WIN..)
table5 <- table5 %>%
  add_row(TEAM = 'Princeton', OFFENSIVE.REBOUND.. = 28.7, DEFENSIVE.REBOUND.. = 77.3, SEED = 15, X3PT.. = 33.4, WIN.. = 72.4)%>%
  add_row(TEAM = 'Penn State', OFFENSIVE.REBOUND.. = 19.2, DEFENSIVE.REBOUND.. = 74.4, SEED = 10, X3PT.. = 38.7, WIN.. = 62.2)
table5$Average <- rowSums(table5[,3:6])
table5$Average <- table5$Average / 5

table6 <- tournData23%>%
  select(TEAM, SEED, OFFENSIVE.REBOUND.., X3PT..)
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

#confirming our variables help predict upsets
upsetReasoning1 <- mergedData%>%
  dplyr::filter(TEAM == "Furman" | TEAM == "Virginia")

upsetReasoning2 <- mergedData%>%
  dplyr::filter(TEAM == "Michigan State" | TEAM == "Marquette")

#make this example reproducible
set.seed(1)


#use 70% of dataset as training set and 30% as test set
samp= sample(1:nrow(table7), size = round(0.7*nrow(table7)),replace=FALSE)
train<-table7[samp,]
test<-table7[-samp,]




#shiny app

ui <- fluidPage(
  titlePanel("March Madness Win Percentage"),
  sidebarPanel(
    sidebarLayout(
      selectInput("team1", "Select Team 1", choices = unique(mergedData$TEAM)),
      selectInput("team2", "Select Team 2", choices = unique(mergedData$TEAM)),
      
    )),
  mainPanel(
    plotOutput("winplot")
  )
  
)

server <- function(input, output) {
  selected_teams <- reactive({
    mergedData %>%
      filter(TEAM %in% c(input$team1, input$team2))
  })
  output$winplot <- renderPlot({
    ggplot(selected_teams(), aes(x = TEAM , y = Average, fill = TEAM)) +
      geom_bar(stat = "identity") +
      labs(title = "Probability of winning ", x = "Team", y = "Calculated chances of winning ") +
      theme_bw() +
      theme(legend.position = "none")
  })
}


shinyApp(ui=ui, server=server)


