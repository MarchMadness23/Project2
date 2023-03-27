# March Madness

For this project we analysed data on the performance of teams on March madness since 2003. We cleaned, ran a correlation test, then created a new metric that had a high correlation to win percentage. Our goal was to come up with a good way to choose a winner for the tournament.


![19a42410dda651c98b107d53ef654d0d](https://user-images.githubusercontent.com/108307724/224206117-8971413f-8999-4c35-a383-d61730d24871.jpeg)

# Dictionary📝
We used information of the Tournament Game data and the colums used to predict the winner were:
1. TEAM - the team name 
2. SEED - the rank of the team 
3. YEAR - the year of their performace rate
4. TEAM.ROUND - the round number they got eliminated 
5. FREE.THROW.. - percentage of unopposed attempts to score points 
6. WIN - the win percentage of their regular season 
7. OFFENSIVE.REBOUND - percentage of the ball is recovered by the offensive side
8. DEFENSIVE.REBOUND - percentage of when defensive players gain possession of the ball after an offensive player misses the shot




# Data Cleaning🧹 
To clean the data, we first removed the duplicates in the dataset of Tournament Game Data.
```
tournGameDataClean <- tournGameData[!duplicated(tournGameData), ]
```
From the cleaned dataset, we made a table of the colums we were going to use to predict our winner.
```
table1 <- tournGameDataClean %>%
  select(YEAR,TEAM,SEED,TEAM.ROUND, FREE.THROW..,)
 ```
 
<img width="495" alt="Screen Shot 2023-03-25 at 10 36 23 PM" src="https://user-images.githubusercontent.com/97116253/227753893-23ec0be3-5bbe-468b-a590-674e3a0e8f65.png">

Furthermore, we created another table only for the year 2023 to get the teams for this year. However there were missing teams in the dataset so we added the teams to get the accurate data of the teams playing for this year in the march madness. In addition, the seeds of certain teams were entered wrong so we corrected that. The last thing we needed to add was the region in which the teams were playing in. 
```
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
                    mergedData$TEAM == "Colgate" | mergedData$TEAM == "Mississippi State"] <- "Midwest"
mergedData$REGION[mergedData$TEAM == "Kansas" | mergedData$TEAM == "Howard" | mergedData$TEAM == "Arkansas" | mergedData$TEAM == "Illinois" |
                    mergedData$TEAM == "St. Mary's (CA)" | mergedData$TEAM == "VCU" | mergedData$TEAM == "Connecticut" |
                    mergedData$TEAM == "Iona" | mergedData$TEAM == "TCU" | mergedData$TEAM == "Arizona State" | mergedData$TEAM == "Gonzaga" |
                    mergedData$TEAM == "Northwestern" | mergedData$TEAM == "Boise State" | mergedData$TEAM == "UCLA" | mergedData$TEAM == "NC-Asheville" | mergedData$TEAM == "Nevada"] <- "West"
mergedData<-mergedData[complete.cases(mergedData), ]
mergedData <- mergedData %>%
  add_row(TEAM = 'Drake', OFFENSIVE.REBOUND.. = 23.8, DEFENSIVE.REBOUND.. = 77.6, SEED = 12, X2PT.. = 51.9, X3PT.. = 36.4, WIN.. = 76.5, FREE.THROW.. = 76.9, REGION = "Midwest")%>%
  add_row(TEAM = 'Utah State', OFFENSIVE.REBOUND.. = 27.2, DEFENSIVE.REBOUND.. = 75.5, SEED = 10, X2PT.. = 54.6, X3PT.. = 37.9, WIN.. = 73.5, FREE.THROW.. = 76.6, REGION = "South")%>%
  add_row(TEAM = 'Kent State', OFFENSIVE.REBOUND.. = 31.3, DEFENSIVE.REBOUND.. = 69.9, SEED = 13, X2PT.. = 51.4, X3PT.. = 33.2, WIN.. = 78.8, FREE.THROW.. = 72.3, REGION = "Midwest")%>%
  add_row(TEAM = 'Louisiana', OFFENSIVE.REBOUND.. = 33.3, DEFENSIVE.REBOUND.. = 73.7, SEED = 13, X2PT.. = 51.9, X3PT.. = 36.5, WIN.. = 74.2, FREE.THROW.. = 67.1, REGION = "East")%>%
  add_row(TEAM = 'UCSB', OFFENSIVE.REBOUND.. = 26.2, DEFENSIVE.REBOUND.. = 74.0, SEED = 14, X2PT.. = 54.8, X3PT.. = 34.8, WIN.. = 75.8, FREE.THROW.. = 73.6, REGION = "South")%>%
  add_row(TEAM = 'Montana State', OFFENSIVE.REBOUND.. = 25.7, DEFENSIVE.REBOUND.. = 74.7, SEED = 14, X2PT.. = 52.6, X3PT.. = 31.8, WIN.. = 69.7, FREE.THROW.. = 75.8, REGION = "East")%>%
  add_row(TEAM = 'GCU', OFFENSIVE.REBOUND.. = 30.2, DEFENSIVE.REBOUND.. = 69.5, SEED = 14, X2PT.. = 50.4, X3PT.. = 37.6, WIN.. = 63.6, FREE.THROW.. = 71.5, REGION = "West")%>%
  add_row(TEAM = 'Kennesaw State', OFFENSIVE.REBOUND.. = 27.3, DEFENSIVE.REBOUND.. = 74.6, SEED = 14, X2PT.. = 51.2, X3PT.. = 36.8, WIN.. = 71.9, FREE.THROW.. = 66.2, REGION = "Midwest")%>%
  add_row(TEAM = 'SE Missouri State', OFFENSIVE.REBOUND.. = 25.0, DEFENSIVE.REBOUND.. = 71.7, SEED = 16, X2PT.. = 50.3, X3PT.. = 33.2, WIN.. = 50.0, FREE.THROW.. = 71.1, REGION = "South")%>%
  add_row(TEAM = 'Teaxas Southern', OFFENSIVE.REBOUND.. = 29.2, DEFENSIVE.REBOUND.. = 72.6, SEED = 16, X2PT.. = 47.7, X3PT.. = 28.0, WIN.. = 36.4, FREE.THROW.. = 66.3, REGION = "East")%>%
  add_row(TEAM = 'Northern Kentucky', OFFENSIVE.REBOUND.. = 31.6, DEFENSIVE.REBOUND.. = 67.0, SEED = 16, X2PT.. = 47.4, X3PT.. = 34.3, WIN.. = 60.6, FREE.THROW.. = 70.8, REGION = "Midwest")


```



# Looking at the winners- Leandra
Since the first round and second round had already started taking place when we were finalizing our metrics, we looked at how the variables we were considering played a part in the first round upsets. The first one where we found a correlation in our variables and the upset was the Furman against Virgina game. 
<img width="767" alt="Screen Shot 2023-03-26 at 11 14 42 PM" src="https://user-images.githubusercontent.com/113047041/227840293-49d8e5fc-03fa-4ec2-b31f-6f0395bf4e24.png">

From this table, we are able to see that Furman has a higher offensive rebound, 2 point shot and free throw percentage compared to Virginia. 
The next game we compared was the Marquette versus Michigan State game. 
<img width="786" alt="Screen Shot 2023-03-26 at 11 15 36 PM" src="https://user-images.githubusercontent.com/113047041/227840560-9ec0fc86-4d0a-4a68-9552-d2f645809450.png">

Here we can see that Michigan State has a higher offensive and defensive rebound percentage along with a higher three point and free throw shot percentage.  

# Correlation-Hildana
```
variables <- df%>%
  select(Company, Product,Issue,State)

```

# Creating a new meteric- Leandra
While trying to figure out which variable correctly predicted the most games from the first two rounds, we realized there was not one single stat that made the difference in the win or loss so we decided to combine all the variables to created a new column. 
<img width="887" alt="Screen Shot 2023-03-26 at 11 29 56 PM" src="https://user-images.githubusercontent.com/113047041/227841266-8456e36c-b4c3-41db-a824-ebf17d2ac86b.png">

This new column summed up all those variables to create a new score for each team and in order to decide which variables we liked the best for this final score, we tried multiple different combinations of variables. After messing around with the variables and finding the ones we liked the best, we decided on Houston as the winner of the tournament, which we sadly now know didn't work since March Madness is nearly impossible to predict. 

# Shiny App - Mansi 
For the shiny app, we made a model where we used the average column in the y axis and team name in the x axis to illustrate the winning probabily of two teams selected. 
```
ui <- fluidPage(
  titlePanel("March Madness Win Percentage"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Select Team 1", choices = unique(mergedData$TEAM)),
      selectInput("team2", "Select Team 2", choices = unique(mergedData$TEAM)),
      
    ),
    mainPanel(
      plotOutput("winplot")
    )
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
```

# Conclusion-
Winner for each bracket
Overall winner

# Contributors
Hildana Teklegiorgis</br>
Leandra Gottschalk</br>
Mansi Gujadhur</br>





