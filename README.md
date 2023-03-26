# March Madness

For this project we analysed data on the performance of teams on March madness since 2003. We cleaned, run a correlation test, then created a new metric that had a high correlation to win percentage. Our goal was to come up with a good way to choose a winner for the tournament.


![19a42410dda651c98b107d53ef654d0d](https://user-images.githubusercontent.com/108307724/224206117-8971413f-8999-4c35-a383-d61730d24871.jpeg)

# Dictionary📝
We used information of the Tournament Game data and the colums used to predict the winner were:
1. TEAM - the team name 
2. SEED - the rank of the team 
3. YEAR - the year of their performace rate
4. TEAM.ROUND - the round number they got eliminated 
5. FREE.THROW.. - percentage of unopposed attempts to score points 
6. WIN - percentage of chances of winning 
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

Furthermore, we created another table only for the year 2023 to get the teams for thsi year. However there were missing teams in the dataset so we added the teams to get the accurate data of the teams playing for this year in the march madness.
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

```



# Looking at the winners- Leandra

# Correlation-Hildana
```
variables <- df%>%
  select(Company, Product,Issue,State)

```

# Creating a new meteric- Leandra
# Creating Interactive Shiny app

# Conclusion-
Winner for each bracket
Overall winner

# Contributors
Hildana Teklegiorgis</br>
Leandra Gottschalk</br>
Mansi Gujadhur</br>





