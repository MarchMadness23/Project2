library(readxl)
library(dplyr)

#Leandra's working directory
setwd("~/git_data/marchMadness23")
#Hildana's directory
setwd("~/Desktop/DATA-332/Project2")


tournGameData <- read.csv('Tournament Game Data.csv')

gonzagaData <- tournGameData %>%
  dplyr::filter(TEAM == "Gonzaga")

