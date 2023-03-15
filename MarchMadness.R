library(readxl)
library(dplyr)

#Leandra's working directory
setwd("~/git_data/marchMadness23")

tournGameData <- read.csv('Tournament Game Data.csv')

gonzagaData <- tournGameData %>%
  dplyr::filter(TEAM == "Gonzaga")
