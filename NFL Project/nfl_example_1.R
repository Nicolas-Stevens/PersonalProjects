##
## Get started: https://www.nflfastr.com/articles/beginners_guide.html
## - A whole book about it: https://bradcongelio.com/nfl-analytics-with-r-book/ 
## - But you know all the advanced models (Chapter 5.1)
## - A good resource to learn more about data wrangling (tydiverse, r4ds)
## 

library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(caret)

# Load data 2020:2022
data <- load_pbp(2020:2022)

# dimension
dim(data)

# column names: https://www.nflfastr.com/articles/field_descriptions.html
colnames(data)

# Data of one team: ARI
D_ari <- subset(data, home_team=="ARI" | away_team=="ARI")

#########################################################
# One row in data: One play in a game.

# Unit of analysis: Offensive Play
# Outcome: Success

D_ari_off <- subset(D_ari, posteam=="ARI")

D_outcome <- D_ari_off[ , c("play_id","week", "game_date", "home_team", "away_team", "success")]
table(D_outcome$success)

# Create year variable
D_outcome$year <- as.numeric(str_sub(D_outcome$game_date, 1, 4))

# Create variable: ARI is home team
D_outcome$ARI_home <- ifelse(D_outcome$home_team=="ARI", "home_game", "away_game")

# Success rate for home and away games
table(D_outcome$ARI_home, D_outcome$success)

##########################################################
# Create predictors, for each play:
colnames(D_ari_off)
# https://www.nflfastr.com/articles/field_descriptions.html
# (1). posteam_score: Score the posteam at the start of the play.
# (2). defteam_score: Score the defteam at the start of the play.
# (3). series: ..
# (4). play_type: 
# (5). Total ARI pass, from (total_home_pass_wpa and total_away_pass_wpa)
# (6). weather, wind, Quarter of the game (5 is overtime).


#########################################################
# One row in data: One play in a game.

# Unit of analysis: Deffense Play
D_ari_def <- subset(D_ari, posteam!="ARI")

# Outcome: Success
# Predictors: ...


