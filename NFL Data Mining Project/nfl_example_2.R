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

# Load data 2012:2022
data <- load_pbp(2012:2022)

# dimension
dim(data)

# column names: https://www.nflfastr.com/articles/field_descriptions.html
colnames(data)

# Data of one team: ARI
D_ari <- subset(data, home_team=="ARI" | away_team=="ARI")

#########################################################
# One row in data: One play in a game.
# You need to decide unit of analysis

# (1) Unit of analysis / one row: A Game of ARI
# - A model to predict who win the game based on half-time stats, for betting.
# - Outcome variable: ARI win or lose a game.
# - Predictors: Half-time stats, end of 3rd qt stats, etc.

D_outcome <- D_ari[ , c("game_id","week", "game_date", "home_team", "away_team", 
                        "home_score", "away_score")]
D_outcome <- distinct(D_outcome)

# Create year variable (or lubridate library)
D_outcome$year <- as.numeric(str_sub(D_outcome$game_date, 1, 4))

# Create variable: ARI is home team
D_outcome$ARI_home <- ifelse(D_outcome$home_team=="ARI", "home_game", "away_game")

# Create class label: ARI_win: Yes/No
D_outcome$ARI_win <- NA
for (i in 1:nrow(D_outcome)) {
  if(D_outcome$home_team[i]=="ARI" & D_outcome$home_score[i]>=D_outcome$away_score[i]){
    D_outcome$ARI_win[i] <- "yes"
  } 
  
  if(D_outcome$home_team[i]=="ARI" & D_outcome$home_score[i]<D_outcome$away_score[i]){
    D_outcome$ARI_win[i] <- "no"
  } 
  
  if(D_outcome$home_team[i]!="ARI" & D_outcome$home_score[i]<D_outcome$away_score[i]){
    D_outcome$ARI_win[i] <- "yes"
  } 
  
  if(D_outcome$home_team[i]!="ARI" & D_outcome$home_score[i]>D_outcome$away_score[i]){
    D_outcome$ARI_win[i] <- "no"
  } 
}

# Create numeric outcome: ARI score
D_outcome$ARI_score <- 0
for (i in 1:nrow(D_outcome)) {
  if(D_outcome$home_team[i]=="ARI"){
    D_outcome$ARI_score[i] <- D_outcome$home_score[i]
  } else {
    D_outcome$ARI_score[i] <- D_outcome$away_score[i]
  }
}


##############################################################
# Calculate some offensive measures, aggregated to each game
D_ari_off <- subset(D_ari, posteam=="ARI")

##############
# Predictor 1: Average kick distance for a game
D_ari_avg_kick_dist <- D_ari_off[ , c("game_id", "kick_distance")]
D_ari_avg_kick_dist <- na.omit(D_ari_avg_kick_dist)

# group_by game_id
D_ari_avg_kick_dist <- group_by(D_ari_avg_kick_dist, game_id)
# calculate average game kick distance
D_ari_avg_kick_dist <- mutate(D_ari_avg_kick_dist, avg_kick_dist=mean(kick_distance))
# Get distinct game and avg game kick distance
D_ari_avg_kick_dist <- distinct(D_ari_avg_kick_dist, game_id, avg_kick_dist)

# Add ARI avg_game_kick_dist to outcome data
D_outcome <- left_join(D_outcome, D_ari_avg_kick_dist,
                       by=c("game_id"="game_id"))


##############
# Predictor 2: run location
D_ari_run_location <-  D_ari_off[ , c("game_id", "run_location")]
D_ari_run_location <- na.omit(D_ari_run_location)

# number of times for each game-run_location:
D_ari_run_location <- group_by(D_ari_run_location, game_id, run_location) %>%
  mutate(n=n()) %>%
  distinct(game_id, run_location, n)

# Create three variables: long to wide data
D_ari_run_location <- pivot_wider(D_ari_run_location, 
                                  names_from = "run_location",
                                  values_from = "n")
# Replace NA with 0
D_ari_run_location[is.na(D_ari_run_location)] <- 0
# Check one row is a unique game_id
length(unique(D_ari_run_location$game_id))

# Join to D_outcome data
D_outcome <- left_join(D_outcome, D_ari_run_location,
                       by=c("game_id"="game_id"))


##############
# Predictor 3: play type: proportion of pass
D_ari_play_type <-  D_ari_off[ , c("game_id", "play_type")]
D_ari_play_type <- subset(D_ari_play_type, play_type %in% c("run", "pass"))

# number of times for each play_type:
D_ari_play_type <- group_by(D_ari_play_type, game_id, play_type) %>%
  mutate(n=n()) %>%
  distinct(game_id, play_type, n)

# Create three variables: long to wide data
D_ari_play_type <- pivot_wider(D_ari_play_type, 
                               names_from = "play_type",
                               values_from = "n")

D_ari_play_type$p_run <- D_ari_play_type$run / (D_ari_play_type$run + D_ari_play_type$pass)

# Join to D_outcome data
D_outcome <- left_join(D_outcome, D_ari_play_type,
                       by=c("game_id"="game_id"))

##############################################################
# Calculate some defensive measures, aggregated to each game
D_ari_def <- subset(D_ari, posteam!="ARI")
# ....

#########################################################
# Training / Holdout split:
D_Train <- subset(D_outcome, year<=2021)
D_Test <- subset(D_outcome, year==2022)

# Build one model: glmnet
# - Need to build more and choose one using 1-sd rule.
fitControl <- trainControl(method="cv", number=5)
glmnetGrid <- expand.grid(alpha = seq(0, 1, .1),
                          lambda = 10^seq(-3,-0.5,by=.5)) 
colnames(D_Train)
GLMnet <- train(ARI_score ~ week + ARI_home + avg_kick_dist + middle + left + right + p_run, # Should add more predictors.
                data=D_Train, 
                method='glmnet',  # 1 pt
                tuneGrid=glmnetGrid,
                trControl=fitControl, 
                preProc = c("center", "scale"))
GLMnet$bestTune
GLMnet$results[rownames(GLMnet$bestTune),]

## 
D_Test$predict_Ari_score <- predict(GLMnet, D_Test)

# Plot true and predicted ARI score
plot(D_Test$ARI_score, D_Test$predict_Ari_score,
     xlim=c(5,35), ylim=c(5,35), pch=15)
abline(a=0,b=1)

# Holdout RMSE
RMSE <- sqrt(mean((D_Test$ARI_score - D_Test$predict_Ari_score)^2))
RMSE
mean(D_Test$ARI_score)
RMSE / mean(D_Test$ARI_score)


##########################################################################
# (2) Unit of analysis: Offensive performance of a game for ARI
# - subset: posteam=="ARI", then aggregate by "game_id"
# - Outcome variable: ..
# - Features: ...


##########################################################################
# (3) Unit of analysis: Defensive performance of a game for ARI
# - subset: defteam=="ARI", then aggregate by "game_id"
# - Outcome variable: ..
# - Features: ...


##########################################################################
# Training / Holdout split:
# - By date
# - By regular / playoffs (pick a team made to the playoffs...)
# - By games: 
#.  - Unit of analysis: Game
#.  - Outcome: win
#.  - Predictors: Half-time stats
#   - A model to predict who win the game based on half-time stats, for betting.

##########################################################################
# Build Models with training data
# Model 1: regression
# Model 2: regularized regression
# Model 3: Tree
# Model 4: RF
# Model 5: BGM
# - Pick one using 1-sd rule

# - Predict holdout set, get holdout set performances



