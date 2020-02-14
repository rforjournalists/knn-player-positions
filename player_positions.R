
library(tidyverse)
library(class)
library(gmodels)
library(fplscrapR)

setwd('C:/Users/Rob/Documents/R/ml/ch3')

#get season data
data_2018_19 <- getplayerdetails1819


#get totals for season
data_2018_19_by_player <- data_2018_19 %>%
  group_by(playername) %>%
  summarise(minutes = sum(minutes), fouls = sum(fouls), errors = sum(errors_leading_to_goal), threat = sum(threat), goals_scored = sum(goals_scored), creativity = sum(creativity), cbi = sum(clearances_blocks_interceptions), assists = sum(assists), dribbles = sum(dribbles), key_passes = sum(key_passes), offside = sum(offside), open_play_crosses = sum(open_play_crosses), tackles = sum(tackles))


#add in player positions
players <- read.csv('players.csv', stringsAsFactors = FALSE)
player_data <- merge(players,data_2018_19_by_player, by.x = 'Player', by.y = 'playername')

#remove goalkeepers
table(player_data$Position)
player_data <- player_data[player_data$Position != 'Goalkeeper',]
table(player_data$Position)

#remove players with less than 7 games
player_data <- player_data[player_data$minutes >= 630,]
player_data$minutes <- NULL

#make positions a factor
player_data$Position <- factor(player_data$Position, levels = c('Defender','Midfielder','Forward'))

#randomize order
set.seed(4)
player_data <- player_data[sample(nrow(player_data)),]


#reset row.names
row.names(player_data) <- seq(1,nrow(player_data),1)

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#apply
player_data_n <- as.data.frame(lapply(player_data[4:13], normalize))

#get sample of 80% for training
player_data_train <- player_data_n[1:194,]
player_data_test <- player_data_n[195:nrow(player_data_n),]

#get positions
player_train_labels <- player_data[1:194, 2]
player_test_labels <- player_data[195:nrow(player_data), 2]

#work out k (13.9 so 14)
round(sqrt(nrow(player_data_train)), digits = 0)

#run
player_test_pred <- class::knn(train = player_data_train, test = player_data_test, cl = player_train_labels, k = 14)

#evaluate
gmodels::CrossTable(x = player_test_labels, y = player_test_pred, prop.chisq = FALSE)

#look at outliers
player_test_names <- data.frame(name = player_data[195:nrow(player_data), 1], position = player_data[195:nrow(player_data), 2])
player_test_names$predicted_position <- player_test_pred
player_test_names$correct <- player_test_pred == player_test_labels
player_test_names[player_test_names$correct == FALSE,]

