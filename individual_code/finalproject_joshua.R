#### Jaja's code

## Preprocessing Team & Team Attributes
team <- data.frame(lDataFrames[6])
team_attr <- data.frame(lDataFrames[7])

team <- select(team,id,team_api_id)
team_attr$date <- substr(team_attr$date,1,10)
team_attr <- select(team_attr,id,team_api_id,date,
                    buildUpPlaySpeed,buildUpPlayDribbling,
                    buildUpPlayPassing,chanceCreationPassing,chanceCreationCrossing,
                    chanceCreationShooting,chanceCreationPositioningClass,defencePressure,
                    defenceAggression,defenceTeamWidth,defenceDefenderLineClass)

#filter season date
team_attr_2014 <- subset(team_attr, date >= '2014-07-18' & date <= '2015-05-31')
team_attr_2015 <- subset(team_attr, date >= '2015-07-17' & date <= '2016-05-25')

# remove negative values
team_attr_2014[team_attr_2014 < 0] <- NA
team_attr_2015[team_attr_2015 < 0] <- NA

# merge values of one team to one
aggregate_process <- function(x) {
  if (is.numeric(x)) {mean(x)}
  else {x[1]}
}

team_attr_2014_aggregate <- aggregate(team_attr_2014, 
                                      by=list(team_attr_2014$team_api_id),
                                      aggregate_process)
team_attr_2014 <- 
  team_attr_2014_aggregate[2:ncol(team_attr_2014_aggregate)]

team_attr_2015_aggregate <- aggregate(team_attr_2015, 
                                      by=list(team_attr_2015$team_api_id),
                                      aggregate_process)
team_attr_2015 <- 
  team_attr_2015_aggregate[2:ncol(team_attr_2015_aggregate)]

# remove outliers and normalize
preprocess <- function(data) {
  for(i in 1:ncol(data)) {
    if (is.numeric(data[,i]) && (grepl("Width",names(data)[i]) | !grepl("id",names(data)[i]))) {
      mean <- mean(data[,i])
      sd <- sd(data[,i])
      data[,i] <- replace(data[,i],data[,i]>mean+2*sd,NA)
      data[,i] <- replace(data[,i],data[,i]<mean-2*sd,NA)
      data[,i] <- scale(data[,i])
    }
  }
  return(data)
}

team_attr_2014 <- preprocess(team_attr_2014)
team_attr_2015 <- preprocess(team_attr_2015)

# merge with team's rank
team_2014 <- merge(team_ranking14,team_attr_2014[,c(2:14)],
                   by=c('team_api_id'),
                   all.x = TRUE, all.y = TRUE)
team_2015 <- merge(team_ranking15,team_attr_2015[,c(2:14)],
                   by=c('team_api_id'),
                   all.x = TRUE, all.y = TRUE)

team_2014[is.na(team_2014)] <- 0
team_2015[is.na(team_2015)] <- 0

# filter only 3 leagues
team_id_2014 <- team_players_2014$team_api_id
team_2014 <- subset(team_2014,
                    team_api_id %in% team_id_2014)

team_id_2014_eng <- subset(team_players_2014$team_api_id,
                           team_players_2014$league_name == "England Premier League")
team_2014_eng <- subset(team_2014,
                        team_api_id %in% team_id_2014_eng)

team_id_2014_ger <- subset(team_players_2014$team_api_id,
                           team_players_2014$league_name == "Germany 1. Bundesliga")
team_2014_ger <- subset(team_2014,
                        team_api_id %in% team_id_2014_ger)

team_id_2014_spa <- subset(team_players_2014$team_api_id,
                           team_players_2014$league_name == "Spain LIGA BBVA")
team_2014_spa <- subset(team_2014,
                        team_api_id %in% team_id_2014_spa)

team_id_2015 <- team_players_2015$team_api_id
team_2015 <- subset(team_2015,
                    team_api_id %in% team_id_2015)

team_id_2015_eng <- subset(team_players_2015$team_api_id,
                           team_players_2015$league_name == "England Premier League")
team_2015_eng <- subset(team_2015,
                        team_api_id %in% team_id_2015_eng)

team_id_2015_ger <- subset(team_players_2015$team_api_id,
                           team_players_2015$league_name == "Germany 1. Bundesliga")
team_2015_ger <- subset(team_2015,
                        team_api_id %in% team_id_2015_ger)

team_id_2015_spa <- subset(team_players_2015$team_api_id,
                           team_players_2015$league_name == "Spain LIGA BBVA")
team_2015_spa <- subset(team_2015,
                        team_api_id %in% team_id_2015_spa)


# change dummies
team_2014$chanceCreationPositioning_organised <- ifelse(team_2014$chanceCreationPositioningClass == "Organised", 1, 0)
team_2015$chanceCreationPositioning_organised <- ifelse(team_2015$chanceCreationPositioningClass == "Organised", 1, 0)

team_2014$defenceDefenderLine_cover <- ifelse(team_2014$defenceDefenderLineClass == "Cover", 1, 0)
team_2015$defenceDefenderLine_cover <- ifelse(team_2015$defenceDefenderLineClass == "Cover", 1, 0)


###PCA
library(psych)
library(dplyr)
library(ggfortify)
library("factoextra")
library("FactoMineR")

## England
team_2014_eng_num <- team_2014_eng[,c(6:11,13:15)]
eng.pca <- PCA(team_2014_eng_num, graph = FALSE)
fviz_pca_var(eng.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping)

## Germany
team_2014_ger_num <- team_2014_ger[,c(6:11,13:15)]
ger.pca <- PCA(team_2014_ger_num, graph = FALSE)
fviz_pca_var(ger.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping)

## Spain
team_2014_spa_num <- team_2014_spa[,c(6:11,13:15)]
spa.pca <- PCA(team_2014_spa_num, graph = FALSE)
fviz_pca_var(spa.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping)
             
             
### linear regression
library(forecast)

## England
team_2014_eng_win_rate.lr <- lm(win_rate ~ buildUpPlaySpeed+buildUpPlayDribbling+buildUpPlayPassing+
                                  chanceCreationPassing+chanceCreationCrossing+chanceCreationShooting+
                                  defencePressure+defenceAggression+defenceTeamWidth,
                                na.action = na.exclude, data = team_2014_eng)
summary(team_2014_eng_win_rate.lr)


## Germany
team_2014_ger_win_rate.lr <- lm(win_rate ~ buildUpPlaySpeed+buildUpPlayDribbling+buildUpPlayPassing+
                                  chanceCreationCrossing+chanceCreationShooting+
                                  defencePressure+defenceTeamWidth,
                                na.action = na.exclude, data = team_2014_ger)
summary(team_2014_ger_win_rate.lr)


## Spain
team_2014_spa_win_rate.lr <- lm(win_rate ~ buildUpPlaySpeed+buildUpPlayDribbling+buildUpPlayPassing+
                                  chanceCreationPassing+chanceCreationCrossing+chanceCreationShooting+
                                  defencePressure+defenceAggression+defenceTeamWidth,
                                na.action = na.exclude, data = team_2014_spa)
summary(team_2014_spa_win_rate.lr)