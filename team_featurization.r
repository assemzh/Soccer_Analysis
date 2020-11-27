library(DBI)
library(RSQLite)

soccer <- dbConnect(drv=RSQLite::SQLite(), dbname="../input/soccer/database.sqlite")

##### Assem's code
library(tidyverse)
library(reshape2)
library(data.table)
library(dplyr)
library(knitr)

## list all tables
tables <- dbListTables(soccer)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]
lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- 
    dbGetQuery(conn=soccer, 
               statement=paste("SELECT * FROM '", 
                               tables[[i]], "'", sep=""))
}

# create dataframes
country <-  data.frame(lDataFrames[1])
league  <-  data.frame(lDataFrames[2])
match   <-  data.frame(lDataFrames[3])
player  <-  data.frame(lDataFrames[4])
team    <-  data.frame(lDataFrames[6])

# select relevant columns

match <- match %>% select(match_api_id,date,home_team_api_id,away_team_api_id,home_team_goal, away_team_goal)
team <- team %>% select(team_api_id,team_long_name)


match$home_score <- (match$home_team_goal - match$away_team_goal)
match$away_score <- - match$home_score
match$home_win_rate <- as.integer(as.logical(match$home_score>0))
match$away_win_rate <- as.integer(as.logical(match$away_score>0))


match_home <- match %>% select(match_api_id,date,home_team_api_id,home_score,home_win_rate) %>% 
  rename(
    team_api_id = home_team_api_id,
    score = home_score,
    win_rate = home_win_rate
    )
match_away <- match %>% select(match_api_id,date,away_team_api_id,away_score, away_win_rate) %>% 
  rename(
    team_api_id = away_team_api_id,
    score = away_score,
    win_rate = away_win_rate
    )

total <- rbind(match_home, match_away)
total$date <- as.Date(total$date)

match14 = with(total, total[(date >= "2014-01-01" & date < "2015-01-01"),])
match15 = with(total, total[(date >= "2015-01-01" & date < "2016-01-01"),])
df14 = subset(match14, select=-c(date))
df15 = subset(match15, select=-c(date))

team_ranking14_byscore <- aggregate(df14$score, by=list(team_api_id=df14$team_api_id), FUN=mean) %>% rename(score=x)
team_ranking15_byscore <- aggregate(df15$score, by=list(team_api_id=df15$team_api_id), FUN=mean) %>% rename(score=x)

team_ranking14_bywin <- aggregate(df14$win_rate, by=list(team_api_id=df14$team_api_id), FUN=mean) %>% rename(win_rate=x)
team_ranking15_bywin <- aggregate(df15$win_rate, by=list(team_api_id=df15$team_api_id), FUN=mean) %>% rename(win_rate=x)

team_ranking14 <- merge(team,team_ranking14_byscore, by="team_api_id",all.team_ranking14_byscore = TRUE)
team_ranking15 <- merge(team,team_ranking15_byscore,by="team_api_id",all.team_ranking14_byscore = TRUE)

team_ranking14 <- arrange(merge(team_ranking14,team_ranking14_bywin, by="team_api_id"),-win_rate)
team_ranking15 <- arrange(merge(team_ranking15,team_ranking15_bywin,by="team_api_id"),-win_rate)


#### John's code
# create dataframes
country <-  data.frame(lDataFrames[1])
league  <-  data.frame(lDataFrames[2])
match   <-  data.frame(lDataFrames[3])
player  <-  data.frame(lDataFrames[4])
team    <-  data.frame(lDataFrames[6])
# select relevant columns
country <- select(country, id, name) %>% 
  rename(country_id = id)  %>% 
  rename(country_name = name)   # use country_id as key for join
league  <- select(league, country_id, name) %>% 
  rename(league_name = name) # use country_id as key for join
match   <- select(match, id, country_id, league_id, season, 
                  stage, date, match_api_id, home_team_api_id, 
                  away_team_api_id, home_team_goal, away_team_goal, 
                  home_player_1, home_player_2, home_player_3, 
                  home_player_4, home_player_5, home_player_6, 
                  home_player_7, home_player_8, home_player_9, 
                  home_player_10, home_player_11, away_player_1, 
                  away_player_2, away_player_3, away_player_4, 
                  away_player_5, away_player_6, away_player_7, 
                  away_player_8, away_player_9, away_player_10, 
                  away_player_11, goal, shoton, shotoff, 
                  foulcommit, card, cross, corner, possession)
player  <- select(player,player_api_id, player_name) # use player_api_id as key for join
team    <- select(team, team_api_id, team_long_name, team_short_name) # use team_api_id as key for join

others = colnames(match)[1:11]
players = colnames(match)[12:33]

flatten <- melt(match, id = others, measure = players, na.rm = TRUE, value.name = "player_api_id") %>% 
  mutate(team_api_id = 
           ifelse(grepl("home",variable),home_team_api_id, 
                  ifelse(grepl("away",variable),away_team_api_id,NA))) %>%  
  left_join(country, by = "country_id")    %>% 
  left_join(league,  by = "country_id")    %>%  
  left_join(team,    by = "team_api_id")   %>%
  left_join(player,  by = "player_api_id") %>% 
  separate(season, into = c("season_start","season_end"), sep = "/", convert = TRUE) 
head(flatten)

team_players <- select(flatten, season_start, season_end, 
                       country_name, league_name, player_api_id, 
                       team_api_id, team_long_name, team_short_name,
                       player_name)

team_players_2015 <- team_players %>% filter(season_start == 2015)
team_players_2014 <- team_players %>% filter(season_start == 2014)

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
    if (is.numeric(data[,i]) && !grepl("id",names(data)[i])) {
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

# filter only 3 leagues
team_id_2014 <- team_players_2014$team_api_id
team_2014 <- subset(team_2014,
                      team_api_id %in% team_id_2014)
team_id_2015 <- team_players_2015$team_api_id
team_2015 <- subset(team_2015,
                      team_api_id %in% team_id_2015)

# change dummies
team_2014$chanceCreationPositioning_organised <- ifelse(team_2014$chanceCreationPositioningClass == "Organised", 1, 0)
team_2015$chanceCreationPositioning_organised <- ifelse(team_2015$chanceCreationPositioningClass == "Organised", 1, 0)

team_2014$defenceDefenderLine_cover <- ifelse(team_2014$defenceDefenderLineClass == "Cover", 1, 0)
team_2015$defenceDefenderLine_cover <- ifelse(team_2015$defenceDefenderLineClass == "Cover", 1, 0)

# linear regression
library(forecast)

team_2014_score.lr <- lm(score ~ buildUpPlaySpeed+buildUpPlayDribbling+buildUpPlayPassing+
                   chanceCreationPassing+chanceCreationCrossing+chanceCreationShooting+
                   chanceCreationPositioningClass+defencePressure+defenceAggression+defenceTeamWidth+
                   defenceDefenderLineClass+chanceCreationPositioning_organised+defenceDefenderLine_cover,
                   na.action = na.exclude, data = team_2014)
#summary(team_2014_score.lr)

team_2014_win_rate.lr <- lm(win_rate ~ buildUpPlaySpeed+buildUpPlayDribbling+buildUpPlayPassing+
                   chanceCreationPassing+chanceCreationCrossing+chanceCreationShooting+
                   chanceCreationPositioningClass+defencePressure+defenceAggression+defenceTeamWidth+
                   defenceDefenderLineClass+chanceCreationPositioning_organised+defenceDefenderLine_cover,
                   na.action = na.exclude, data = team_2014)
#summary(team_2014_win_rate.lr)

team_2015_score.predict <- predict(team_2014_score.lr, team_2015[,c(6:18)], level=0.95)
accuracy(team_2015_score.predict, team_2015$score)

team_2015_win_rate.predict <- predict(team_2014_win_rate.lr, team_2015[,c(6:18)], level=0.95)
accuracy(team_2015_win_rate.predict, team_2015$win_rate)
