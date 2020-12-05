library(DBI)
library(RSQLite)

soccer <- dbConnect(SQLite(), 
                 "/Users/kec_1/soccer/database.sqlite")

##### John's code
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

##### EC's code

player <- dbReadTable(soccer, 'Player')
player_attr <- dbReadTable(soccer, 'Player_Attributes')

player <- as.data.frame(player)
player_attr <- as.data.frame(player_attr)
player_attr$date <- substr(player_attr$date,1,10)

# filter season date
player_attr_2014 <- subset(player_attr, 
                           date >= '2014-07-18' & date <= '2015-05-31')
player_attr_2015 <- subset(player_attr, 
                           date >= '2015-07-17' & date <= '2016-05-25')

# remove negative values
player[player < 0] <- NA
player_attr_2014[player_attr_2014 < 0] <- NA
player_attr_2015[player_attr_2015 < 0] <- NA

# merge values of one player to one
aggregate_process <- function(x) {
  if (is.numeric(x)) {mean(x)}
  else {x[1]}
}

player_attr_2014_aggregate <- aggregate(player_attr_2014, 
               by=list(player_attr_2014$player_api_id),
               aggregate_process)
player_attr_2014 <- 
  player_attr_2014_aggregate[2:ncol(player_attr_2014_aggregate)]

player_attr_2015_aggregate <- aggregate(player_attr_2015, 
                                        by=list(player_attr_2015$player_api_id),
                                        aggregate_process)
player_attr_2015 <- 
  player_attr_2015_aggregate[2:ncol(player_attr_2015_aggregate)]

# remove outliers
# normalize
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

player <- preprocess(player)
player_attr_2014 <- preprocess(player_attr_2014)
player_attr_2015 <- preprocess(player_attr_2015)

# merge with players' basic info
player_2014 <- merge(player,player_attr_2014[,c(2:42)],
                     by=c('player_api_id','player_fifa_api_id'),
                     all.x = TRUE, all.y = TRUE)
player_2015 <- merge(player,player_attr_2015[,c(2:42)],
                     by=c('player_api_id','player_fifa_api_id'),
                     all.x = TRUE, all.y = TRUE)
# change dummies
player_2014$left_foot <- ifelse(player_2014$preferred_foot == 'left', 1, 0)
player_2015$left_foot <- ifelse(player_2015$preferred_foot == 'left', 1, 0)

player_2014$attacking_work_rate <- replace(
  player_2014$attacking_work_rate,player_2014$attacking_work_rate=='None',NA)
player_2014$attack_high <- ifelse(player_2014$attacking_work_rate == 'high', 1, 0)
player_2014$attack_medium <- ifelse(player_2014$attacking_work_rate == 'medium', 1, 0)

player_2015$attacking_work_rate <- replace(
  player_2015$attacking_work_rate,player_2015$attacking_work_rate=='None',NA)
player_2015$attack_high <- ifelse(player_2015$attacking_work_rate == 'high', 1, 0)
player_2015$attack_medium <- ifelse(player_2015$attacking_work_rate == 'medium', 1, 0)

player_2014$defend_high <- ifelse(player_2014$defensive_work_rate == 'high', 1, 0)
player_2014$defend_medium <- ifelse(player_2014$defensive_work_rate == 'medium', 1, 0)
player_2015$defend_high <- ifelse(player_2015$defensive_work_rate == 'high', 1, 0)
player_2015$defend_medium <- ifelse(player_2015$defensive_work_rate == 'medium', 1, 0)

# filter only 3 leagues
player_name_2014 <- subset(team_players_2014$player_name,
  team_players_2014$league_name == "England Premier League" |
  team_players_2014$league_name == "Germany 1. Bundesliga" |
  team_players_2014$league_name == "Spain LIGA BBVA"
)
player_2014 <- subset(player_2014,
                      player_name %in% player_name_2014)
player_name_2015 <- subset(team_players_2015$player_name,
  team_players_2015$league_name == "England Premier League" |
  team_players_2015$league_name == "Germany 1. Bundesliga" |
  team_players_2015$league_name == "Spain LIGA BBVA"
)
player_2015 <- subset(player_2015,
                      player_name %in% player_name_2015)

## filter for EACH league
player_name_2014_eng <- subset(team_players_2014$player_name,
  team_players_2014$league_name == "England Premier League")
player_name_2014_ger <- subset(team_players_2014$player_name,
  team_players_2014$league_name == "Germany 1. Bundesliga")
player_name_2014_spa <- subset(team_players_2014$player_name,
  team_players_2014$league_name == "Spain LIGA BBVA")

player_2014_eng <- subset(player_2014,
                      player_name %in% player_name_2014_eng)

player_2014_ger <- subset(player_2014,
                          player_name %in% player_name_2014_ger)

player_2014_spa <- subset(player_2014,
                          player_name %in% player_name_2014_spa)

# export
# write.csv(player_2014,'player_2014.csv')
# write.csv(player_2015,'player_2015.csv')

# LR
#cor(player_2014[,c(6,7,14:51)], use="complete.obs")

player_2014.lr <-
  lm(overall_rating ~ 
       height+weight+crossing+finishing+heading_accuracy+short_passing+
     volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+
     acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+
     stamina+strength+long_shots+aggression+interceptions+positioning+vision+
     penalties+marking+standing_tackle+sliding_tackle+gk_diving+gk_handling+
     gk_kicking+gk_positioning+gk_reflexes+left_foot+attack_high+
     attack_medium+defend_high+defend_medium,
   na.action = na.exclude, data = player_2014)
summary(player_2014.lr)

player_2014_eng.lr <-
  lm(overall_rating ~ 
       height+weight+crossing+finishing+heading_accuracy+short_passing+
       volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+
       acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+
       stamina+strength+long_shots+aggression+interceptions+positioning+vision+
       penalties+marking+standing_tackle+sliding_tackle+gk_diving+gk_handling+
       gk_kicking+gk_positioning+gk_reflexes+left_foot+attack_high+
       attack_medium+defend_high+defend_medium,
     na.action = na.exclude, data = player_2014_eng)
summary(player_2014_eng.lr)

player_2014_ger.lr <-
  lm(overall_rating ~ 
       height+weight+crossing+finishing+heading_accuracy+short_passing+
       volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+
       acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+
       stamina+strength+long_shots+aggression+interceptions+positioning+vision+
       penalties+marking+standing_tackle+sliding_tackle+gk_diving+gk_handling+
       gk_kicking+gk_positioning+gk_reflexes+left_foot+attack_high+
       attack_medium+defend_high+defend_medium,
     na.action = na.exclude, data = player_2014_ger)
summary(player_2014_ger.lr)

player_2014_spa.lr <-
  lm(overall_rating ~ 
       height+weight+crossing+finishing+heading_accuracy+short_passing+
       volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+
       acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+
       stamina+strength+long_shots+aggression+interceptions+positioning+vision+
       penalties+marking+standing_tackle+sliding_tackle+gk_diving+gk_handling+
       gk_kicking+gk_positioning+gk_reflexes+left_foot+attack_high+
       attack_medium+defend_high+defend_medium,
     na.action = na.exclude, data = player_2014_spa)
summary(player_2014_spa.lr)

library(ggplot2)
ggplot(data=player_2014_eng, 
       aes(x=player_2014_eng$aggression, y=player_2014_eng$overall_rating, 
           col=player_2014_eng$height, size=player_2014_eng$stamina)) + 
  geom_point()

player_2014.res <- resid(player_2014.lr)
plot(player_2014$overall_rating, player_2014.res,
     ylab="Residuals", xlab="Overall Rating")
abline(0,0)

plot(player_2014.lr)

player_2015.predict <- predict(player_2014.lr, player_2015[,c(6,7,14:51)],
        level=0.95)

library(forecast)
accuracy(player_2015.predict, player_2015$overall_rating)
