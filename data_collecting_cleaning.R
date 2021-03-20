library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(nflfastR)
library(janitor)
rm(list = ls())


# Just looking at scoring totals through the years.-----
games <- readRDS(url("http://www.habitatring.com/games.rds"))
scoring <- games %>% select(game_type,home_team,home_score,away_team,away_score,season,total) %>% filter(game_type=="REG") %>% group_by(season)
p <- ggplot(scoring, aes(x=season, y=total,group=season)) + 
  geom_boxplot()
p

#Pulling margins from the games dataframe code borrowed frm
#Ben Baldin with nflfastR--------
home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team) %>%
  filter(season %in% c(2018,2019,2020))
away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  filter(season %in% c(2018,2019,2020,2021)) %>% mutate(result=-result)

results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    ))
# Going to keep the names contant with current names.
#For some reason LV is the only one not current in this set
results <- results %>% mutate(team=str_replace(team,"OAK","LV")) 
margin <- results %>% group_by(season,team) %>% summarise(Margin = sum(result))
rm(away,home,games)
#Pulling multiple seasons for our independant variables 2018 to 2020 season-----------

seasons <- 2018:2020
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

### correct names
cols=c("home_team","away_team","posteam","defteam")
pbp[cols] <- lapply(pbp[cols], A)
A <-  function(name_column)(
  name_column = case_when(
    name_column == "OAK" ~ 'LV',
    name_column == 'SD' ~ 'LAC',
    name_column == 'STL' ~ 'LA',
    TRUE ~ name_column))
### filter for regular season
df <- pbp %>% filter(season_type=="REG")
rm(pbp)
####This is regular season data.
#### Below are independent variables that are used in Mathletics to determine wins
#Team offense PY/A (sacks and yards lost are included)----------
passatt <- df %>% select(posteam, pass_attempt, yards_gained, sack,season) %>%
  filter(pass_attempt==1)
pass_yard_per_attempt <- passatt %>% group_by(season,posteam) %>%
  summarise(total_yards=sum(yards_gained), total_attempts=sum(pass_attempt)) %>% 
  mutate(PY_A = (total_yards/total_attempts)) %>% rename(team=posteam) %>%
  select(season,team,PY_A)
rm(passatt)

#Team  defense- passing yards per attempt:(DPY/A)----
def_passatt <- df %>% select(defteam, pass_attempt, yards_gained, sack,season) %>%
  filter(pass_attempt==1)
def_pass_yard_per_attempt <- def_passatt %>% group_by(season,defteam) %>%
  summarise(total_yards=sum(yards_gained), total_attempts=sum(pass_attempt)) %>% 
  mutate(DPY_A = (total_yards/total_attempts)) %>% rename(team=defteam) %>%
  select(season,team,DPY_A)
rm(def_passatt)
#Team offense rushing yards per attempt (RY/A)----
rushatt <- df %>% select(posteam,defteam, rush_attempt, yards_gained,season) %>%
  filter(rush_attempt==1)

rush_yard_per_attempt <- rushatt %>% group_by(season,posteam) %>%
  summarise(total_yards=sum(yards_gained), total_attempts=sum(rush_attempt)) %>% 
  mutate(RY_A = (total_yards/total_attempts)) %>% rename(team=posteam) %>%
  select(season,team,RY_A)
#Team defense rushing yards per attempt (DRY/A)-----
def_rush_yard_per_attempt <- rushatt %>% group_by(season,defteam) %>%
  summarise(total_yards=sum(yards_gained), total_attempts=sum(rush_attempt)) %>% 
  mutate(DRY_A = (total_yards/total_attempts)) %>% rename(team=defteam) %>%
  select(season,team,DRY_A)
# Turnovers committed on offense(TO)------
turnovers_committed <- df %>% select(posteam,defteam,interception, fumble_lost, play_type,season) %>%
  filter(play_type %in% c("run","pass"))

turnovers_committed_offense <- turnovers_committed %>% filter(interception==1 | fumble_lost==1) %>%
  group_by(posteam,season) %>% count(name = "TO") %>% rename(team = posteam)

#Defensive turnovers(DTO)------
defensive_turnovers_committed <- turnovers_committed %>% filter(interception==1 | fumble_lost==1) %>%
  group_by(defteam,season) %>% count(name = "DTO") %>% rename(team = defteam)
rm(turnovers_committed)
#Differential between penalties committed by team and penalties commit (left out) for now------
##ted by its opponents (PENDIF)
# Going to leave this out for now, it did not make a huge difference in the mathletics study
# And quite frankly I need to think how to do this with this data.
# penalties <- df %>% filter(game_id =="2018_01_ATL_PHI") %>% select(posteam,defteam,desc,penalty, penalty_team) %>%
#  filter(penalty==1) %>% group_by(penalty_team) %>% count(penalty_team)
#return TDs by opponent (RET TD) (includes TDs scored on-----
#fumbles, interceptions, kickoffs, and punts)
#Thinking of combining frames
X = margin %>% select(team,season) #going to use this to merge with the dataframes below.

turnover_returned <- df %>% select(posteam,defteam,interception, fumble_lost, play_type,season, desc, touchdown) %>% 
  filter(touchdown==1, fumble_lost==1|interception==1) %>% group_by(defteam, season) %>% count(name="turnovers_returned") %>% 
  rename(team=defteam)#Turnovers returned...obvi defteam

kickoff<- df %>% select(posteam,defteam,kickoff_attempt, play_type,season, desc, touchdown) %>%
  filter(kickoff_attempt==1, touchdown ==1) %>% group_by(posteam,season) %>% count(name="kickoffs_returned")%>% 
  rename(team=posteam)# Kickoffs returned is posteam

punt <- df %>% select(posteam,defteam,punt_attempt, play_type,season, desc, touchdown) %>%
  filter(punt_attempt==1, touchdown ==1) %>% group_by(defteam,season) %>% count(name = "punts_returned")%>% 
  rename(team=defteam)#punts returned is defteam

all_returned <- X %>% left_join(turnover_returned) %>% left_join(kickoff) %>% 
  left_join(punt) %>% replace(is.na(.), 0) %>%
  mutate(ret_td=(turnovers_returned+kickoffs_returned+punts_returned)) %>% 
  select(team,season,ret_td)

#Combine all data, cleaning, and write out------
# This is really cool! Combining muiltiple frames
final_data=Reduce(function(x, y) merge(x, y, all=TRUE), list(margin,pass_yard_per_attempt,
                                                  def_pass_yard_per_attempt,rush_yard_per_attempt,
                                                  def_rush_yard_per_attempt,
                                                  turnovers_committed_offense,
                                                  defensive_turnovers_committed,all_returned))

#Function to round values
round_2_dgts <- function(x) {round(x, digits = 1)}
#Apply function
final_data <- final_data %>% rename_with(toupper) %>%
  mutate_at(c("PY_A","DPY_A","RY_A" ,"DRY_A"),.funs = round_2_dgts)
#writing out file for actually analysis
write.csv(x = final_data,file = "final_data.csv")
