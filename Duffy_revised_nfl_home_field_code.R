#-----------------------------------------------
#author: Carlo Duffy
#last edited: 03/08/19
#Code for my first CMU Sports Analytics Club article
#"Which NFL Teams Perform Better at Home Games?"
#https://www.cmusportsanalytics.com/nfl-teams-perform-better-home-games/
#I use 2009-2016 NFL play-by-play data to obtain teams' points scored 
#and points given up on defense per game.
#I create a binary variable indicating whether a team is home in a game.
#To determine which NFL team perform better at home games, for every team I use 
#two SLR models regressing:
# (1) demeaned points scored on the home indicator variable;
# (2) demeaned points given up on the home indicator variable.
#So I implement each SLR model on all 32 teams, meaning I create 32 * 2 = 64 regressions.
# I determine teams performing better at home games have staistically significant. 
# slope estimates at the 95% level.

#-------------------------------------------------
library(tidyverse)
library(data.table)
library(broom)

data <- fread("C:/CMU First Semester/TSAC/NFL Game Results with Passing and Rushing Stats ('09-'16).CSV",header=TRUE)
#was taken from:
#https://www.kaggle.com/maxhorowitz/nflplaybyplay2009to2016/discussion/39200

#note, for future articles I have used nflscrapR (created by CMU alumni), 
#which has the same data and a lot more:
#https://github.com/maksimhorowitz/nflscrapR

#new column in data - season
data$season <- str_sub(data$date, start = nchar(data$date) - 3, 
                       end = nchar(data$date))

# Adjusting for Rams move from STL to LA 
data$home<-ifelse(data$home=='LA','STL',data$home)
data$away<-ifelse(data$away=='LA','STL',data$away)

# Identifying the home team for each game
data$home_ind<-ifelse( data$Team.x==data$home ,1,0)

# Calculating points scored and given up by defense
data$score<-ifelse( data$Team.x==data$home, data$homescore,data$awayscore) 
data$defense<-ifelse( data$Team.x==data$home, data$awayscore,data$homescore)

# Removing duplicates
data <- data %>% 
  distinct(Team.x,season,date,score,defense,home_ind)

# Sorting by season, team - getting average points scored and points given up
team.means <- data %>% 
  group_by(season, Team.x) %>% 
  summarise(avgscore = mean(score), avgdefense = mean(defense))

data <- inner_join(data, team.means)
data$scoreminusavg <- data$score-data$avgscore
data$defenseminusavg <- data$defense-data$avgdefense

# Separating score and defense regression model by team
# and getting regression results

#https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
scoreteam_models <- data %>% 
  group_by(Team.x) %>% 
  do(scoremodel = lm(scoreminusavg ~ as.character(home_ind), data = .)) %>% 
  tidy(scoremodel) %>% 
  select(team=Team.x,term, scoreest=estimate, scorep.value=p.value)
  

defenseteam_models <- data %>% 
  group_by(Team.x) %>% 
  do(defensemodel = lm(defenseminusavg ~ as.character(home_ind), data = .)) %>% 
  tidy(defensemodel) %>% 
  select(team=Team.x,term, defenseest=estimate, defensep.value=p.value)

# getting data for scatterplot
regoutput <- inner_join(scoreteam_models,defenseteam_models) %>% 
  subset(term=='as.character(home_ind)1')

regoutput$scoreest <- ifelse(regoutput$scorep.value > .05, 0, 
                             regoutput$scoreest)
regoutput$defenseest <- ifelse(regoutput$defensep.value > .05, 0,
                                 regoutput$defenseest)
#big reveal- the graph:
regoutput %>% 
ggplot(aes(x = defenseest, y = scoreest))+
  geom_point(color='blue') +
  geom_text(size=3,aes(label=team), position=position_jitter(width=.2,height=.2))
