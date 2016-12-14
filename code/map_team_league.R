source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("tidyjson","dplyr","XLConnect"))

#read data
players<-read.csv("./raw/fangraphs_player_2015.csv",header = T)

#Create Team League Mapping
team_league_map<-select(players,Team)%>% distinct(.)
##map manually filled out
#write.csv(team_league_map,paste("./mapping/team_league_map.csv",sep = ""),na="",row.names = F)
#read in map 
map_team_league<-read.csv("./mapping/team_league_map.csv",header = T)


#save map
save(map_team_league,file="./mapping/map_team_league.RData")

#load map
#load(file="./mapping/map_age.RData")