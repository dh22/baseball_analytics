source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("tidyjson","dplyr"))

#read-in aging map
load(file="./mapping/map_age.RData")
load(file="./mapping/map_team_league.RData")

#read datasets 
leagueavg<-read.csv("./raw/fangraphs_leagueavg_2016.csv",header = T)
parkfactors<-read.csv("./raw/fangraphs_parkfactors_2015.csv",header = T)
players<-read.csv("./raw/fangraphs_player_2015-2.csv",header = T)
al_league<-read.csv("./raw/fangraphs_al_leagueavg.csv",header = T)
nl_league<-read.csv("./raw/fangraphs_nl_leagueavg.csv",header = T)
#map_team_league<-read.csv("./mapping/team_league_map.csv",header = T)


pitch<-read.csv("./raw/fangraphs_player_pitchingex.csv",header = T)
parkfactors<-read.csv("./raw/fangraphs_parkfactors_2015.csv",header = T)
#use this for pitching:
#parkfactors<-read.csv("./raw/fangraphs_parkfactors_2008.csv",header = T)


#rename variables

leagueavg<- rename(leagueavg,Season=ï..Season)
parkfactors<- rename(parkfactors,Season=ï..Season)
players<- rename(players,Name=ï..Name)
al_league<- rename(al_league,Season=ï..Season)
nl_league<- rename(nl_league,Season=ï..Season)


pitch<- rename(pitch,Season=ï..Season)


colnames(leagueavg)<- paste("lg_",colnames(leagueavg),sep = "")
leagueavg<- rename(leagueavg,Season=lg_Season)

colnames(parkfactors)<- paste("pfct_",colnames(parkfactors),sep = "")
parkfactors<- rename(parkfactors,Season=pfct_Season,Team=pfct_Team)

##merge AL NL League Stats
al_league$League="AL"
nl_league$League="NL"
alnl_league<- bind_rows(al_league,nl_league)%>%arrange(.,desc(Season))



#clean player data

players<-arrange(players, playerid)
players$Season = 2015

#join player data with parkfactors to get home team factors
players<- left_join(players,parkfactors, by=c("Season"="Season","Team"="Team"))
players <- left_join(players,leagueavg,by=c("Season"="Season"))


pitch<-left_join(pitch,parkfactors, by=c("Season"="Season","Team"="Team"))

pitch<- left_join(pitch,leagueavg,by=c("Season"="Season"))


#join team league mapping to players

players <- left_join(players,map_team_league, by=c("Team"="Team"))
pitch<- left_join(pitch,map_team_league, by=c("Team"="Team"))

#join players to alnl_league stats 

players <- select(alnl_league,Season,League,wRC,PA)%>%rename(.,alnl_wRC=wRC,alnl_PA=PA)%>%left_join(players,.,by=c("Season"="Season","League"="League"))



###########################
#Calculating WAR 
###########################



  ###########################
  #Position Players 
  ###########################

          #calculate Batting Runs
          players$BattingRuns = players$wRAA+(players$lg_R.PA-((players$pfct_Basic/100)*players$lg_R.PA))*players$PA + (players$lg_R.PA-(players$alnl_wRC/players$alnl_PA))*players$PA
          
           #change formatting for batting runs to 3 decimals (later we will want to get rid of this when calc WAR)
          #players$BattingRuns=format(round(players$BattingRuns,3),nsmall = 3)
          
          
          #calculate Base Running Runs
          
          players$BaseRunning = players$UBR+players$wSB+players$wGDP
          
          #calculate fielding 
              # for non catchers UZR
              #Catchers rSB and RPP
            
          #positional Adjustment
          
          #calculate war for position players
          players$WAR_Calc = ((players$BattingRuns+players$BaseRunning+players$Fld+players$Pos+players$Lg+players$Rep)/players$lg_R.W)

#compare Fangraphs WAR to Calculated WAR  
war_comp <- players %>% select (.,playerid,Name,Team,Season,WAR,WAR_Calc)
war_comp$diff = war_comp$WAR-war_comp$WAR_Calc
war_comp$WAR_CalcRd = round(war_comp$WAR_Calc,digits=1)
war_comp$diffRd = war_comp$WAR-war_comp$WAR_CalcRd


#Add in Aging Curve
players_forcast<- players %>% select(.,playerid,Name,Age,Season,wRAA,lg_R.PA,pfct_Basic,PA,UBR,wGDP,wSB,alnl_wRC,alnl_PA,BattingRuns,BaseRunning,Fld,Pos,Lg,Rep,lg_R.W,WAR,WAR_Calc)

aging_curve<- map_age %>% select(.,Age,ERA.Multiplier,Delta.R.PA,NetHitting,NetPitching,PitchAdd) %>% distinct(.)
#AGE PLAYERS 1 YEAR
players_forcast$Age=players_forcast$Age+1
players_forcast$Season=players_forcast$Season+1

#JOIN AGING CURVE
players_forcast <- players_forcast%>% left_join(.,aging_curve,by=c("Age"))

#APPLY AGING CURVE
players_forcast$PA_aged=players_forcast$PA*(1+players_forcast$Delta.R.PA)


#RECALCULATE WAR FOR NEW YEAR
    #calculate Batting Runs
    players_forcast$BattingRuns_Aged = players_forcast$wRAA+(players_forcast$lg_R.PA-((players_forcast$pfct_Basic/100)*players_forcast$lg_R.PA))*players_forcast$PA_aged + (players_forcast$lg_R.PA-(players_forcast$alnl_wRC/players_forcast$alnl_PA))*players_forcast$PA_aged
    #calculate Base Running Runs
    players_forcast$BaseRunning_Aged = players_forcast$UBR+players_forcast$wSB+players_forcast$wGDP
    #calculate war for position players
    players_forcast$WAR_Calc_Aged = ((players_forcast$BattingRuns_Aged+players_forcast$BaseRunning_Aged+players_forcast$Fld+players_forcast$Pos+players_forcast$Lg+players_forcast$Rep)/players_forcast$lg_R.W)



          
    ###########################
    #Pitchers  
    ###########################


    pitch_ex <- pitch %>% filter(.,Name=="Felix Hernandez")
    
    pitch_ex$FIP.Scaled =  ((4.78-pitch_ex$FIP)/4.78)*((200.67/2)*0.96)
      
      1-(4.78/pitch_ex$FIP) 
      
    pitch_ex$FIP.Scaled 
    
    pitch_ex$FIP.Calc = ((13*pitch_ex$HR )+(3*(pitch_ex$BB+pitch_ex$HBP))-(2*pitch_ex$K.))/(pitch_ex$IP+3.132)
    
    



