source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/fill_gap.R")
get_package(c("dplyr","Lahman","lubridate","Sabermetrics","imputeTS"))




#set  data filters
  year_filter=1996
  ab_filter=130
  innings_filter=50

###############################################################

str(Master)

#select data
  master<-Master%>%select(.,playerID,bbrefID,retroID,nameFirst,nameLast,nameGiven,birthYear,birthMonth,birthDay,debut,height,bats,weight,debut,finalGame)%>%filter(.,year(debut)>=year_filter)
  
  batting<-Batting%>%filter(.,yearID>=year_filter)
  
  fielding<-Fielding%>%filter(.,yearID>=year_filter)
  
  pitching<-Pitching%>%filter(.,yearID>=year_filter)
  
  players<-master%>%select(.,playerID,birthYear,debut)

#reanme columns

  colnames(batting)<- paste("btt_",colnames(batting),sep = "")
  batting<- rename(batting,playerID=btt_playerID,yearID=btt_yearID,teamID=btt_teamID,lgID=btt_lgID)
  
  colnames(fielding)<- paste("fldg_",colnames(fielding),sep = "")
  fielding<- rename(fielding,playerID=fldg_playerID,yearID=fldg_yearID,teamID=fldg_teamID,lgID=fldg_lgID,POS=fldg_POS)
  
  colnames(pitching)<- paste("ptch_",colnames(pitching),sep = "")
  pitching<- rename(pitching,playerID=ptch_playerID,yearID=ptch_yearID,teamID=ptch_teamID,lgID=ptch_lgID)
  
#join data
  #t1_batting<-players%>%left_join(.,batting,by=c("playerID"))
  #t1_pitching<-players%>%left_join(.,fielding,by=c("playerID")) 
  #t1_fielding <- players %>% left_join(.,pitching,by=c("playerID"))
  
  #add position to pitching
  pitching$POS="P"
  
  
  
  w1<-full_join(batting,fielding,by=c("playerID","yearID","teamID","lgID"))%>%full_join(.,pitching,by=c("playerID","yearID","teamID","lgID","POS")) %>% right_join(.,players,by="playerID")

  
#calculate age

  w1$Age=w1$yearID - w1$birthYear
  w1$debutAge=year(w1$debut) - w1$birthYear



#calcualte WAR

# position players:
#WAR = (Batting Runs + Base Running Runs + Fielding Runs + Positional Adjustment + League Adjustment +Replacement Runs) / (Runs Per Win)

#Batting Runs = wRAA + (lgR/PA – (PF*lgR/PA))*PA + (lgR/PA – (AL or NL non-pitcher wRC/PA))*PA

#Base Running = UBR + wSB + wGDP
  
  #calc batting PA
  w1$btt_PA=(w1$btt_AB+w1$btt_BB+w1$btt_HBP+w1$btt_SH+w1$btt_SF)
  #calc batting Singles
  w1$btt_X1B=(w1$btt_H-w1$btt_X2B-w1$btt_X3B-w1$btt_HR)

#Calc woba
for (i in 1:nrow(w1)) {
  w1[i,]$woba= woba(w1[i,]$yearID,w1[i,]$btt_AB,w1[i,]$btt_BB,w1[i,]$btt_IBB,w1[i,]$btt_HBP,w1[i,]$btt_X1B,w1[i,]$btt_X2B,w1[i,]$btt_X3B,w1[i,]$btt_HR,w1[i,]$btt_SF)
  
}
#check woba
test<-w1[112,]

woba(test$yearID,test$btt_AB,test$btt_BB,test$btt_IBB,test$btt_HBP,test$btt_X1B,test$btt_X2B,test$btt_X3B,test$btt_HR,test$btt_SF)

#calc wrAA
for (i in 1:nrow(w1)) {
  w1[i,]$wraa= wraa(w1[i,]$woba,year=w1[i,]$yearID,PA=w1[i,]$btt_PA)
}
str(Teams)
#get league and team averages and parkfactors - THIS IS MISSING IBB WHICH WE NEED TO CALCULATE LG_wOBA!!!!
league<-Teams%>%filter(.,yearID>=year_filter) %>% select(.,yearID,lgID,teamID,Rank,G,Ghome,W,L,R,AB,H,X2B,X3B,HR,BB,SO,SB,CS,HBP,SF,RA,ER,ERA,CG,SHO,SV,IPouts,HA,HRA,BBA,SOA,E,DP,FP,BPF,PPF)
#make table
alnl_table<-league%>%select(.,-teamID)
team_table<-league%>%select(.,-lgID)

#rename
colnames(alnl_table)<- paste("alnl_",colnames(alnl_table),sep = "")
alnl_table<- rename(alnl_table, yearID=alnl_yearID,lgID=alnl_lgID)

colnames(team_table)<- paste("team_",colnames(team_table),sep = "")
team_table<- rename(team_table,yearID=team_yearID,teamID=team_teamID)


#calc alnl PA and singles and R/PA
alnl_table$alnl_PA=(alnl_table$alnl_AB+alnl_table$alnl_BB+alnl_table$alnl_HBP+alnl_table$alnl_SH+alnl_table$alnl_SF)
alnl_table$alnl_X1B=(alnl_table$alnl_H-alnl_table$alnl_X2B-alnl_table$alnl_X3B-alnl_table$alnl_HR)

alnl_table$alnl_R.PA= alnl_table$alnl_R/alnl_table$alnl_PA


#summarise
alnl_table<-alnl_table%>% group_by(.,yearID,lgID)%>% summarise_each(.,funs(mean(.,na.rm=F))) %>%ungroup(.)

team_table<-team_table%>% group_by(.,yearID,teamID)%>% summarise_each(.,funs(mean(.,na.rm=F)))%>%ungroup(.)

#join ALNL data to player data

w2<-w1 %>% left_join(.,alnl_table,by=c("yearID","lgID"))

#join team data to player data

w3<-w2 %>% left_join(.,team_table,by=c("yearID","teamID"))



#Batting Runs = wRAA + (lgR/PA – (PF*lgR/PA))*PA + (lgR/PA – (AL or NL non-pitcher wRC/PA))*PA

#Calc leauge woba
for (i in 1:nrow(w3)) {
  w3[i,]$woba= woba(w3[i,]$yearID,w3[i,]$alnl_AB,w3[i,]$alnl_BB,w3[i,]$alnl_IBB,w3[i,]$alnl_HBP,w3[i,]$alnl_X1B,w3[i,]$alnl_X2B,w3[i,]$alnl_X3B,w3[i,]$alnl_HR,w3[i,]$alnl_SF)
  
}




  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #roll up
  #batting - all vars can be summed
  #fielding - all vars can be summed except for ZR
  #pitching - all vars can be summed except: BAOpp,ERA,
  


