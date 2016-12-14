#BASEBALL CARMELO#

source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","tidyr","Lahman","lubridate","sqldf","zoo","ggplot2","forecast","Sabermetrics"))


#This code brings in all datasets and creates an olap

#load BBRef daily war

load("./trn/war_batting.RData")
load("./trn/war_pitching.RData")


# get players and bio stats

players<-select(Master,playerID,retroID,bbrefID,nameFirst,nameLast,nameGiven,weight,height,bats,throws,debut,finalGame,birthDate)

#anyDuplicated(players)


#get batting stats
group <- c("playerID","yearID")
battinglist_sum <- c("G","AB","R","H","X2B","X3B","HR","RBI","IBB","SO","SB")
battinglist_avg <- c("BA","SlugPct")


Batting_Stats <- battingStats(data = Lahman::Batting, 
                              idvars = c("playerID", "yearID", "stint", "teamID", "lgID"), 
                              cbind = TRUE)

batting <- select(Batting_Stats, playerID, yearID,one_of(battinglist_sum),one_of(battinglist_avg))

#summarize by playerid and year

batting_sum <-batting %>% select(.,one_of(group),one_of(battinglist_sum))%>% group_by(.,playerID,yearID)%>% summarise_each(.,funs(sum(.,na.rm = T))) %>%ungroup(.)

batting_avg <- batting %>% select(.,one_of(group),one_of(battinglist_avg))%>% group_by(.,playerID,yearID)%>% summarise_each(.,funs(mean(.,na.rm = T))) %>%ungroup(.)

batting2 <-  full_join(batting_sum,batting_avg,by=group)%>% mutate(.,PlayerYearID=paste(playerID,"-",yearID,sep=""))


#get pitching stats


pitchinglist_sum <-  c("W","L","G","GS","CG","IPouts","H","SO","BB","SHO","SV")
pitchinglist_avg <- c("ERA")

pitching <- select(Pitching,one_of(group),one_of(pitchinglist_sum),one_of(pitchinglist_avg))

pitching_sum <- pitching %>% select(., one_of(group),one_of(pitchinglist_sum)) %>% group_by(.,playerID,yearID) %>% summarise_each(.,funs(sum(.,na.rm=T))) %>% ungroup(.)
pitching_avg <- pitching %>% select(., one_of(group),one_of(pitchinglist_avg)) %>% group_by(.,playerID,yearID) %>% summarise_each(.,funs(mean(.,na.rm=T))) %>% ungroup(.)

pitching2 <-  full_join(pitching_sum,pitching_avg,by=group) %>% mutate(.,W_Pct= W/G,Inn = IPouts/3,PlayerYearID=paste(playerID,"-",yearID,sep=""))



#rename columns w prefix 
colnames(batting2)<- paste("btt_",colnames(batting2),sep = "")


batting2<- rename(batting2,playerID=btt_playerID,
                  yearID=btt_yearID,
                  PlayerYearID=btt_PlayerYearID)

colnames(pitching2)<- paste("ptch_",colnames(pitching2),sep = "")

pitching2<- rename(pitching2,playerID=ptch_playerID,
                   yearID=ptch_yearID,
                   PlayerYearID=ptch_PlayerYearID)

#get player primary position

player_pos <- select(Fielding,playerID,POS,G) %>% filter(.,!POS %in% c("RF","CF","LF")) %>% group_by(.,playerID,POS) %>% summarise(.,psum=sum(G,na.rm=T))%>% ungroup(.) %>% arrange(.,playerID,desc(psum)) %>% group_by(.,playerID)%>% mutate(.,primary_pos = first(POS,order_by = c("playerID"))) %>% select(.,-POS,-psum) %>% distinct(.,.keep_all=T) %>% ungroup(.)

#create position weight mapping
pweight<-c(240,168,132,84,48,12,0)
pos<-c("C","SS","2B","3B","OF","1B","DH")
map_pos_weight <- data.frame(pos,pweight)

#create batting weight mapping
btt_weight<- c(20,75,10,15,5,4,2,10,25,150,20,.001,.002)  
btt_sts<- c("G","AB","R","H","X2B","X3B","HR","RBI","IBB","SO","SB", "BA","SlugPct")
map_btt_weight <- data.frame(btt_sts,btt_weight) %>% spread(.,btt_sts,btt_weight)

#create pitching weight mapping
ptch_weight<- c(1,2,.002,.02,10,20,20,50,50,30,10,5,3)  
ptch_sts<- c("W","L","W_Pct","ERA","G","GS","CG","Inn","H","SO","BB","SHO","SV")
map_ptch_weight <- data.frame(ptch_sts,ptch_weight) %>% spread(.,ptch_sts,ptch_weight)


#subset bbref war datasets
war_batting2 <- war_batting %>% select(.,playerID,BBRef_playerID,yearID,pitcher,WAR) 
war_pitching2 <- war_pitching %>% select(.,playerID,BBRef_playerID,yearID,WAR) 
war_all <- full_join(war_batting2,war_pitching2, by=c("playerID","BBRef_playerID","yearID"))
war_all$WAR <- ifelse(war_all$pitcher=="Y",war_all$WAR.y,war_all$WAR.x)
war_all <-  mutate(war_all,PlayerYearID=paste(playerID,yearID,sep="-")) %>% select(., -WAR.x, -WAR.y) 


#add player years and primary position to player table
player_yrs <- select(batting2,playerID, yearID) %>% full_join(.,select(pitching2,playerID,yearID))
players<- left_join(players,player_yrs) %>% mutate(.,PlayerYearID=paste(playerID,"-",yearID,sep=""),nameFirstLast=paste(nameFirst,nameLast),BBRefYearID=paste(bbrefID,"-",yearID,sep="")) %>% left_join(.,player_pos,by="playerID")

#anyDuplicated(players)



#check for duplicates
print(paste("Dateframe 'players' duplicates:",anyDuplicated(players)))
print(paste("Dateframe 'batting2' duplicates:",anyDuplicated(batting2)))
print(paste("Dateframe 'pitching2' duplicates:",anyDuplicated(pitching2)))


#drop playerid and yearid
batting3<-batting2%>%select(.,-playerID, -yearID)
pitching3<-pitching2%>%select(.,-playerID, -yearID)
war_all2<-war_all%>%select(.,PlayerYearID,WAR)

#Join Datasets
olap <- left_join(players,batting3, by="PlayerYearID") %>%left_join(.,pitching3, by="PlayerYearID")%>%left_join(.,war_all2,by="PlayerYearID")





