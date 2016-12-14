#CREATES OLAP FOR BASEBALL CARMELO#

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
battinglist_sum <- c("G","AB","R","H","X2B","X3B","HR","RBI","BB","SO","SB")
battinglist_avg <- c("BA","SlugPct","OBP")


Batting_Stats <- battingStats(data = Lahman::Batting, 
                              idvars = c("playerID", "yearID", "stint", "teamID", "lgID"), 
                              cbind = TRUE)

batting <- select(Batting_Stats, playerID, yearID,one_of(battinglist_sum),one_of(battinglist_avg))

#summarize by playerid and year

batting_sum <-batting %>% select(.,one_of(group),one_of(battinglist_sum))%>% group_by(.,playerID,yearID)%>% summarise_each(.,funs(sum(.,na.rm = T))) %>%ungroup(.)

batting_avg <- batting %>% select(.,one_of(group),one_of(battinglist_avg))%>% group_by(.,playerID,yearID)%>% summarise_each(.,funs(mean(.,na.rm = T))) %>%ungroup(.) %>% mutate(.,ISO = SlugPct-BA)

batting2 <-  full_join(batting_sum,batting_avg,by=group)%>% mutate(.,PlayerYearID=paste(playerID,"-",yearID,sep=""))


#get pitching stats


pitchinglist_sum <-  c("W","L","G","GS","CG","IPouts","H","SO","BB","SHO","SV")
pitchinglist_avg <- c("ERA")

pitching <- select(Pitching,one_of(group),one_of(pitchinglist_sum),one_of(pitchinglist_avg))

pitching_sum <- pitching %>% select(., one_of(group),one_of(pitchinglist_sum)) %>% group_by(.,playerID,yearID) %>% summarise_each(.,funs(sum(.,na.rm=T))) %>% ungroup(.)
pitching_avg <- pitching %>% select(., one_of(group),one_of(pitchinglist_avg)) %>% group_by(.,playerID,yearID) %>% summarise_each(.,funs(mean(.,na.rm=T))) %>% ungroup(.)

pitching2 <-  full_join(pitching_sum,pitching_avg,by=group) %>% mutate(.,W_Pct= W/(W+L),Inn = IPouts/3,PlayerYearID=paste(playerID,"-",yearID,sep=""),BB_9=(BB*9)/(IPouts/3),SO_9=(SO*9)/(IPouts/3)) %>% select(.,-IPouts)



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
# btt_weight<- c(20,75,10,15,5,4,2,10,25,150,20,.001,.002)  
    #btt_weight<- c(20,75,10,15,5,4,2,10,25,150,20,1/.001,1/.002,1/.002,1/.5) #DH -Updated 11/8/16 previous weights were incorrrect ##added btt_WAR_def
    #btt_sts<- c("G","AB","R","H","X2B","X3B","HR","RBI","BB","SO","SB", "BA","SlugPct","OBP","WAR_def") ##added btt_WAR_def

btt_weight<- c(10,12,2,2,8,10,12) #DH -Updated 11/8/16 previous weights were incorrrect ##added btt_WAR_def
btt_sts<- c("G","AB","BA","SlugPct","OBP","ISO","WAR_def") ##added btt_WAR_def
map_btt_weight <- data.frame(btt_sts,btt_weight) %>% spread(.,btt_sts,btt_weight)
colnames(map_btt_weight)<- paste("btt_",colnames(map_btt_weight),"_weight",sep = "")


#create pitching weight mapping
# ptch_weight<- c(1,2,.002,.02,10,20,20,50,50,30,10,5,3)  
    #ptch_weight<- c(1,2,.002,.02,10,20,20,50,50,30,10,5,3) #DH -Updated 11/1/16 previous weights were incorrrect
    #ptch_sts<- c("W","L","W_Pct","ERA","G","GS","CG","Inn","H","SO","BB","SHO","SV")

ptch_weight<- c(20,10,50,10,10,5) #DH -Updated 11/1/16 previous weights were incorrrect
ptch_sts<- c("G","GS","Inn","SO_9","BB_9","SV")
map_ptch_weight <- data.frame(ptch_sts,ptch_weight) %>% spread(.,ptch_sts,ptch_weight)
colnames(map_ptch_weight)<- paste("ptch_",colnames(map_ptch_weight),"_weight",sep = "")


##subset bbref war datasets
war_batting2 <- war_batting %>% select(.,playerID,BBRef_playerID,yearID,pitcher,WAR,WAR_def) %>% group_by(.,playerID,BBRef_playerID,yearID,pitcher)%>%summarise_each(.,funs(sum(.,na.rm=T)))%>%ungroup(.)##added btt_WAR_def
war_pitching2 <- war_pitching %>% select(.,playerID,BBRef_playerID,yearID,WAR)  %>% group_by(.,playerID,BBRef_playerID,yearID)%>%summarise_each(.,funs(sum(.,na.rm=T)))%>%ungroup(.)
war_all <- full_join(war_batting2,war_pitching2, by=c("playerID","BBRef_playerID","yearID"))
war_all$WAR <- ifelse(war_all$pitcher=="Y",war_all$WAR.y,war_all$WAR.x)
war_all <-  mutate(war_all,PlayerYearID=paste(playerID,yearID,sep="-")) %>% select(., -WAR.x, -WAR.y) %>%rename(.,btt_WAR_def=WAR_def) ##added btt_WAR_def


#add player years and primary position to player table & create age
player_yrs <- select(batting2,playerID, yearID) %>% full_join(.,select(pitching2,playerID,yearID))

players<- left_join(players,player_yrs) %>% mutate(.,PlayerYearID=paste(playerID,"-",yearID,sep=""),nameFirstLast=paste(nameFirst,nameLast),BBRefYearID=paste(bbrefID,"-",yearID,sep=""))%>% mutate(.,season_start =as.Date(paste(as.character(yearID),"04","01",sep="-")),birthDate=as.Date(birthDate)) %>%mutate(.,age=as.numeric(round((difftime(season_start,birthDate,unit="weeks")/52.25),1))) %>% left_join(.,player_pos,by="playerID") %>% select(.,-season_start)



#anyDuplicated(players)


#check for duplicates
print(paste("Dateframe 'players' duplicates:",anyDuplicated(players)))
print(paste("Dateframe 'batting2' duplicates:",anyDuplicated(batting2)))
print(paste("Dateframe 'pitching2' duplicates:",anyDuplicated(pitching2)))
print(paste("Dateframe 'war_all' duplicates:",anyDuplicated(war_all)))
#war_all_dups<- war_all %>% filter(.,duplicated(war_all)==T)


final_btt_vars <-  paste("btt_",c("G","AB","BA","SlugPct","OBP","ISO"),sep="")
final_ptch_vars<-  paste("ptch_",c("G","GS","Inn","SV","BB_9","SO_9"),sep="")
#drop playerid and yearid 
batting3<-batting2%>%select(.,PlayerYearID,one_of(final_btt_vars)) #%>% group_by(.,PlayerYearID) %>% mutate_each(funs(replace(., is.na(.), 0)))
pitching3<-pitching2%>%select(.,PlayerYearID,one_of(final_ptch_vars))#%>% group_by(.,PlayerYearID) %>% mutate_each(funs(replace(., is.na(.), 0)))
war_all2<-war_all%>%select(.,PlayerYearID,WAR,btt_WAR_def)#%>% group_by(.,PlayerYearID) %>% mutate_each(funs(replace(., is.na(.), 0))) ##added btt_WAR_def

#Join Datasets
olap <- left_join(players,batting3, by="PlayerYearID") %>%left_join(.,pitching3, by="PlayerYearID")%>%left_join(.,war_all2,by="PlayerYearID")%>%merge(.,map_btt_weight)%>%merge(.,map_ptch_weight)%>%left_join(.,map_pos_weight,by=c("primary_pos"="pos")) 

#load marcels results btt
load("./trn/marcels_results_btt_war.RData")

olap  <- left_join(olap,marcels_results) 

#zero fill
grp_by <- colnames(players)
olap<-olap%>%  group_by_(.dots=grp_by)%>% mutate_each(funs(replace(., is.na(.), 0)))%>%ungroup(.)


save(olap,file="./trn/olap_cut.RData")





