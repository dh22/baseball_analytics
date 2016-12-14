# Joins war_batting and war_pitching datasets 


source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","Lahman","lubridate","sqldf","zoo","ggplot2"))

#set  data filters
year_filter=1890
# ab_filter=130
# innings_filter=50
obs_filter=500

#load datasets
load("./trn/war_batting.RData")
load("./trn/war_pitching.RData")

#rename columns w prefix
colnames(war_batting)<- paste("btt_",colnames(war_batting),sep = "")


war_batting<- rename(war_batting,playerID=btt_playerID,
                                  BBRef_playerID=btt_BBRef_playerID,
                                  BBRef_Name=btt_BBRef_Name,
                                  nameFirst=btt_nameFirst,
                                  nameLast=btt_nameLast,
                                  yearID=btt_yearID,
                                  teamID=btt_teamID,
                                  lgID=btt_lgID,
                                  age=btt_age)

colnames(war_pitching)<- paste("ptch_",colnames(war_pitching),sep = "")

war_pitching<-rename(war_pitching,playerID=ptch_playerID,
                                  BBRef_playerID=ptch_BBRef_playerID,
                                  BBRef_Name=ptch_BBRef_Name,
                                  nameFirst=ptch_nameFirst,
                                  nameLast=ptch_nameLast,
                                  yearID=ptch_yearID,
                                  teamID=ptch_teamID,
                                  lgID=ptch_lgID,
                                  age=ptch_age)


#full join

t1<-full_join(war_batting,war_pitching,by=c("playerID","BBRef_playerID","BBRef_Name","nameFirst","nameLast","yearID","teamID","age","lgID"))

#if pitcher = Y then use pitching WAR

t1$WAR<-ifelse(t1$btt_pitcher=="Y",t1$ptch_WAR,ifelse(is.na(t1$btt_pitcher),t1$ptch_WAR,t1$btt_WAR))

# select only playerid, age, pitcher/nonpitcher, WAR

t2_btt<- t1 %>% filter(.,yearID>=year_filter,btt_pitcher=="N")%>% select(.,BBRef_playerID,age,WAR) %>% group_by(BBRef_playerID,age) %>% summarise(.,WAR=sum(WAR,na.rm=F)) %>%ungroup(.) %>%arrange(.,BBRef_playerID,age)

t2_ptch<- t1 %>% filter(.,yearID>=year_filter,btt_pitcher!="N")%>% select(.,BBRef_playerID,age,WAR) %>% group_by(BBRef_playerID,age) %>% summarise(.,WAR=sum(WAR,na.rm=F)) %>%ungroup(.) %>%arrange(.,BBRef_playerID,age)



#Omit missing data
# BATTING
t4_btt <- t2_btt%>% select(.,age,WAR) %>% group_by(age) %>% summarise(.,Avg_WAR=mean(WAR,na.rm=T),Median_WAR=median(WAR,na.rm=T),Age_Obs=n()) %>%ungroup(.) %>% filter(.,Age_Obs>=obs_filter)

t4_btt$Lead_Avg_WAR<-lead(t4_btt$Avg_WAR,n=1,order_by = t4_btt$age)
t4_btt$Lead_Median_WAR<-lead(t4_btt$Median_WAR,n=1,order_by = t4_btt$age)

t4_btt$Pct_Chg_Avg_WAR= (t4_btt$Lead_Avg_WAR-t4_btt$Avg_WAR)/t4_btt$Avg_WAR
t4_btt$Pct_Chg_Median_WAR= (t4_btt$Lead_Median_WAR-t4_btt$Median_WAR)/t4_btt$Median_WAR

# PITCHING
t4_ptch <- t2_ptch%>% select(.,age,WAR) %>% group_by(age) %>% summarise(.,Avg_WAR=mean(WAR,na.rm=T),Median_WAR=median(WAR,na.rm=T),Age_Obs=n()) %>%ungroup(.) %>% filter(.,Age_Obs>=obs_filter)

t4_ptch$Lead_Avg_WAR<-lead(t4_ptch$Avg_WAR,n=1,order_by = t4_ptch$age)
t4_ptch$Lead_Median_WAR<-lead(t4_ptch$Median_WAR,n=1,order_by = t4_ptch$age)

t4_ptch$Pct_Chg_Avg_WAR= (t4_ptch$Lead_Avg_WAR-t4_ptch$Avg_WAR)/t4_ptch$Avg_WAR
t4_ptch$Pct_Chg_Median_WAR= (t4_ptch$Lead_Median_WAR-t4_ptch$Median_WAR)/t4_ptch$Median_WAR





#plot data
# BATTING
chart_t4_btt<- ggplot(t4_btt,aes(x=age),color)+
  geom_line(aes(y=Pct_Chg_Avg_WAR),color="red") +
  geom_point(aes(y=Pct_Chg_Avg_WAR),color="red")+
  geom_line(aes(y=Pct_Chg_Median_WAR),color="blue")+
  geom_point(aes(y=Pct_Chg_Median_WAR),color="blue")+
  scale_x_continuous(breaks = round(seq(min(t4_btt$age), max(t4_btt$age), by = 1),1))+ggtitle("Batting- WAR Aging Curve Mean vs Median") 

chart_t4_btt_age_dist <- ggplot(t4_btt,aes(x=age,y=Age_Obs))+
  geom_point(color="purple")+
  geom_line(color="purple")+
  scale_x_continuous(breaks = round(seq(min(t4_btt$age), max(t4_btt$age), by = 1),1))+scale_y_continuous(breaks = round(seq(0, max(t4_btt$Age_Obs), by = 500),0))
# +ggtitle("Batting - WAR Age Distribution")




# PITCHING
chart_t4_ptch<- ggplot(t4_ptch,aes(x=age),color)+
  geom_line(aes(y=Pct_Chg_Avg_WAR),color="red") +
  geom_point(aes(y=Pct_Chg_Avg_WAR),color="red")+
  geom_line(aes(y=Pct_Chg_Median_WAR),color="blue")+
  geom_point(aes(y=Pct_Chg_Median_WAR),color="blue")+
  scale_x_continuous(breaks = round(seq(min(t4_ptch$age), max(t4_ptch$age), by = 1),1))+ggtitle("Pitching- WAR Aging Curve Mean vs Median") 

chart_t4_ptch_age_dist <- ggplot(t4_ptch,aes(x=age,y=Age_Obs))+
  geom_point(color="purple")+
  geom_line(color="purple")+
  scale_x_continuous(breaks = round(seq(min(t4_ptch$age), max(t4_ptch$age), by = 1),1))+scale_y_continuous(breaks = round(seq(0, max(t4_ptch$Age_Obs), by = 500),0))
# +ggtitle("Pitching - WAR Age Distribution")





multiplot(chart_t4_btt_age_dist,chart_t4_btt)

multiplot(chart_t4_ptch_age_dist,chart_t4_ptch)




#use avg for aging curve chart
# aging_curve<-

# cluster by - age at first professional season, 