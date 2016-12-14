# Joins war_batting and war_pitching datasets 


source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","Lahman","lubridate","sqldf","zoo","ggplot2","forecast","sabermetrics"))

#set  data filters
year_filter= 1996 #1996
batt_PA_filter=100 #change to 100 PA
innings_filter=40 #innings filter / 3  = around 40
obs_filter=100

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
#apply Filters


t2_btt<- t1 %>% filter(.,yearID>=year_filter,btt_pitcher=="N",btt_PA>=batt_PA_filter)%>% select(.,BBRef_playerID,age,WAR) %>% group_by(BBRef_playerID,age) %>% summarise(.,WAR=sum(WAR,na.rm=F)) %>%ungroup(.) %>%arrange(.,BBRef_playerID,age)

t2_ptch<- t1 %>% filter(.,yearID>=year_filter,((ptch_IPouts_start+ptch_IPouts_relief)/3) >=innings_filter,btt_pitcher!="N")%>% select(.,BBRef_playerID,age,WAR) %>% group_by(BBRef_playerID,age) %>% summarise(.,WAR=sum(WAR,na.rm=F)) %>%ungroup(.) %>%arrange(.,BBRef_playerID,age)




# BATTING


#Filter out Ages below observation limit
t4_btt <- t2_btt%>% select(.,age,WAR) %>% group_by(age) %>% summarise(.,Avg_WAR=mean(WAR,na.rm=T),Median_WAR=median(WAR,na.rm=T),Age_Obs=n()) %>%ungroup(.) %>% filter(.,Age_Obs>=obs_filter)

#create % change variables
t4_btt$Lag_Avg_WAR<-lag(t4_btt$Avg_WAR,n=1,order_by = t4_btt$age)
t4_btt$Lag_Median_WAR<-lag(t4_btt$Median_WAR,n=1,order_by = t4_btt$age)


t4_btt$Pct_Chg_Avg_WAR= (t4_btt$Avg_WAR - t4_btt$Lag_Avg_WAR)/abs(t4_btt$Lag_Avg_WAR)
t4_btt$Pct_Chg_Median_WAR= (t4_btt$Median_WAR - t4_btt$Lag_Median_WAR)/abs(t4_btt$Lag_Median_WAR)

# PITCHING

#Filter out Ages below observation limit
t4_ptch <- t2_ptch%>% select(.,age,WAR) %>% group_by(age) %>% summarise(.,Avg_WAR=mean(WAR,na.rm=T),Median_WAR=median(WAR,na.rm=T),Age_Obs=n()) %>%ungroup(.) %>% filter(.,Age_Obs>=obs_filter)

t4_ptch$Lag_Avg_WAR<-lag(t4_ptch$Avg_WAR,n=1,order_by = t4_ptch$age)
t4_ptch$Lag_Median_WAR<-lag(t4_ptch$Median_WAR,n=1,order_by = t4_ptch$age)


t4_ptch$Pct_Chg_Avg_WAR= (t4_ptch$Avg_WAR - t4_ptch$Lag_Avg_WAR)/abs(t4_ptch$Lag_Avg_WAR)
t4_ptch$Pct_Chg_Median_WAR= (t4_ptch$Median_WAR - t4_ptch$Lag_Median_WAR)/abs(t4_ptch$Lag_Median_WAR)


#forcast average war

t5_btt <- select(t2_btt,age,WAR)

ts_btt <- ts(t5_btt$WAR,start = min(t5_btt$age),end = max(t5_btt$age),frequency = 1)

plot(ts_btt,type='o',col='blue')
  axis(1,tck=1)

library("tseries")
adf=adf.test(ts_btt)
adf
kpss=kpss.test(ts_btt)
kpss

#non stationalry data differencing is required
ndiffs(ts_btt) #number of times to difference data

diff_btt = ts_btt
i=0
while (i<=ndiffs(diff_btt)) {
  diff_btt = diff(diff_btt) #difference data
  i=i+1
}

#test differenced data
diff_adf=adf.test(diff_btt)
diff_adf
diff_kpss = kpss.test(diff_btt)
diff_kpss


#id seasonality/trend
stl = stl(diff_btt,s.window = "periodic") # this means no seasonality

forecast(auto.arima(diff_btt))
plot(forecast(auto.arima(diff_btt)))





