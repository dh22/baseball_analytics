# Joins war_batting and war_pitching datasets 


source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","Lahman","lubridate","sqldf","zoo","ggplot2"))

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


#plot data
# BATTING
chart_t4_btt<- ggplot(t4_btt,aes(x=age),color)+
  geom_line(size=1,aes(y=Pct_Chg_Avg_WAR),color="red") +
  geom_point(aes(y=Pct_Chg_Avg_WAR),color="red")+
  geom_line(size=1,aes(y=Pct_Chg_Median_WAR),color="blue")+
  geom_point(aes(y=Pct_Chg_Median_WAR),color="blue")+
  scale_x_continuous(breaks = round(seq(min(t4_btt$age), max(t4_btt$age), by = 1),1))+ggtitle("Batting- WAR Aging Curve Mean vs Median") 

chart_t4_btt_age_dist <- ggplot(t4_btt,aes(x=age,y=Age_Obs))+
  geom_point(color="purple")+
  geom_line(size=1,color="purple")+
  scale_x_continuous(breaks = round(seq(min(t4_btt$age), max(t4_btt$age), by = 1),1))+scale_y_continuous(breaks = round(seq(0, max(t4_btt$Age_Obs), by = 100),0))
# +ggtitle("Batting - WAR Age Distribution")




# PITCHING
chart_t4_ptch<- ggplot(t4_ptch,aes(x=age),color)+
  geom_line(size=1,aes(y=Pct_Chg_Avg_WAR),color="red") +
  geom_point(aes(y=Pct_Chg_Avg_WAR),color="red")+
  geom_line(size=1,aes(y=Pct_Chg_Median_WAR),color="blue")+
  geom_point(aes(y=Pct_Chg_Median_WAR),color="blue")+
  scale_x_continuous(breaks = round(seq(min(t4_ptch$age), max(t4_ptch$age), by = 1),1))+ggtitle("Pitching- WAR Aging Curve Mean vs Median")

chart_t4_ptch_age_dist <- ggplot(t4_ptch,aes(x=age,y=Age_Obs))+
  geom_point(color="purple")+
  geom_line(size=1,color="purple")+
  scale_x_continuous(breaks = round(seq(min(t4_ptch$age), max(t4_ptch$age), by = 1),1))+scale_y_continuous(breaks = round(seq(0, max(t4_ptch$Age_Obs), by = 100),0))
# +ggtitle("Pitching - WAR Age Distribution")



multiplot(chart_t4_btt_age_dist,chart_t4_btt)
multiplot(chart_t4_ptch_age_dist,chart_t4_ptch)


#output data 
# write.csv(t4_btt,"./output/btt_war.csv",na="",row.names = F)
# write.csv(t4_ptch,"./output/ptch_war.csv",na="",row.names = F)


#GRAB SAMPLE PLAYER

sample_playerid = "pujolal01"
sql<- paste("select BBRef_playerid,BBRef_Name,age, sum(war) as war
 from t1 
where BBRef_playerid = '",sample_playerid,"'
group by age;",sep="")

sample_data<-sqldf(sql)



#join 

join<-t4_btt %>% select(.,age,Pct_Chg_Avg_WAR)%>% filter(.,t4_btt$age>=min(sample_data$age)) %>% left_join(.,sample_data, by=c("age"))
#need to apply this calcuation in a loop
join$forcastedWAR=NA

for (i in 2:nrow(join)) {
    lag=i-1
    print(lag)
    join$forcastedWAR[i]=ifelse(lag==0,NA,ifelse(lag==2,(join$Pct_Chg_Avg_WAR[lag]*join$war[lag])+join$war[lag],(join$Pct_Chg_Avg_WAR[lag]*join$forcastedWAR[lag])+join$forcastedWAR[lag] ))
      
      # join$Pct_Chg_Avg_WAR[lag]*join$war[lag]
}




join$Pct_Chg_Avg_WAR[1]*join$war[1]
join$Pct_Chg_Avg_WAR[4]*join$forcastedWAR[4]


# Chart Forecast
forecast_chart<- ggplot(join,aes(x=age),color)+
  geom_line(size=1,aes(y=war),color="red") +
  geom_point(aes(y=war),color="red")+
  geom_line(size=1,aes(y=forcastedWAR),color="blue")+
  geom_point(aes(y=forcastedWAR),color="blue")+
  scale_y_continuous(breaks=round(seq(min(join$war,na.rm = T), max(join$war,na.rm = T)*1.5, by = .2),1))+
  scale_x_continuous(breaks = round(seq(min(join$age), max(join$age), by = 1),1))+ggtitle(paste("Forcasted WAR vs Actual for ",sample_data$BBRef_Name[1]," - BBRef ID: ",sample_data$BBRef_playerID[1], sep=""))

forecast_chart





# CONVERT TO TIMESERIES FOR AREN
get_package(c("TTR","forecast"))
# library(devtools)
# install_github('sinhrks/ggfortify')


t5_btt <- t4_btt %>% select(.,age,Pct_Chg_Avg_WAR) #%>% filter(.,is.na(Pct_Chg_Avg_WAR)==F)
t5_ptch <- t4_ptch %>% select(.,age,Pct_Chg_Avg_WAR) #%>% filter(.,is.na(Pct_Chg_Avg_WAR)==F)

ts_btt <-ts(t5_btt[,2],start = min(t5_btt$age))
ts_ptch <- ts(t5_ptch[,2],start = min(t5_ptch$age))

#use avg for aging curve chart

ts_btt_SMA<- SMA(ts_btt,n=3)
ts_ptch_SMA<- SMA(ts_ptch,n=3)

plot(ts_btt_SMA)

arima_btt<-auto.arima(ts_btt)
arima_ptch<-auto.arima(ts_ptch)


arima_btt<-Arima(ts_btt,order=c(3,2,0))
#summary(arima_btt)
forecast_btt<-forecast(arima_btt)
plot(forecast_btt)

forecast_ptch<-forecast(arima_ptch)


plot(forecast_ptch,h=8)

autoplot.acf(forecast_btt)
#######################################
# select id = younger03 sum war by age and filter to a single year and use the forcast to determine how the player will age. compare to actuals
#######################################

player_test <- t1 %>% select(.,BBRef_playerID,BBRef_Name,age,WAR) %>% filter(.,BBRef_playerID=="younger03") %>% group_by(.,BBRef_playerID,BBRef_Name,age)%>% summarise(.,WAR = sum(WAR,na.rm=T))%>%ungroup(.) 
ts_player_test <- player_test %>% select(.,WAR)%>%ts(.)

refit<-Arima(player_test,model=arima_btt)

ggplot(ts_btt_SMA,aes(x=time(ts_btt_SMA)),color)+
  geom_line(size=1,aes(y=ts_btt_SMA[,2]),color="red") +
  geom_point(aes(y=Pct_Chg_Avg_WAR),color="red")+
  geom_line(size=1,aes(y=Pct_Chg_Median_WAR),color="blue")+
  geom_point(aes(y=Pct_Chg_Median_WAR),color="blue")+
  scale_x_continuous(breaks = round(seq(min(t4_ptch$age), max(t4_ptch$age), by = 1),1))+ggtitle("Pitching- WAR Aging Curve Mean vs Median")

multiplot(a,b)


lines(HoltWinters(data,gamma = F)$fitted,col="red")


plot(data, xlab="AGE", ylab = "Batting WAR % Change")
plot(diff(data),ylab=  "Diff Batting WAR % Change")
plot(log10(data),ylab=  "Log Batting WAR % Change")
plot(diff(log10(data)),ylab=  "Diff Log Batting WAR % Change")

get_package(c("forecast"))
ARIMAfit<- auto.arima(data,approximation = F,trace=F)
summary(ARIMAfit)

pred <- predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type="l",xlim=c(19,45),ylim=c(-2,2),xlab = "Age",ylab = " Batting WAR % Change")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+2*pred$se),col="orange")
lines(10^(pred$pred-2*pred$se),col="orange")

