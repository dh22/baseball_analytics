#ALGORITHM FOR BASEBALL CARMELO#

source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","tidyr","Lahman","lubridate","sqldf","zoo","ggplot2","forecast","Sabermetrics","mc2d","weights"))

load("./trn/olap.RData")
olap_actuals <- olap
olap<-olap%>% filter(.,yearID <=2013) #Cuts off data for hindcasting

#parameters

player_select <-  "Edwin Encarnacion" #"Robinson Cano" #"Aroldis Chapman"  #"Mike Trout"#"Matt Kemp"#"Billy Hamilton" 
comp_cutoff <- 1950   #Cuts off historical data
btt_AB_cutoff <- 130
ptch_Inn_cutoff <- 50
#zvalue= 2.58#1.645          # 90% confidence level
trim=.90              # % trim used for trimmed mean and sd. 
ptch_constant=3       #higher values relax similarity criteria
btt_constant=30     #higher values relax similarity criteria
confidence_level= .99
position_match=T
WAR_Value = 8 #dollars per war in millions

#get player
selected<-olap %>% filter(.,nameFirstLast==player_select) %>% filter(.,age >= round((max(age)-2),1))

  #filter(.,year(now())-year(finalGame)<=3) %>% 

#calculate 3 year weighted average of player

#assign recent season weights

    selected_rows <- nrow(selected)
    if (selected_rows==3) {
      age_weight=c(.10,.30,.60)
    }
    if (selected_rows==2) {
      age_weight=c(.40,.60)
    }
    if (selected_rows==1) {
      age_weight=c(1)
    }
    
    selected<-cbind(selected,as.data.frame(age_weight))


    grp<-c("playerID","retroID","bbrefID","nameFirst","nameLast","nameGiven","weight","height","bats","throws","debut","finalGame","birthDate",
    "nameFirstLast","age_eval","primary_pos")
    
    #create weighted average
    selected_eval<- selected %>%mutate(.,age_eval=max(age),age_weight=ifelse(primary_pos !="P", sqrt(btt_AB)*age_weight,sqrt(ptch_Inn)*age_weight))%>% select(.,-yearID,-PlayerYearID,-age,-BBRefYearID)%>%
      group_by_(.dots=grp) %>% 
      summarise_each(funs(weighted.mean(., age_weight,na.rm=T)),
                     -age_weight)%>% ungroup(.)


#subset player pool for comps

hist_player_select  <- olap %>% filter(.,yearID>=comp_cutoff, btt_AB >=btt_AB_cutoff |ptch_Inn>= ptch_Inn_cutoff)
#added in option to only select players with same position
if (position_match == T) {
  hist_player_select<- hist_player_select %>% filter(.,primary_pos==selected_eval$primary_pos)
  
}

hist_player_select <- hist_player_select %>% select(.,playerID,age) %>% filter(.,playerID!=unique(selected_eval$playerID) & age>=max(selected_eval$age_eval)-.5 & age <=max(selected_eval$age_eval)+.5) %>% group_by(.,playerID) %>% filter(.,age==max(age)) %>% ungroup(.) %>% rename(.,age_filter=age)
  
hist_player_select <- olap %>% filter(.,yearID>=comp_cutoff, btt_AB >=btt_AB_cutoff |ptch_Inn>= ptch_Inn_cutoff) %>% select(.,playerID,age) %>% inner_join(.,hist_player_select, by="playerID") %>% filter(.,age<=age_filter & age>=age_filter-2) %>% select(.,-age_filter)




hist_player_pool <- olap %>% semi_join(.,hist_player_select) %>%mutate(.,age_eval=selected_eval$age_eval)

#create historical player weights

hist_player_age_weight <- hist_player_pool %>% select_(.,.dots=paste("-",grp[2:length(grp)],sep=""))%>%
  select(.,playerID,age) %>%arrange(.,playerID,age) %>% group_by(.,playerID) %>% mutate(.,age_maxcount=n(),age_count=seq_along(playerID)) %>% ungroup(.)


hist_player_age_weight$age_weight= ifelse( hist_player_age_weight$age_maxcount==1,
          1,
        
        ifelse( hist_player_age_weight$age_maxcount==2,
                ifelse(hist_player_age_weight$age_count==1,.40,.60),
         ifelse(hist_player_age_weight$age_maxcount==3,
                ifelse(hist_player_age_weight$age_count==1,.10,ifelse(hist_player_age_weight$age_count==2,.30,.60)),NA)
        ))


# join to historical player pool on playerid and age then create weighted avg

hist_player_pool<- select(hist_player_age_weight,playerID,age,age_weight) %>% left_join(hist_player_pool,.,by=c("playerID","age"))  %>% mutate(.,age_weight=ifelse(primary_pos !="P", sqrt(btt_AB)*age_weight,sqrt(ptch_Inn)*age_weight))%>% select(.,-yearID,-PlayerYearID,-age,-BBRefYearID)%>%
  group_by_(.dots=grp) %>% 
  summarise_each(funs(weighted.mean(., age_weight,na.rm=T)),
                 -age_weight) %>%ungroup(.)



#compare position players to position players and pitchers to pitchers
if (selected_eval$primary_pos != "P") {
  hist_player_pool <- filter(hist_player_pool, hist_player_pool$primary_pos != "P")
} else {
  hist_player_pool <- filter(hist_player_pool, hist_player_pool$primary_pos == "P")
}

#get distinct positions
positions<-unique(olap$primary_pos)


colnames <-  names(hist_player_pool)

#subset vars and var_weights
btt_vars <- colnames[grep("^btt_",colnames)]
btt_weight<- btt_vars[grep("*_weight",btt_vars )] 
btt_vars  <- btt_vars[!btt_vars %in% btt_weight]

ptch_vars <- colnames[grep("^ptch_",colnames)]
ptch_weight<- ptch_vars[grep("*_weight",ptch_vars )] 
ptch_vars  <- ptch_vars[!ptch_vars %in% ptch_weight]



#run comps
if (selected_eval$primary_pos !="P") {
  
  #calculate positionplayers here  
for (r in 1:nrow(hist_player_pool)) {
  
  for (i in btt_vars) {
    i_sd <- sd(unlist(hist_player_pool[,i]),na.rm=T)

    hist_player_pool[r,paste(i,'_idx',sep="")]= (selected_eval[,paste(i,'_weight',sep="")]/10)*(((selected_eval[,i]/i_sd)-(hist_player_pool[r,i]/i_sd))**2) 
    
  } 
}
}

if (selected_eval$primary_pos =="P") {
  for (r in 1:nrow(hist_player_pool)) {
    
    for (i in ptch_vars) {

      i_sd <- sd(unlist(hist_player_pool[,i]),na.rm=T)
      hist_player_pool[r,paste(i,'_idx',sep="")]= (selected_eval[,paste(i,'_weight',sep="")]/10)*(((selected_eval[,i]/i_sd)-(hist_player_pool[r,i]/i_sd))**2)

    }
  }
}
  

for (r in 1:nrow(hist_player_pool)) {
  
#weights position weights
  hist_player_pool[r,"pos_idx"]= abs((selected_eval[,'pweight']-hist_player_pool[r,'pweight']))
}

#add in player index

idx_vars<- names(hist_player_pool)[grep("*_idx",names(hist_player_pool))]

hist_player_pool2 <- hist_player_pool %>% mutate(.,diviance=sqrt(rowSums(hist_player_pool[,idx_vars])))


#calc similarity score - [11/8/2016 DH] we are using different constance for 

similarity_constant <-ifelse(selected_eval$primary_pos =="P",ptch_constant,btt_constant) 
similarity_constant

hist_player_pool3<- hist_player_pool2%>% mutate(.,Similarity_Score=100*(( similarity_constant-diviance)/similarity_constant )) %>% arrange(.,desc(Similarity_Score))


#get eval age for each historical player
hist_player_age <- hist_player_select %>% group_by(.,playerID) %>% mutate(.,hist_eval_age = max(age)) %>% select(.,-age)%>% ungroup(.) %>% distinct(.) 

#select only players with positive comps

hist_player_pool4 <-hist_player_pool3%>%filter(.,Similarity_Score>0) %>% select(.,playerID,Similarity_Score) %>% left_join(.,hist_player_age,by="playerID") #%>% filter(.,Similarity_Score >=40)

#get career arc for these players [eval season -2 , eval season + 7]

hist_player_career <- olap %>% filter(.,playerID %in% hist_player_pool4$playerID) %>% left_join(.,hist_player_pool4) %>% filter(.,age >=hist_eval_age-2 & age<=hist_eval_age+7)%>%mutate(.,d_eval_age=ifelse(age==hist_eval_age,1,0))%>% select(., playerID,retroID,bbrefID,nameFirstLast,age,hist_eval_age,d_eval_age,yearID,WAR,mWAR,Similarity_Score) %>% mutate(.,yearSeason=round(age-hist_eval_age),mWAR_diff=WAR-mWAR)%>%arrange(.,desc(Similarity_Score))

#create war diff from year to year
hist_player_career <- hist_player_career %>% group_by(.,playerID,retroID,bbrefID,nameFirstLast) %>%mutate(.,WAR_pct_chg=(WAR-lag(WAR,1,order_by=yearSeason))/lag(WAR,1,order_by=yearSeason)) %>% ungroup(.)


#output datasets for demo friday:
# write.csv(selected_eval,paste("./output/demo_friday_11-11-16/selected_eval_",as.Date(now(), format = "%Y-%m-%d"),".csv",sep = ""),na="",row.names = F)
# write.csv(hist_player_career,paste("./output/demo_friday_11-11-16/hist_player_career_",as.Date(now(), format = "%Y-%m-%d"),".csv",sep = ""),na="",row.names = F)
# write.csv(hist_player_pool3,paste("./output/demo_friday_11-11-16/hist_player_pool3_",as.Date(now(), format = "%Y-%m-%d"),".csv",sep = ""),na="",row.names = F)

#############################################################################
####################### PROJECT WAR FORWARD HERE ############################
#############################################################################

#create war weighted average with Similarity_Score as weight by yearSeason


war_prj<- select(hist_player_career,yearSeason,Similarity_Score)%>% group_by(.,yearSeason) %>%summarise(.,sim_sum=sum(Similarity_Score)) %>% left_join(select(hist_player_career,yearSeason,WAR_pct_chg,Similarity_Score),.) %>%mutate(.,sim_weight=Similarity_Score/sim_sum)%>% group_by(.,yearSeason) %>%select(.,-Similarity_Score,-sim_sum ) %>% mutate(.,n=n(),s=sd(WAR_pct_chg))%>% mutate(.,trim.lo=floor(n*trim)+1)%>%mutate(.,trim.hi=n+1-trim.lo) %>% arrange(.,yearSeason,WAR_pct_chg)%>% mutate(.,groupcount=index(n))%>% filter(.,groupcount>=trim.hi & groupcount<=trim.lo)

#%>%summarise_each(funs(weighted.mean(.,sim_weight)),-sim_weight)%>% mutate(.,error=(qgamma(.95,rate=WAR,shape=5)*s/sqrt(n)),left=WAR-error,right=WAR+error)

#trim hist player career to remove outliers
# # trim=.90
# lo <- floor(n * trim) + 1
# hi <- n + 1 - lo
# x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]




#######  WEIGHTED T DIST

#use weighted T distribution and get the mean, tvalue, and std.err for each yearseason 1- max(yearSeason:)

# install.packages("weights")
# library(weights)

# t.test.dta <- war_prj 
# test <- t.test.dta$WAR_pct_chg[which(t.test.dta$yearSeason==5)]
# weight <- t.test.dta$sim_weight[which(t.test.dta$yearSeason==5)]
# wtt <- wtd.t.test(test,weight = weight)
# wtt$additional[[4]]


war_prj$pct_chg_mean=NA
war_prj$lower=NA
war_prj$upper=NA


for (i in 1:max(war_prj$yearSeason)) {
  
  wttest <-wtd.t.test(war_prj$WAR_pct_chg[which(war_prj$yearSeason==i)],weight=war_prj$sim_weight[which(war_prj$yearSeason==i)])
  
  prob= 1-((1-confidence_level)/2)
  
  pct_chg_mean=wttest$additional[[2]]
  i_se=wttest$additional[[4]]
  i_df=wttest$coefficients[[2]]
  i_error= qt(prob,df=i_df)*i_se
  lower=pct_chg_mean-i_error
  upper=pct_chg_mean+i_error
    
  war_prj$pct_chg_mean=ifelse(war_prj$yearSeason==i,pct_chg_mean,war_prj$pct_chg_mean)
  war_prj$lower=ifelse(war_prj$yearSeason==i,lower,war_prj$lower)
  war_prj$upper=ifelse(war_prj$yearSeason==i,upper,war_prj$upper)
  
}

#wttest <-wtd.t.test(war_prj$WAR_pct_chg[which(war_prj$yearSeason==5)],weight=war_prj$sim_weight[which(war_prj$yearSeason==5)])


war_prj <- war_prj %>% group_by(.,yearSeason) %>% summarise_each(funs(mean(.,na.rm=T))) 


##############





# sqlcode <- "select yearSeason, sum(WAR_pct_chg*sim_weight)as wt_WAR_pct_chg
#             from war_prj
#             group by yearSeason"
# weighted_war<- sqldf(sqlcode)
# weighted_war$wt_WAR_pct_chg=as.numeric(weighted_war$wt_WAR_pct_chg)
# 
# 
# war_prj <- left_join(war_prj,weighted_war) %>% group_by(.,yearSeason)%>%mutate(.,wt.variance=sum(sim_weight*(WAR_pct_chg-wt_WAR_pct_chg)^2)) %>% mutate(.,wt.sd=sqrt(wt.variance)) %>% mutate(.,wt.upper=wt_WAR_pct_chg+zvalue*(wt.sd/sqrt(n)),wt.lower=wt_WAR_pct_chg-zvalue*(wt.sd/sqrt(n))) %>% select(.,-n,-s,-WAR_pct_chg,-sim_weight)%>%summarise_each(funs(mean(.,na.rm=T))) %>% rename(.,WAR_pct_chg=wt_WAR_pct_chg)



# war_prj <- left_join(war_prj,weighted_war) %>% group_by(.,yearSeason)%>%mutate(.,wt.variance=sum(sim_weight*(WAR-wWAR)^2)) %>% mutate(.,wt.sd=sqrt(wt.variance)) %>% mutate(.,wt.upper=wWAR+zvalue*(wt.sd/sqrt(n)),wt.lower=wWAR-zvalue*(wt.sd/sqrt(n))) %>% select(.,-n,-s,-WAR,-sim_weight)%>%summarise_each(funs(mean(.,na.rm=T))) %>% rename(.,WAR=wWAR)



# Using Marcels:
 # war_prj<- select(hist_player_career,yearSeason,Similarity_Score) %>% group_by(.,yearSeason)%>%summarise(.,sim_sum=sum(Similarity_Score)) %>% left_join(select(hist_player_career,yearSeason,WAR,mWAR_diff,Similarity_Score),.)%>%mutate(.,sim_weight=Similarity_Score/sim_sum)%>% group_by(.,yearSeason) %>%select(.,-Similarity_Score,-sim_sum ) %>% summarise_each(funs(weighted.mean(.,sim_weight,na.rm=T)),-sim_weight) %>% ungroup(.)%>%mutate(.,warnew=WAR+mWAR_diff)


# war_player<-select(selected,playerID,yearID,age,WAR)%>%mutate(.,age_eval=selected_eval$age_eval,yearSeason=age-age_eval)%>%full_join(.,war_prj,by="yearSeason")%>%mutate(.,WAR=coalesce(WAR.x,WAR.y),d_forecast=ifelse(yearSeason>0,1,0)) %>% select(.,-WAR.x,-WAR.y) %>% mutate(.,playerID=na.locf(playerID),age=na.locf(age),age_eval=na.locf(age_eval),yearID=na.locf(yearID),age=ifelse(yearSeason>0,age+yearSeason,age),yearID=ifelse(yearSeason>0,yearID+yearSeason,yearID))%>%mutate(.,left=ifelse(d_forecast==1,left,NA),right=ifelse(d_forecast==1,right,NA))


war_player<-select(selected,playerID,yearID,age,WAR)%>%mutate(.,age_eval=selected_eval$age_eval,yearSeason=round(age-age_eval,0))%>%full_join(.,war_prj,by="yearSeason")%>%mutate(.,d_forecast=ifelse(yearSeason>0,1,0)) %>% mutate(.,playerID=na.locf(playerID),age=na.locf(age),age_eval=na.locf(age_eval),yearID=na.locf(yearID),age=ifelse(yearSeason>0,age+yearSeason,age),yearID=ifelse(yearSeason>0,yearID+yearSeason,yearID)) %>% arrange(.,yearSeason)



# for (i in 2:nrow(war_player)) {
#   lastvar= ncol(war_player)
#   i_lag=i-1
#   if (is.na(war_player[i_lag,4])) {
#     war_player[i,lastvar]= (war_player[i,lastvar]+(war_player[i,lastvar]*war_player[i,7]))
#   }
#   else  {  war_player[i,lastvar]=(war_player[i_lag,4]+(war_player[i_lag,4]*war_player[i,7]))
#   }
# }

war_player$WAR2=NA

for (i in 2:nrow(war_player)) {
lag_i = i-1
  if (is.na(war_player$WAR[lag_i])) {
    war_player$WAR2[i]= war_player$WAR2[lag_i]*(1+war_player$pct_chg_mean[i])
    war_player$upper[i]= war_player$WAR2[lag_i]*(1+war_player$upper[i])
    war_player$lower[i]= war_player$WAR2[lag_i]*(1+war_player$lower[i])
}
    else {

      #tried using the 3 year averaged war as the starting point for the player forcast
      war_player$WAR2[i]= selected_eval$WAR*(1+war_player$pct_chg_mean[i])
      war_player$upper[i]= selected_eval$WAR*(1+war_player$upper[i])
      war_player$lower[i]= selected_eval$WAR*(1+war_player$lower[i])
      
        }
}



war_player<-select(war_player,-WAR_pct_chg, -trim.lo, -trim.hi,-groupcount, -sim_weight)%>% mutate(.,WAR=coalesce(WAR,WAR2),upper=ifelse(d_forecast==1,upper,NA),lower=ifelse(d_forecast==1,lower,NA)) %>% mutate(.,projected_value=WAR*WAR_Value)


 
#join actuals
actuals <- filter(olap_actuals,playerID==selected_eval$playerID,yearID>=min(war_player$yearSeason))%>%select(.,playerID,yearID,WAR)%>% group_by(.,playerID,yearID)%>%summarise(.,WAR=sum(WAR,na.rm=T)) %>% rename(.,WAR_Actual=WAR)

war_player <- left_join(war_player,actuals)

# ##############################################################################################
# with(hist_player_career, tapply(WAR, yearSeason, shapiro.test))
# 
# #MONTE-CARLO SIMULATIONS
# war_prj_mc<- select(hist_player_career,yearSeason,Similarity_Score) %>% group_by(.,yearSeason)%>%summarise(.,sim_sum=sum(Similarity_Score)) %>% left_join(select(hist_player_career,yearSeason,WAR,Similarity_Score),.)%>%mutate(.,sim_weight=Similarity_Score/sim_sum)%>% group_by(.,yearSeason) %>%select(.,-Similarity_Score,-sim_sum ) %>% filter(.,yearSeason>0) %>% ungroup(.)
# 
# 
# # war_prj_mc <- select(hist_player_career,yearSeason,WAR,Similarity_Score) %>% mutate(.,sim_sum=sum(Similarity_Score),sim_weight=Similarity_Score/sim_sum)%>% group_by(.,yearSeason) %>%select(.,-Similarity_Score,-sim_sum ) %>% filter(.,yearSeason>0)
# 
# for (i in 1:7) {
# s <- as.data.frame(print(summary.mc(mc(mcstoc(rempiricalD,values=c(war_prj_mc[war_prj_mc$yearSeason==1,]$WAR), prob=c(war_prj_mc[war_prj_mc$yearSeason==1,]$sim_weight))))))
# assign(paste("s", i, sep = '_'), s)
# }
# 
# mcdata(war_prj_mc$WAR, type="0", nsv=ndvar(), nsu=ndunc(),
#        nvariates=1, outm="each")
# 
# 
# forecast_seasons <- bind_rows(s_1,s_2,s_3,s_4,s_5,s_6,s_7) %>% select(.,mean,X2.5.,X97.5.)%>%mutate(.,yearSeason=as.numeric(rownames(.)))
# 
# war_player_mc<-select(selected,playerID,yearID,age,WAR)%>%mutate(.,age_eval=selected_eval$age_eval,yearSeason=age-age_eval)%>%full_join(.,forecast_seasons,by="yearSeason")%>%mutate(.,WAR=coalesce(WAR,mean),d_forecast=ifelse(yearSeason>0,1,0)) %>% select(.,-mean) %>% mutate(.,playerID=na.locf(playerID),age=na.locf(age),age_eval=na.locf(age_eval),yearID=na.locf(yearID),age=ifelse(yearSeason>0,age+yearSeason,age),yearID=ifelse(yearSeason>0,yearID+yearSeason,yearID))#%>%mutate(.,left=ifelse(d_forecast==1,left,NA),right=ifelse(d_forecast==1,right,NA))


#visualize w/ actuals
avg_mape=1.429107

war_player <- war_player %>% mutate(.,avg_mape=avg_mape)%>% mutate(.,upper=ifelse(d_forecast==1,WAR*(1+avg_mape),NA), lower=ifelse(d_forecast==1,WAR*(1-avg_mape),NA))


eval_yr = as.numeric(filter(war_player,yearSeason==0)%>%select(yearID))
limits<-aes(ymax=war_player$upper,ymin=war_player$lower) # expand error bar here w/ multi



ggplot(war_player,aes(x=yearID))+
  geom_line(aes(y=WAR),colour="blue")+ 
  geom_point(aes(y=WAR),colour="blue")+
  geom_text(aes(y=WAR,label = round(WAR,2)),vjust=0,nudge_y = .75,nudge_x = .25,colour="blue")+
  geom_line(aes(y=WAR_Actual),colour="red")+
  geom_point(aes(y=WAR_Actual),colour="red")+
  #geom_text(aes(y=WAR_Actual,label = round(WAR_Actual,2)),vjust=0,nudge_y = -2.25,nudge_x = .25)+
  scale_y_continuous(limits=c(-10,30),breaks = seq(-10,30,by = 3))+
  scale_x_continuous(breaks=seq(min(war_player$yearID),max(war_player$yearID),by=1))+
  geom_vline(xintercept = eval_yr)+
  geom_hline(yintercept = 0)+
  geom_errorbar(limits,position=position_dodge(width=.25),colour="blue")+
  ggtitle(paste(player_select," -  Age:",war_player$age_eval," -  Wins Above Replacement Projection",sep=" "))+
  theme_gray()

