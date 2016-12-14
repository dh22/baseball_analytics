source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("dplyr"))

#read datasets 
al_league<-read.csv("./raw/fangraphs_al_leagueavg.csv",header = T)
nl_league<-read.csv("./raw/fangraphs_nl_leagueavg.csv",header = T)

#rename columns
al_league<- rename(al_league,Season=ï..Season)
nl_league<- rename(nl_league,Season=ï..Season)


#append
al_league$League="AL"
nl_league$League="NL"
alnl_league<- bind_rows(al_league,nl_league)%>%arrange(.,desc(Season))


#rename columns
colnames(alnl_league)<- paste("alnl_",colnames(alnl_league),sep = "")
alnl_league<- rename(alnl_league,Season=alnl_Season,League=alnl_League)

#save data
save(alnl_league,file="./trn/alnl_league.RData")
