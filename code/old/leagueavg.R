source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("dplyr"))

#read datasets 
leagueavg<-read.csv("./raw/fangraphs_leagueavg.csv",header = T)


#rename columns
leagueavg<- rename(leagueavg,Season=ï..Season)

colnames(leagueavg)<- paste("lg_",colnames(leagueavg),sep = "")
leagueavg<- rename(leagueavg,Season=lg_Season)

#save data
save(leagueavg,file="./trn/leagueavg.RData")
