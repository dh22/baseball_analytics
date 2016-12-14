source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("dplyr"))

#2005 to 2015 parkfactor data

#read data

parkfactors_2005<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2005.csv",header = T)
parkfactors_2006<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2006.csv",header = T)
parkfactors_2007<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2007.csv",header = T)
parkfactors_2008<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2008.csv",header = T)
parkfactors_2009<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2009.csv",header = T)
parkfactors_2010<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2010.csv",header = T)
parkfactors_2011<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2011.csv",header = T)
parkfactors_2012<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2012.csv",header = T)
parkfactors_2013<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2013.csv",header = T)
parkfactors_2014<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2014.csv",header = T)
parkfactors_2015<-read.csv("./raw/parkfactors/fangraphs_parkfactors_2015.csv",header = T)

#append data
parkfactors <- bind_rows(parkfactors_2015,parkfactors_2014,parkfactors_2013,parkfactors_2012,parkfactors_2011,parkfactors_2010,parkfactors_2009,parkfactors_2008,parkfactors_2007,parkfactors_2006,parkfactors_2005)


#rename variables
parkfactors<- rename(parkfactors,Season=ï..Season)

colnames(parkfactors)<- paste("pfct_",colnames(parkfactors),sep = "")
parkfactors<- rename(parkfactors,Season=pfct_Season,Team=pfct_Team)

#save data
save(parkfactors,file="./trn/parkfactors.RData")

