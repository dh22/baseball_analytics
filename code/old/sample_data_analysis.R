source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("tidyjson","dplyr","XLConnect"))

#read-in aging map
load(file="./mapping/map_age.RData")

dta<-read.csv("./raw/sampledata/FanGraphs Leaderboard.csv",header = T)
dta_war<-read.csv("./raw/sampledata/FanGraphs Leaderboard_war.csv",header = T)

wb<- loadWorkbook("./mapping/aging.xlsx")




