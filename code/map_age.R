source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("tidyjson","dplyr","XLConnect"))

#read data
wb<- loadWorkbook("./mapping/aging.xlsx")

age = readWorksheet(wb,sheet = "Aging",header = T, region = 'A:P')
pos = readWorksheet(wb,sheet = "Aging",header = T, region = 'R:T')
#merge positions and age tables
all<-merge(age,pos)
#rearrange and rename variables
map_age <- all %>% select(Age,Lookup,Value,Speed,ERA.Multiplier,Delta.R.PA,NetHitting,NetPitching,PitchAdd,D.Velo,NetVelo,ERA.Multiplier.Old,NetPitching,PitchAdd.1,D.Velo.1,Col13,ERA.Multiplier.1,Col15,NetPitching.1) %>% arrange(.,Age) %>% rename(.,Position=Lookup)

#save map
save(map_age,file="./mapping/map_age.RData")

#load map
#load(file="./mapping/map_age.RData")