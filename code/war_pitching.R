### Open source script for scraping data from BaseballReference.com and laoding in Sean Lahman's Baseball Database.
### This script is the work of Kris Eberwein and can be found on GitHub at https://github.com/keberwein/Data_Science_Riot/tree/master/Lahman_WAR

#DH --- MODIFIED TO READ FROM LAHMAN PACKAGE INSTEAD OF DATABASE - DOWN THE LINE WE MAY WANT TO LOAD ALL OF THIS DATA INTO A DB AND UPDATE


#
#Load packages and install if you don't have them

source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
get_package(c("dplyr","Lahman","lubridate","Sabermetrics","DBI","RMySQL"))

#require(DBI)
#require(RMySQL)
#require(dplyr)

#Get the data from Baseball Reference
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "http://www.baseball-reference.com/data/war_daily_pitch.txt"
download.file(fileUrl, destfile="./raw/war_daily_pitch.csv", method="curl")

#Write the download to a data frame
df <- read.csv("./raw/war_daily_pitch.csv", header=TRUE)

#Connect to your Lahman instance so we can grab some data from the master table
    # drv <- dbDriver("PostgreSQL")
    # con <- dbConnect(drv, host= "localhost", dbname = "Lahman", 
    #                  user="YOUR USERNAME", password="YOUR PASSWORD")

#SQL the database for playerid on the master table "bbrefid" are the Baseball Reference ids
# master <- dbSendQuery(con, "SELECT playerID, bbrefID FROM master")
str(Master)
m <- Master %>% select(., playerID, bbrefID,nameFirst,nameLast )
# m <- fetch(master, n = -1)

#SQL on the teams table to match Baseball Ref team_ID
# teams <- dbSendQuery(con, "SELECT yearID, teamID, teamIDBR FROM teams")

t<-Teams%>%select(.,yearID, teamID, teamIDBR)
# t <- fetch(teams, n = -1)

#Join master and war data frames
df2 <- left_join(df, m, by = c("player_ID" = "bbrefID"))

# Convert and rename a few things in the teams dataframe to make the join smooth
t$teamIDBR <- as.factor(t$teamIDBR)
names(t)[names(t)=="teamIDBR"] <- "team_ID"
names(t)[names(t)=="yearID"] <- "year_ID"

#Now we index the teams
df3 <- left_join(df2, t)

#Reorder data frame
final <- subset(df3, select = c(playerID,player_ID,name_common,nameFirst,nameLast, year_ID, age, teamID,  stint_ID, lg_ID, G, GS, IPouts, IPouts_start, 
                                IPouts_relief, RA, xRA, xRA_sprp_adj, xRA_def_pitcher, PPF, PPF_custom, xRA_final, 
                                BIP, BIP_perc, RS_def_total, runs_above_avg, runs_above_avg_adj, runs_above_rep, 
                                RpO_replacement, GR_leverage_index_avg, WAR, salary, teamRpG, oppRpG, pyth_exponent, 
                                waa_win_perc, WAA, WAA_adj, oppRpG_rep, pyth_exponent_rep, waa_win_perc_rep, WAR_rep))

#Rename a couple of columns in our new tidy data set to fit to Lahman standards
names(final)[names(final)=="stint_ID"] <- "stint"
names(final)[names(final)=="lg_ID"] <- "lgID"
names(final)[names(final)=="year_ID"] <- "yearID"
names(final)[names(final)=="team_ID"] <- "teamID"

# Clean up the data types before loading into Lahman
# This part is nausiating but necessary...
final$age <- as.integer(as.character(final$age))
final$stint <- as.integer(as.character(final$stint))
final$G <- as.integer(as.character(final$G))
final$GS <- as.integer(as.character(final$GS))
final$IPouts <- as.integer(as.character(final$IPouts))
final$salary <- as.integer(as.character(final$salary))
final$IPouts_start <- as.integer(as.character(final$IPouts_start))
final$IPouts_relief <- as.integer(as.character(final$IPouts_relief))
final$RA <- as.integer(as.character(final$RA))
final$xRA_sprp_adj <- as.double(as.character(final$xRA_sprp_adj))
final$xRA_def_pitcher <- as.double(as.character(final$xRA_def_pitcher))
final$PPF <- as.integer(as.character(final$PPF))
final$PPF_custom <- as.double(as.character(final$PPF_custom))
final$xRA_final <- as.double(as.character(final$xRA_final))
final$BIP <- as.integer(as.character(final$BIP))
final$BIP_perc <- as.integer(as.character(final$BIP_perc))
final$RS_def_total <- as.double(as.character(final$RS_def_total))
final$runs_above_avg <- as.double(as.character(final$runs_above_avg))
final$runs_above_avg_adj <- as.double(as.character(final$runs_above_avg_adj))
final$runs_above_rep <- as.double(as.character(final$runs_above_rep))
final$RpO_replacement <- as.double(as.character(final$RpO_replacement))
final$GR_leverage_index_avg <- as.double(as.character(final$GR_leverage_index_avg))
final$WAR <- as.double(as.character(final$WAR))
final$oppRpG <- as.double(as.character(final$oppRpG))
final$pyth_exponent <- as.double(as.character(final$pyth_exponent))
final$waa_win_perc <- as.double(as.character(final$waa_win_perc))
final$WAA <- as.double(as.character(final$WAA))
final$WAA_adj <- as.double(as.character(final$WAA_adj))
final$oppRpG_rep <- as.double(as.character(final$oppRpG_rep))
final$pyth_exponent_rep <- as.double(as.character(final$pyth_exponent_rep))
final$waa_win_perc_rep <- as.double(as.character(final$waa_win_perc_rep))
final$WAR_rep <- as.double(as.character(final$WAR_rep))

# RenameID tags and Names
final<-rename(final,BBRef_playerID=player_ID,BBRef_Name=name_common)


# At this point you can do a write.csv() and load that into your Lahman instance
# OR
# Use the database connection that you established earlier to wirte a new table directly to Lahman
#Write your data frame back to the dbase. I like to write it as a test table first.


war_pitching<-final

save(war_pitching,file="./trn/war_pitching.RData")

# dbWriteTable(con, name='war_pitching', value=final, row.names = FALSE, overwrite = TRUE)


##Now go to the Baseball Reference WAR tables and admire your work!