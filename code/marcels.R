#Marcels Forecast#

source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/get_package.R")
source("C:/Users/dhollier/Box Sync/- DHOLLIER Private/R_Resources/myfunctions/multiplot.R")
get_package(c("dplyr","tidyr","Lahman","lubridate","sqldf","zoo","ggplot2","forecast","Sabermetrics","mc2d"))

load("./trn/war_batting.RData")
load("./trn/war_pitching.RData")
endyear = year(now()) #+ 7

reslist <- list()
for (i in 1957:endyear) {
  
pred.year = i


#building dataset 

B = subset(Batting, yearID >= pred.year - 3 & yearID < pred.year)
B = transform(B, PA = AB + BB + HBP + SF + SH)
B <- select(war_batting,playerID,yearID,WAR,WAR_off,WAR_def)%>% left_join(B,.)
# require(plyr)
stats = c("PA", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP","WAR","WAR_def","WAR_off")
B = ddply(B[, c("playerID", "yearID", stats)], ~playerID + yearID, summarise
          , PA = sum(PA), AB = sum(AB), R = sum(R)
          , H = sum(H), X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR)
          , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
          , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
          , SF = sum(SF), GIDP = sum(GIDP), WAR= sum(WAR), WAR_off=sum(WAR_off)
          , WAR_def=sum(WAR_def)
)


#beltran
x = subset(B, playerID == "beltrca01")$HR
x

n = subset(B, playerID == "beltrca01")$PA
n

#time 
B$t = with(B, ifelse(pred.year - yearID == 1, 5, ifelse(pred.year - yearID == 2, 4, 3)))

#beltran
t = subset(B, playerID == "beltrca01")$t
t


# COMPUTING LEAGUE AVERAGES

P = subset(Pitching, yearID >= pred.year - 3 & yearID < pred.year)
B.pos = subset(merge(x=B, y=P[, c("playerID", "yearID", "BFP")], by=c("playerID", "yearID"), all.x=TRUE), PA > BFP | is.na(BFP))

#  per plate appearance
p0 = ddply(B.pos, ~yearID, summarise, lgPA = sum(PA), lgAB = sum(AB)/sum(PA), lgR = sum(R)/sum(PA)
           , lgH = sum(H)/sum(PA), lgX2B = sum(X2B)/sum(PA), lgX3B = sum(X3B)/sum(PA), lgHR = sum(HR)/sum(PA)
           , lgRBI = sum(RBI)/sum(PA), lgSB = sum(SB)/sum(PA), lgCS = sum(CS)/sum(PA), lgBB = sum(BB)/sum(PA)
           , lgSO = sum(SO)/sum(PA), lgIBB = sum(IBB)/sum(PA), lgHBP = sum(HBP)/sum(PA), lgSH = sum(SH)/sum(PA)
           , lgSF = sum(SF)/sum(PA), lgGIDP = sum(GIDP)/sum(PA),lgWAR= sum(WAR)/sum(PA), lgWAR_off=sum(WAR_off)/sum(PA)
           , lgWAR_def=sum(WAR_def)/sum(PA)
)
p0[, c("yearID", "lgHR")]

# PERFORMING COMPUATION

M = merge(x=B, y=p0, by="yearID")

stats.lg = paste("lg", stats, sep="")

X = M[, stats]
P0 = M[, stats.lg]

t.X = M$t * X
t.n = M$t * M$PA
t.n0 = M$t * 100
t.P0 = M$t * M$PA * P0

mPA = with(M, ifelse(pred.year - yearID == 1, 0.5 * PA, ifelse(pred.year - yearID == 2, 0.1 * PA, 200)))

Q = cbind(M[, c("playerID", "yearID")], t.n, t.X, t.n0, t.P0, mPA)

res = ddply(Q, ~playerID, summarise, numSeasons = length(t.n), reliability = sum(t.n)/(sum(t.n) + sum(t.n0))
            , tn = sum(t.n)
            , PA = sum(PA), lgPA = sum(lgPA), mPA = sum(mPA)
            , AB = sum(AB), lgAB = sum(lgAB), R = sum(R), lgR = sum(lgR)
            , H = sum(H), lgH = sum(lgH), X2B = sum(X2B), lgX2B = sum(lgX2B)
            , X3B = sum(X3B), lgX3B = sum(lgX3B), HR = sum(HR),  lgHR = sum(lgHR)
            , RBI = sum(RBI), lgRBI = sum(lgRBI), SB = sum(SB),  lgSB = sum(lgSB)
            , CS = sum(CS), lgCS = sum(lgCS), BB = sum(BB),  lgBB = sum(lgBB)
            , SO = sum(SO), lgSO = sum(lgSO), IBB = sum(IBB),  lgIBB = sum(lgIBB)
            , HBP = sum(HBP), lgHBP = sum(lgHBP), SH = sum(SH),  lgSH = sum(lgSH)
            , SF = sum(SF), lgSF = sum(lgSF), GIDP = sum(GIDP),  lgGIDP = sum(lgGIDP)
            , WAR= sum(WAR),lgWAR= sum(lgWAR), WAR_off=sum(WAR_off),lgWAR_off=sum(lgWAR_off)
            , WAR_def=sum(WAR_def), lgWAR_def=sum(lgWAR_def))

stats.proj = setdiff(stats, "PA")
stats.m = paste("m", stats.proj, sep="")
stats.lg.proj = paste("lg", stats.proj, sep="")
res[, stats.m] = with(res, (reliability * res[, stats.proj]) / tn + (1 - reliability) * res[, stats.lg.proj] / tn)

res = merge(x=res, y=Master[,c("playerID", "birthYear")], by="playerID")
res = transform(res, age = pred.year - birthYear)

res$age.adj = with(res, ifelse(age > 29, 0.003 * (29-age), 0.006 * (29-age)))
res[, stats.m] = res[, stats.m] * (1 + res$age.adj)

res[, stats.m] = res[, stats.m] * res$mPA
#create prediction year
res$pred_year=pred.year
assign(paste("res", i, sep = '_'), res)
# reslist <- append(reslist,paste("res", i, sep = '_'))
}

#append datasets
marcels_results <- lapply(mget(ls(pattern = "^res_")), rbind.fill)
marcels_results <-  rbind.fill(marcels_results)
marcelsvars <- grep("^mWAR",colnames(marcels_results))

marcels_results <- select(marcels_results,playerID,pred_year,reliability,marcelsvars)
marcels_results$yearID = marcels_results$pred_year
marcels_results_btt <- select(marcels_results,-pred_year)

save(marcels_results_btt,file="./trn/marcels_results_btt.RData")



