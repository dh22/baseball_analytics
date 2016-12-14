library(forecast)
library(sqldf)
library(zoo)
dataset1<-t4_btt
sql<- "select age, sum(war) as war
 from t1 
where BBRef_playerid = 'younger03'
group by age;"
dataset2<-sqldf(sql)

str(dataset1)
print('########')
str(dataset2)
dataset1$age<- as.numeric(dataset1$age)
dataset2$age<- as.numeric(dataset2$age)
print('########')
print('##1##')
labels <- as.numeric(dataset1$Pct_Chg_Avg_WAR)
print('##3##')
timeseries <- ts(labels,frequency=1)
print('##4##')
print(timeseries)
model <- auto.arima(timeseries)
print('##5##')
print(ceiling(max(dataset2$age))) 
print(ceiling(max(dataset1$age)))

numPeriodsToForecast <-  max(dataset2$age)-max(dataset1$age)
print('##6##')
numPeriodsToForecast <-abs(numPeriodsToForecast)
print(numPeriodsToForecast)
print('##7##')
print(model)
forecastedData <- forecast(model, h=numPeriodsToForecast)
print('##8##')
forecastedData <- as.numeric(forecastedData$mean)
print(forecastedData)
print('##9##')
#output <- data.frame(cbind(dataset2,forecastedData))
str(forecastedData)
str(dataset2)
output <- cbind(dataset2,forecastedData) #bind_cols(dataset2$age,forecastedData)   data.frame(age=dataset2$age,forecast=forecastedData)
# data.set <- zoo(output)
plot(model$x)
print('##10##')
# data.set$LagWAR=lag(data.set$war,n=1,order_by = data.set$age,na.pad = T)
# data.set$LagForecast=lag(data.set$forecastedData,n=1,order_by = data.set$age,na.pad = T)
# data.set$forecastWAR=as.numeric(lag(data.set$war,n=1,order_by = data.set$age,na.pad = T)*lag(data.set$forecastedData,n=1,order_by = data.set$age,na.pad = T))
# data.set
data.set <- output
# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("data.set");