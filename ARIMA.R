library(readxl)
library(ggplot2)
library(forecast)
library(fUnitRoots)

df<-read_excel("ARIMA.xlsx")
names(df)<-c("Year","GDP")
head(df)


df_ts<-ts(df$GDP,start=1979)
df_ts

plot(df_ts,ylab="GDP(billion)",xlab="Year")

adfTest(df_ts)

df_ts1<-diff(df_ts,diff=1)
adfTest(df_ts1)

df_ts2<-diff(df_ts,diff=2)
adfTest(df_ts2)

plot(df_ts2,ylab="GDP(billion)",xlab="Year",main="Two times difference")

acf(df_ts2)  
pacf(df_ts2)

fit<-auto.arima(df_ts,d=2,trace=1,ic = "aic")
fit

Box.test(fit$residuals,type="Ljung-Box")

temp<-df_ts
for (i in seq(1,31)){
  fit1<-arima(temp,c(0,2,0))  
  pred<-forecast(fit1,h=1,level = c(95))  
  pred.value<-pred$mean   
  temp<-ts(c(temp,pred.value),start=1979)  
}
temp

plot(temp,xlab="Year",ylab="GDP(billion)",main="ARIMA")


