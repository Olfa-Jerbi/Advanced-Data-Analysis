library(readxl)
elec <- read_excel("D:/ecl/info_avan/elec.xlsx")
View(elec)

summary(elec)
library(zoo)
library(ggplot2)
library(fpp2)
library(forecast)
library(plyr)
library(ggfortify)
library(tseries)


library(fpp2)
library(ggplot2)
library(reshape)
library(forecast)

summary(elec)
#extraction de la var énergie et transformer en ts
Time <- seq(as.Date("2010-01-01"), as.Date("2017-12-31"), by = "day")
x_tr_1 <- ts(elec[,8],     # random data
             start = c(2010, as.numeric(format(Time[1], "%j"))),
             frequency = 365.25)
#visualiser la série
autoplot(x_tr_1)

#décopser la série
f<-decompose(x_tr_1)
autoplot(f)

#prédiction stlf
x_tr_1 %>%
  stlf(lambda = 0, h = 365) %>%
  autoplot()

#prédiction tslm
fit.x <- tslm(x_tr_1 ~ trend + season)
fit.x
fcast <- forecast(fit.x,h=365)
autoplot(fcast) +
  ggtitle("Consommation de l'énergie en Tunisie") +
  xlab("Year") + ylab("Energie")

#prédiction en fct des jrs et Tmoy
fit.consBest <- tslm(
  x_tr_1 ~ JF+ Ramadhan+ Samedi+ Dimanche + Tmoy  ,
  data = elec)
h <- 365
newdata <-data.frame(
  JF= rep(1,h),
  Ramadhan=rep(1,h),
  Samedi=rep(1,h),
  Dimanche=rep(1,h),
  Tmoy= elec[1:365,7]
)
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <-data.frame(
  JF= rep(0,h),
  Ramadhan=rep(0,h),
  Samedi=rep(0,h),
  Dimanche=rep(0,h),
  Tmoy= elec[1:365,7]
)
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(x_tr_1) +
  ylab("énergie cosommée") +
  autolayer(fcast.up, PI = TRUE, series = "cas minimal") +
  autolayer(fcast.down, PI = TRUE, series = "cas maximal")+
  guides(colour = guide_legend(title = "Scenario"))

#prédiction en fct de Tmax et tmin
fit.cons <-  tslm(
  x_tr_1 ~ tmin,
  data = elec)
newdata <- data.frame(
  tmin= elec[1:365,5])
fcast.minT <- forecast(fit.cons, newdata =newdata)
#z=cbind.data.frame(elec[,8])
#z
#u=mean(as.numeric(z[,1]))
#u
fit.cons2 <- tslm(
  x_tr_1 ~  Tmax  ,
  data = elec)
#u1=max(as.numeric(z[,1]))
newdata <- data.frame(
  Tmax= elec[1:365,6]
)

fcast.maxt <- forecast(fit.cons2, newdata =newdata)
autoplot(x_tr_1) +
  ylab("énergie cosommée") +
  autolayer(fcast.minT, series = "en Température minimale",
            PI = TRUE) +
  autolayer(fcast.maxt, series = "en Température maximale",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))


dat= ts(data=elec, start=1, frequency=365 )
dat <- na.locf(dat)
dat%>%autoplot()+theme_bw()+xlab("Years")+ ylab("energy")
decompose_elec <- decompose(dat,"additive")
decompose_elec$figure
autoplot(decompose_elec$trend)
autoplot(decompose_elec$random)
autoplot(decompose_elec)

dat1= ts(data=elec[,7:8], start=1, frequency=365 )
dat1
dat1 <- na.locf(dat1)
dat1%>%autoplot()+theme_bw()+xlab("Years")+ ylab("energy")
decompose_elec1 <- decompose(dat1,"additive")
autoplot(decompose_elec1)
decompose_elec1$figure
autoplot(decompose_elec1$trend)
autoplot(decompose_elec1$random)
autoplot(decompose_elec1$x)

decompose_elec2 <- decompose(dat1,"multiplicative")
autoplot(decompose_elec2)

dat2= ts(data=elec[,8], start=1, frequency=365 )
decompose_elec2_ <- decompose(dat2,"multiplicative")
adf.test(dat2)
autoplot(decompose_elec2_$trend)
autoplot(decompose_elec2_$random)
autoplot(decompose_elec2_$x)
decompose_elec2_$seasonal

arima1 <- auto.arima(dat2)
arima1
#arima2<- ARIMA(dat (2,0,5)(0,1,0) 
model_115_ml = arima(dat2,order=c(2,0,5),seasonal = list(order=c(0,1,0)),method='ML')
coeftest(model_115_ml)
forecast3 <- forecast(model_115_ml, level = c(95), h = 200)
autoplot(forecast3)
#CALCULATE FORECASTS
forecast1 <- forecast(arima1, level = c(95), h = 36)
autoplot(forecast3)

forecast2 <- forecast(arima1, level = c(95), h = 200)
autoplot(forecast2)


fit <- Arima(dat2,order = c(2,1,3),seasonal = list(order=c(2,0,2),period=350),lambda = NULL)
fit
f_fit<-forecast(fit)
f_fit
dat2$Date <- as.Date(dat2$Date) 
autoplot(dat2, series="Data") +  
     autolayer(fit$fitted, series="SARIMA(2,1,3)(2,0,2)[12]") +
     autolayer(f_fit, series="Prediction") +
    xlab("annee") + ylab("Energie_trans") + ggtitle("result") + theme_bw()+theme(legend.title = element_blank(),legend.position = "bottom")
autoplot(f_fit)

var.m <- vars::VAR(dat, lag.max = 6, ic= "AIC", type = "none")
summary(var.m)
library(tidyverse)
library(tidymodels)
library(data.table)
library(tidyposterior)
library(tsibble) 
library(forecast) 
library(tseries)
library(TTR) 
library(vars) 
library(fable)
library(ggfortify)
library(MTS)
apply(dat,2,adf.test)

dataas <- diffM(dat)
apply(dataas,2,adf.test)
plot.ts(dataas)
autoplot(ts(dataas,start=2010, end=2017, frequency=12)) +ggtitle("Transformed time series")
VARselect(dataas,type = "const",lag.max = 7)
var.m <- vars::VAR(dataas, lag.max = 3, ic= "AIC", type = "none")
summary(var.m)
var.m$type


library(ggfortify)
#Do prediction for all time series
fc1=predict(var.m,n.ahead = 365,ci=0.95)
fc1
#Extract model for traffic volume from VAR
Tra<- fc1$fcst[8]
Tra<-Tra$Energie_trans[,1]

#Plot the predicted year 
print( paste ("Here we see the predicted values of Traffic volume for the chosen two years range " ))
autoplot(ts(Tra,start=2017, end=2018, frequency=12)) +ggtitle("Predicted part of time series for Traffic Volume (Years 2019 & 2020)")
tail(dat)
tra22<-cumsum(tra2)+  66477.87

#Create and trace the time series of traffic volume (real+predicted)
traff1<- ts(c(dataas[,1],tra22),start=2010, end=2018, frequency=12)

plot(window(traff1, start=start(traff1), end=2017), col="blue", xlim=range(time(traff1)), ylim=range(traff1))
par(new=TRUE)
plot(window(traff1, start=2017), col="red", axes=F, xlab="", ylab="", xlim=range(time(traff1)), ylim=range(traff),main="Traffic volume between 2012 and 2020 (Existing +Predicted years)") 

