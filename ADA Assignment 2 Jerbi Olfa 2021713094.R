## ----setup, echo=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)




## ----pressure, echo=FALSE---------------------------------------------------------------------
#import data
data <- read.csv("D:\\Grad school\\2nd semester courses\\Advanced data analysis\\assignment 2\\Bias_correction_ucl.csv", header=TRUE, stringsAsFactors=FALSE)
#change variable type to date 
data$Date <- as.Date(data$Date, "%m/%d/%Y")
#view data structure
#str(data)
library(knitr)
kable(data[1:5,], caption="Data preview")


## ----chunk-x----------------------------------------------------------------------------------
summary(data)


## ----a, echo=FALSE----------------------------------------------------------------------------
d1<- data.frame(data$Date,data$Present_Tmax)
library(ggplot2)
library(scales) # to access breaks/formatting functions
ggplot(aes(x = data$Date, y = data$Present_Tmax), data = d1) + geom_line()


## ----b, echo=FALSE----------------------------------------------------------------------------

d2<- data.frame(data$Date,data$Present_Tmin)
ggplot(aes(x = data$Date, y = data$Present_Tmin), data = d2) + geom_line()

 


## ----c, echo=FALSE----------------------------------------------------------------------------

boxplot(data$Present_Tmax, main="Present Max Temperature")



## ----d, echo=FALSE----------------------------------------------------------------------------
sub1<-subset(d1, data$Date > as.Date("2017-05-30") )
ggplot(aes(x = sub1$data.Date, y = sub1$data.Present_Tmax), data = sub1) + geom_line()



## ----e, echo=FALSE----------------------------------------------------------------------------
boxplot(data$Present_Tmin, main="Present Min Temperature")
sub2<-subset(d2, data$Date > as.Date("2017-05-30") )
ggplot(aes(x = sub2$data.Date, y = sub2$data.Present_Tmin), data = sub2) + geom_line()



## ----f, echo=FALSE----------------------------------------------------------------------------
h1<- data.frame(data$Date,data$LDAPS_RHmax)
h2<- data.frame(data$Date,data$LDAPS_RHmin)
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hist(h1$data.LDAPS_RHmax, col = c2)
hist(h2$data.LDAPS_RHmin,col = c1 )




## ----g, echo=FALSE----------------------------------------------------------------------------

h11<-subset(h1, data$Date > as.Date("2017-05-30") )
ggplot(aes(x = h11$data.Date, y = h11$data.LDAPS_RHmax), data = h11) + geom_line()
#boxplot(h1$data.LDAPS_RHmax)
h22<-subset(h2, data$Date > as.Date("2017-05-30") )
ggplot(aes(x = h22$data.Date, y = h22$data.LDAPS_RHmin), data = h11) + geom_line()
#boxplot(h2$data.LDAPS_RHmin)
#calculate variance
v1<-var(h1$data.LDAPS_RHmax, y=NULL, na.rm=TRUE)
print( paste ("variance of Max humidity =" , v1) )
v2<-var(h2$data.LDAPS_RHmin, y=NULL, na.rm=TRUE)
print( paste ("variance of Min humidity =" , v2))



## ----h, echo=FALSE----------------------------------------------------------------------------

corr1 <- cor(data$Present_Tmax, data$LDAPS_Tmax_lapse, use = "complete.obs")
print( paste ("The correlation between present day Max temperature and next-day maximum air temperature applied lapse rate  =" , corr1))




## ----i, echo=FALSE----------------------------------------------------------------------------

corr2 <- cor(data$Present_Tmin, data$LDAPS_Tmin_lapse, use = "complete.obs")
print( paste ("The correlation between present day Min temperature and next-day minimum air temperature applied lapse rate  =" , corr2))




## ----j, echo=FALSE----------------------------------------------------------------------------

corr3 <- cor(data$Present_Tmin,data$LDAPS_WS, use = "complete.obs")
print( paste ("The correlation between present day Min temperature and wind speed  =" , corr3))
corr4 <- cor( data$Present_Tmax,data$LDAPS_WS, use = "complete.obs")
print( paste ("The correlation between present day Max temperature and wind speed  =" , corr4))





## ----k, echo=FALSE----------------------------------------------------------------------------

dc <- data.frame(data$LDAPS_CC1, data$LDAPS_CC2, data$LDAPS_CC3, data$LDAPS_CC4)
names(dc)[1] <- "Cloud coverage for 1st 6 hours"
names(dc)[2] <- "Cloud coverage for 2nd 6 hours"
names(dc)[3] <- "Cloud coverage for 3rd 6 hours"
names(dc)[4] <- "Cloud coverage for 4th 6 hours"
par(mar=c(7,5,1,1)) 
boxplot(dc,las=2)  




## ----l, echo=FALSE----------------------------------------------------------------------------

dp <- data.frame(data$LDAPS_PPT1, data$LDAPS_PPT2, data$LDAPS_PPT3, data$LDAPS_PPT4)
names(dp)[1] <- "Precipitation for 1st 6 hours"
names(dp)[2] <- "Precipitation for 2nd 6 hours"
names(dp)[3] <- "Precipitation for 3rd 6 hours"
names(dp)[4] <- "Precipitation for 4th 6 hours"
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
  vertical <- (par("usr")[3] + par("usr")[4]) / 2;
  text(horizontal, vertical, format(abs(cor(x,y,use = "complete.obs")), digits=2))
}
pairs(dp, main = "Edgar Anderson's for precpiation", pch = 21, bg = c("red","green3","blue","orange")[unclass(data$Next_Tmax)], upper.panel=panel.pearson)

  


## ----m, echo=FALSE----------------------------------------------------------------------------
ggplot(aes(x = Date, y = Solar.radiation), data = data) + geom_line()


## ----n, echo=FALSE----------------------------------------------------------------------------
ss <- data.frame(data$Date, data$Solar.radiation)
subsr <-subset(ss, data$Date > as.Date("2017-05-30") )
ggplot(aes(x= data.Date, y = data.Solar.radiation), data = subsr) + geom_line()


## ----o, echo=FALSE----------------------------------------------------------------------------

dsr <- data.frame(data$Solar.radiation ,data$lat, data$lon, data$DEM, data$Slope)
names(dsr)[1] <- "Solar radiation"
names(dsr)[2] <- "Latitude"
names(dsr)[3] <- "Longitude"
names(dsr)[4] <- " Elevation"
names(dsr)[5] <- " Slope"
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
  vertical <- (par("usr")[3] + par("usr")[4]) / 2;
  text(horizontal, vertical, format(abs(cor(x,y,use = "complete.obs")), digits=2))
}
pairs(dsr, main = "Edgar Anderson's for solar radiation and location ", pch = 21, bg = c("red","green3","blue","orange")[unclass(data$Next_Tmax)], upper.panel=panel.pearson)

  


## ----qq, echo=FALSE---------------------------------------------------------------------------
library(leaps)
library(dplyr)
regfit.full=regsubsets(Next_Tmax~.,data=data[,-25])
?regsubsets
summary(regfit.full)



## ----r, echo=FALSE----------------------------------------------------------------------------
library(leaps)
library(dplyr)

regfit.full=regsubsets(Next_Tmax~.,data=data[,-25], nvmax=15)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
par(mfrow=c(1,1))
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
coef(regfit.full,15)
which.max(reg.summary$adjr2)
print(paste("the best model is model number 15 with an adjusted R square=",reg.summary$adjr2[15]))
points(15,reg.summary$adjr2[15],pch=20,col="red")


## ----s, echo=FALSE----------------------------------------------------------------------------
regfit.fwd=regsubsets(Next_Tmax~.,data=data[,-25], nvmax=15,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale = "adjr2")
reg.summary=summary(regfit.fwd)
plot(reg.summary$rsq,xlab="Number of Variables",ylab="rsq")
which.max(reg.summary$rsq)
print(paste("the best model is model number 15 with an R square=",reg.summary$rsq[15]))
points(15,reg.summary$rsq[15],pch=20,col="red")


## ----t, echo=FALSE----------------------------------------------------------------------------
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
lm1<- get_model_formula(15,regfit.full, "Next_Tmax")
lm1
lm2<- get_model_formula(15,regfit.fwd, "Next_Tmax")
lm2
lm11 <- lm(data$Next_Tmax ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse +data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$lon + data$DEM + data$Slope)
lm22 <- lm(data$Next_Tmax ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse + data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC2 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$lon + data$Slope)
anova(lm11)
anova(lm22)


## ----u, echo=FALSE----------------------------------------------------------------------------

lm111 <- lm(data$Next_Tmax ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse +data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$lon  + data$Slope)
lm22 <- lm(data$Next_Tmax ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse + data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC2 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$lon + data$Slope)
anova(lm111)
anova(lm111,lm22, test="F")


## ----v, echo=FALSE----------------------------------------------------------------------------
print("Model 1")
summary(lm111)

print("Model 2")
summary(lm22)


## ----w, echo=FALSE----------------------------------------------------------------------------
# Split the data into training and test set
library(tidyverse)
library(caret)
set.seed(123)
datana <- na.omit(data)
datana <- datana[,-25]
training.samples <- datana$Next_Tmax %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- datana[training.samples, ]
test.data <- datana[-training.samples, ]
model <-  lm(train.data$Next_Tmax ~ train.data$station + train.data$Date + train.data$Present_Tmax + train.data$LDAPS_RHmin + train.data$LDAPS_Tmax_lapse + train.data$LDAPS_Tmin_lapse + train.data$LDAPS_WS + train.data$LDAPS_LH + train.data$LDAPS_CC1 + train.data$LDAPS_CC2 + train.data$LDAPS_CC3 + train.data$LDAPS_CC4 + train.data$LDAPS_PPT2 + train.data$lon +train.data$Slope, data = train.data)

preds <- predict(model, newdata =  test.data  , interval = 'prediction', na.action = na.pass)

# Make predictions and compute the R2, RMSE and MAE
predictions <-  predict(model, newdata= test.data)
#predictions
#R2 = R2(predictions, test.data$Next_Tmax)
#data.frame( R2 = R2(predictions, test.data$Next_Tmax),
            #RMSE = RMSE(predictions, test.data$Next_Tmax),
            #MAE = MAE(predictions, test.data$Next_Tmax))



## ----qq1, echo=FALSE--------------------------------------------------------------------------
library(leaps)
library(dplyr)
regfit.full2=regsubsets(data$Next_Tmax~.,data=data[,-24])
?regsubsets
summary(regfit.full2)



## ----r1, echo=FALSE---------------------------------------------------------------------------
library(leaps)
library(dplyr)

regfit.full2=regsubsets(data$Next_Tmax~.,data=data[,-c(24,25)], nvmax=18)
summary(regfit.full2)
reg.summary2=summary(regfit.full2)
par(mfrow=c(1,1))
which.max(reg.summary2$adjr2)
plot(reg.summary2$adjr2,xlab="Number of Variables",ylab="adjr2")
coef(regfit.full2,18)
print(paste("the best model is model number 18 with an adjusted R square=",reg.summary2$adjr2[18]))
points(18,reg.summary2$adjr2[18],pch=20,col="red")


## ----s1, echo=FALSE---------------------------------------------------------------------------
regfit.b=regsubsets(Next_Tmax~.,data=data[,-25], nvmax=18,method="backward")
summary(regfit.b)
plot(regfit.b,scale = "adjr2")
reg.summary22=summary(regfit.b)
plot(reg.summary22$rsq,xlab="Number of Variables",ylab="rsq")
which.max(reg.summary22$rsq)
print(paste("the best model is model number 18 with an R square=",reg.summary22$rsq[18]))
points(18,reg.summary22$rsq[18],pch=20,col="red")
coef(regfit.b,18)


## ----t1, echo=FALSE---------------------------------------------------------------------------
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
lm1m<- get_model_formula(18,regfit.full2, "Next_Tmin")
lm1m
lm2m<- get_model_formula(18,regfit.b, "Next_Tmin")
lm2m
lm11m <- lm(data$Next_Tmin ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse +data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC2 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$LDAPS_PPT3 + data$lon + data$DEM + data$Slope + data$lat)
lm22m <- lm(data$Next_Tmin ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse + data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC1 + data$LDAPS_CC2 + data$LDAPS_CC3 + data$LDAPS_CC4 + data$LDAPS_PPT2 + data$LDAPS_PPT3 + data$lon + data$Slope +data$lat + data$DEM)
anova(lm11m)
anova(lm22m)


## ----t111, echo=FALSE-------------------------------------------------------------------------

lm11mn <- lm(data$Next_Tmin ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse +data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH  + data$LDAPS_CC3 + data$LDAPS_PPT2 + data$LDAPS_PPT3 + data$lon + data$DEM + data$Slope + data$lat)
lm22mn <- lm(data$Next_Tmin ~ data$station + data$Date + data$Present_Tmax + data$LDAPS_RHmin + data$LDAPS_Tmax_lapse + data$LDAPS_Tmin_lapse + data$LDAPS_WS + data$LDAPS_LH + data$LDAPS_CC3  + data$LDAPS_PPT2 + data$LDAPS_PPT3 + data$lon  + data$DEM)
anova(lm11mn)
anova(lm22mn)
anova(lm11mn,lm22mn)


## ----v22, echo=FALSE--------------------------------------------------------------------------
print("Model 1")
summary(lm11mn)

print("Model 2")
summary(lm22mn)


## ----v8872, echo=FALSE------------------------------------------------------------------------
library(dplyr)
datatr <- data[1:6000,]
datate <- data [6000:7000,]
modelnewx <- lm(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + LDAPS_LH + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope, data= datatr )
prediii<- predict(modelnewx, newdata= datate)
MAEvaluex<- MAE(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("MAE=",MAEvaluex))
R2valuex<- R2(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("R2=",R2valuex))
RMSEvaluex<- RMSE(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("RMSE=",RMSEvaluex))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----vzk555ggk2, echo=FALSE-------------------------------------------------------------------
#Leave one out cross validation - LOOCV
# Define training control
train.control <- trainControl(method = "LOOCV")
datanew1 <- na.exclude(datatr)
# Train the model
modelnew1x <- train(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + LDAPS_LH + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew1x)


## ----vzkkhhh77, echo=FALSE--------------------------------------------------------------------
#K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
modelnew2x <- train(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + LDAPS_LH + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew2x)
#predict temperature on test dataset
pugh <- predict(modelnew2x, newdata =datate )



## ----vzkssskoo7, echo=FALSE-------------------------------------------------------------------
#Repeated K-fold cross-validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
modelnew3x <- train(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + LDAPS_LH + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew3x)


## ----vzgkgojjo7, echo=FALSE-------------------------------------------------------------------
#bootstrap
library(simpleboot)
set.seed(1)
bootresx <- lm.boot(modelnewx, R=10000, rows = TRUE, new.xpts = NULL, ngrid = 100,
        weights = NULL) 
summary(bootresx)


## ----v88hhip88p2, echo=FALSE------------------------------------------------------------------
modelnonlx <- lm(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + I(LDAPS_LH^2) + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope, data= datatr  )
predinonx<- predict(modelnonlx, newdata= datate)
MAEvalue<- MAE(datate$Next_Tmin, as.numeric(predinonx),na.rm=TRUE)
print(paste("MAE=",MAEvalue))
R2value<- R2(datate$Next_Tmin, as.numeric(predinonx),na.rm=TRUE)
print(paste("R2=",R2value))
RMSEvalue<- RMSE(datate$Next_Tmin, as.numeric(predinonx),na.rm=TRUE)
print(paste("RMSE=",RMSEvalue))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----v88hohddhop2, echo=FALSE-----------------------------------------------------------------
modelintx <- lm(Next_Tmax ~ station + Date + Present_Tmax + 
     LDAPS_RHmin + LDAPS_Tmax_lapse + LDAPS_Tmin_lapse + 
     LDAPS_WS + LDAPS_LH + LDAPS_CC1 + LDAPS_CC2 + 
     LDAPS_CC3 + LDAPS_CC4 + LDAPS_PPT2 + lon + 
     Slope + LDAPS_PPT2*LDAPS_PPT3 , data= datatr)
predintx<- predict(modelintx, newdata= datate)
MAEvalue<- MAE(datate$Next_Tmin, as.numeric(predintx),na.rm=TRUE)
print(paste("MAE=",MAEvalue))
R2value<- R2(datate$Next_Tmin, as.numeric(predintx),na.rm=TRUE)
print(paste("R2=",R2value))
RMSEvalue<- RMSE(datate$Next_Tmin, as.numeric(predintx),na.rm=TRUE)
print(paste("RMSE=",RMSEvalue))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----v88hhii2, echo=FALSE---------------------------------------------------------------------
library(dplyr)
datatr <- data[1:6000,]
datate <- data [6000:7000,]
modelnew <- lm(Next_Tmin~ station + Date + Present_Tmax + LDAPS_RHmin + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat, data= datatr )
prediii<- predict(modelnew, newdata= datate)
MAEvalue<- MAE(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("MAE=",MAEvalue))
R2value<- R2(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("R2=",R2value))
RMSEvalue<- RMSE(datate$Next_Tmin, as.numeric(prediii),na.rm=TRUE)
print(paste("RMSE=",RMSEvalue))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----vzkk2, echo=FALSE------------------------------------------------------------------------
#Leave one out cross validation - LOOCV
# Define training control
train.control <- trainControl(method = "LOOCV")
datanew1 <- na.exclude(datatr)
# Train the model
modelnew1 <- train(Next_Tmin~ station + Date + Present_Tmax + LDAPS_RHmin + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew1)


## ----vccttzkk77, echo=FALSE-------------------------------------------------------------------
#K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
modelnew2 <- train(Next_Tmin~ station + Date + Present_Tmax + LDAPS_RHmin + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew2)


## ----vzkkkkkkoo7, echo=FALSE------------------------------------------------------------------
#Repeated K-fold cross-validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
modelnew3 <- train(Next_Tmin~ station + Date + Present_Tmax + LDAPS_RHmin + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat, data= datanew1, method = "lm",
               trControl = train.control)
# Summarize the results
print(modelnew3)


## ----vzgkgoo7, echo=FALSE---------------------------------------------------------------------
#bootstrap
library(simpleboot)
set.seed(1)
bootres <- lm.boot(modelnew, R=10000, rows = TRUE, new.xpts = NULL, ngrid = 100,
        weights = NULL) 
summary(bootres)


## ----v88hhipp2, echo=FALSE--------------------------------------------------------------------
modelnonl <- lm(Next_Tmin~ station + Date + Present_Tmax + I(LDAPS_RHmin^2) + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat, data= datatr )
predinon<- predict(modelnonl, newdata= datate)
MAEvalue<- MAE(datate$Next_Tmin, as.numeric(predinon),na.rm=TRUE)
print(paste("MAE=",MAEvalue))
R2value<- R2(datate$Next_Tmin, as.numeric(predinon),na.rm=TRUE)
print(paste("R2=",R2value))
RMSEvalue<- RMSE(datate$Next_Tmin, as.numeric(predinon),na.rm=TRUE)
print(paste("RMSE=",RMSEvalue))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----v88hoop2, echo=FALSE---------------------------------------------------------------------
modelint <- lm(Next_Tmin~ station + Date + Present_Tmax + LDAPS_RHmin + LDAPS_Tmax_lapse +LDAPS_Tmin_lapse + LDAPS_WS + LDAPS_LH  + LDAPS_CC3 + LDAPS_PPT2 + LDAPS_PPT3 + lon + DEM + Slope + lat +LDAPS_PPT2* LDAPS_PPT3, data= datatr )
predint<- predict(modelint, newdata= datate)
MAEvalue<- MAE(datate$Next_Tmin, as.numeric(predint),na.rm=TRUE)
print(paste("MAE=",MAEvalue))
R2value<- R2(datate$Next_Tmin, as.numeric(predint),na.rm=TRUE)
print(paste("R2=",R2value))
RMSEvalue<- RMSE(datate$Next_Tmin, as.numeric(predint),na.rm=TRUE)
print(paste("RMSE=",RMSEvalue))
#res <- caret::postResample(datate$Next_Tmin, as.numeric(prediii))
#res


## ----v889642, echo=FALSE----------------------------------------------------------------------
library(ggplot2)
#¨Prepare predicted data from our best model (For Tmax it's the K-cross fold model)
predicteddata <- data.frame(pugh)
#Prepare scales for actual data and predicted data plot (we have different length due to the missing values in test data set)
scalex <- c(1:length(pugh))
scalex2 <- c(1:length(datate$Next_Tmax))

#ggplot(datate, aes(x=scalex))+
 # geom_line(aes( y =sub1x$Next_Tmax), color = "darkred") +
 # geom_line(aes(y = predicteddata), color="steelblue", linetype="twodash")

#Plot the two variables together
ggplot(datate,aes(scalex2,datate$Next_Tmax))+geom_line()+geom_line(data=predicteddata,aes(scalex,pugh ,color="Predicted data"))

#ggplot(datate, aes(x = scalex2, y = datate$Next_Tmax), color="red") +
 # theme_bw() +
 # geom_line() + # values mapped to aes() in ggplot() call
 # geom_line(data = predicteddata, aes(x = scalex, y = pugh), color="blue" ) # explicit mapping, can be different for this geom



## ----v88964ff2, echo=FALSE--------------------------------------------------------------------
library(ggplot2)
#¨Prepare predicted data from our best model (For Tmin it's the interaction model that is very similar to the first regression model)
predicteddata1 <- data.frame(predint)
#Prepare scales for actual data and predicted data plot (we have different length due to the missing values in test data set)
scalexm <- c(1:length(prediii))
scalex2m <- c(1:length(datate$Next_Tmin))
#ggplot(datate, aes(x=scalex))+
 # geom_line(aes( y =sub1x$Next_Tmax), color = "darkred") +
 # geom_line(aes(y = predicteddata), color="steelblue", linetype="twodash")

#Plot the two variables together
ggplot(datate,aes(scalex2m,datate$Next_Tmin))+ geom_line()+ geom_line(data=predicteddata1,aes(scalexm,predicteddata1$predint ,color="Predicted data"))

#ggplot(datate, aes(x = scalex2m, y = datate$Next_Tmin), color="red") +
 #theme_bw() +
 #geom_line() + # values mapped to aes() in ggplot() call
#geom_line(data = predicteddata1, aes(x = scalexm, y = predint), color="blue" ) # explicit mapping, can be different for this geom


