load("C:/Users/stat/Desktop/project/data_07.RData")
library(dplyr)

names(data_07)[c(1,39)] <- c("tg","date")

data_11 <- data_07 %>% group_by(tg) %>% 
  summarise(tr_total_sum = sum(traffic_total_sum, na.rm=T),
            tr_total_mean = mean(traffic_total_sum, na.rm=T),
            temp = mean(temp, na.rm=T),
            ap_mean = mean(ap, na.rm=T),
            ap_sum = sum(ap, na.rm=T),
            wet = mean(wet, na.rm=T),
            ws = mean(ws, na.rm=T),
            NO2 = mean(NO2, na.rm=T),
            O3 = mean(O3, na.rm=T),
            CO = mean(CO, na.rm=T),
            SO2 = mean(SO2, na.rm=T),
            pm10_mean = mean(PM10, na.rm=T),
            pm10_sum = sum(PM10, na.rm=T),
            pm25_mean = mean(PM25, na.rm=T),
            pm25_sum = sum(PM25, na.rm=T),
            al_pm10 = mean(al_pm10, na.rm=T),
            al_pm25 = mean(al_pm25, na.rm=T),
            al_pm = mean(al_pm, na.rm=T))

data <- NULL
a <- data_11
a$l1_temp <- c(NA,unlist(sapply(1:nrow(a), function(x) a$temp[x-1])))
a$l1_wet <- c(NA,unlist(sapply(1:nrow(a), function(x) a$wet[x-1])))
a$l1_ws <- c(NA,unlist(sapply(1:nrow(a), function(x) a$ws[x-1])))
a$l1_pm10_mean <- c(NA,unlist(sapply(1:nrow(a), function(x) a$pm10_mean[x-1])))
a$l1_pm25_mean <- c(NA,unlist(sapply(1:nrow(a), function(x) a$pm25_mean[x-1])))
a$l1_al_pm10 <- c(NA,unlist(sapply(1:nrow(a), function(x) a$al_pm10[x-1])))
a$l1_al_pm25 <- c(NA,unlist(sapply(1:nrow(a), function(x) a$al_pm25[x-1])))
a$l1_al_pm <- c(NA,unlist(sapply(1:nrow(a), function(x) a$al_pm[x-1])))
a$l1_ap_sum <- c(NA,unlist(sapply(1:nrow(a), function(x) a$ap_sum[x-1])))
a$l1_ap_mean <- c(NA,unlist(sapply(1:nrow(a), function(x) a$ap_mean[x-1])))
data <- list(a)

data_12 <- do.call(rbind,data)
names(data_12)

# ?????? ?????? - ??????/주말 구분
library(tidyverse)
library(lubridate)

# ?????? 주말 구분
a <- substr(data_12$tg, 1, 8)
a1 <- ymd(a)
data_12$day <- wday(a1) # ?????????~????????? : 1~7

data_12$weekend <- 1
data_12[2<=data_12$day & data_12$day<=6,]$weekend <- 0

# 계절 변??? ??????
data_12$month <- as.numeric(substr(data_12$tg, 5, 6))
data_12$season <- 0
data_12[3 <= data_12$month & data_12$month <= 5,]$season <- 1
data_12[6 <= data_12$month & data_12$month <= 8,]$season <- 2
data_12[9 <= data_12$month & data_12$month <= 11,]$season <- 3
data_12[12 <= data_12$month | data_12$month <= 2,]$season <- 4

# 공휴??? ??????
load("C:/Users/hsjeong/Desktop/project/eventday.RData")
duplicated(eventday$item.locdate) # 중복 ??????
event <- eventday[-30,] # 개천???, 추석 중복 - 2017??? 10??? 3???
names(event) <- c("a","datename","a1","date","a2")

data_12$date <- substr(data_12$tg,1,8)

data_12 <- merge(x=data_12, y=event[,c(2,4)], by="date", all.x=TRUE)

event <- as.character(c(20160207:20160210, 20160914:20160916, 20170127:20170130, 20171003:20171006, 20180215:20180217, 
                        20180923:20180926, 20190204:20190206, 20190912:20190914))

dt <- NULL;
for (i in 1:length(event)){
  dt[i] <- list(data_12[data_12$date == event[i],])
}

dt1 <- do.call(rbind, dt)
dt1$event <- 1

dt2 <- dt1[,c(2,36)] # tg, sigun, event

data_13 <- merge(x=data_12,
                 y=dt2,
                 by =  "tg",
                 all.x = TRUE)

data_13[is.na(data_13$event),]$event <- 0

data_13$year <- substr(data_13$date, 1,4)

# save(data_13, file="C:/Users/hsjeong/Desktop/project/data_13.RData")


library(ggplot2)
ggplot(data_13, aes(x=date, y=tr_total_sum)) + geom_point()

library(gridExtra)
g1 <- ggplot(data_13, aes(x=date, y=temp)) + geom_point()
g2 <- ggplot(data_13, aes(x=date, y=ws)) + geom_point()
g3 <- ggplot(data_13, aes(x=date, y=ap_sum)) + geom_point()
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
g4 <- ggplot(data_13, aes(x=date, y=pm10_mean)) + geom_point()
g5 <- ggplot(data_13, aes(x=date, y=pm25_mean)) + geom_point()
grid.arrange(g1,g2,g3,g4,g5, nrow=3, ncol=2)


##################### spline regression #####################
install.packages(c("rsample","ggplot2","earth","caret","vip","pdp"))

library(rsample)   # data splitting 
library(ggplot2)   # plotting
library(vip)       # variable importance
library(pdp)       # variable relationships

#############################################################
require(splines)
require(ISLR)

attach(Wage)
head(Wage)

agelims<-range(age)
age.grid<-seq(from=agelims[1], to = agelims[2])

fit <- lm(wage ~ bs(age, knots = c(25,40,60)), data=Wage)
summary(fit)

plot(age,wage,col="grey", xlab="Age", ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

################################################################
library(mgcv)
library(ggplot2)
library(moonBook)
library(ztable)
library(survival)

# devtools::install_github("cardiomoon/ggGam")
library(ggGam)
require(mgcv)
require(ggGam)
library(gamair)

data(mpg,package="gamair")
head(mpg)
m <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
         data=mpg, method="REML")

############# 분석 시작 #############
load("C:/Users/stat/Desktop/project/data_13.RData")
names(data_13)

# ??????, 출퇴근시간만 비교, 주말 ?????? 비교, 공휴???
m <- gam(tr_total_sum ~ s(l1_temp) + s(l1_ap_mean) + s(l1_wet) + s(l1_ws) + s(l1_pm10_mean) + s(l1_pm25_mean) 
         + factor(season) + factor(event) + factor(weekend) + factor(l1_al_pm), data=data_13, method="REML")
summary(m)

# plotting
ggGam(m, shift=coef(m)[1])
plot(m,residuals=TRUE,pch=1,shade=TRUE,seWithMean = TRUE,shift=coef(m)[1])


# 기??? ????????? ???가 ????????? ??????갔는지 ??????
gam.check(m)

# https://bookdown.org/cardiomoon/gam/tab-36.html
# https://stat.snu.ac.kr/heeseok/teaching/asm17/npreg_%EB%B0%9C%ED%91%9C.pdf
# https://liujingjun.tistory.com/90

