#Clear everything
rm(list=ls())
wd = getwd()
setwd(wd)
install.packages('PerformanceAnalytics')
install.packages('quantmod')
install.packages('tidyr')
install.packages('CVXR')
install.packages('resample')
library(readxl)
library(tidyr)
library(quantmod)
library(timetk)
library(plotly)
library(CVXR)
library(resample)
library(lubridate)
library(dplyr)

# get market data
getSymbols(c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW'), periodicity="daily",from="2017-11-24",to="2022-11-18")
KNT<-read_excel('KNT_daily.xlsx')
KNT<-data.frame(KNT)
#monthly risk free rate
getSymbols('DGS1MO',src = 'FRED') #download 1-month annualized risk free rate
#head(DGS1MO)
rf <- na.omit(DGS1MO) #remove NA. risk free
rf<-data.frame(rf)
rf$Date<-rownames(rf)
rf$Date<-ymd(rf$Date)
rf$year<-year(rf$Date)
rf$month<-month(rf$Date)
rf<-rf[rf$year<=2022 & rf$year>=2017,]
monthly.rf<-rf%>%
  group_by(year,month)%>%
  arrange(Date)%>%
  filter(row_number()==1)%>%
  mutate(DGS1MO=DGS1MO/(100*12)) #monthly risk free rate (percentage)
monthly.rf<-monthly.rf[12:(nrow(monthly.rf)-1),]
for(i in 2:nrow(monthly.rf)){
  monthly.rf$Date[i]<-ymd(monthly.rf$Date[i])+days(1)
}
colnames(monthly.rf)[1]<-'rf'
monthly.rf<-monthly.rf[,-(3:4)]
monthly.rf<-monthly.rf[,c('Date','rf')]
monthly.rf$Date<-as.character(monthly.rf$Date)

#get price table for all stocks and risk free
price<-cbind(ENPH$ENPH.Open,CELH$CELH.Open,XENE$XENE.Open,QCLN$QCLN.Open,ICLN$ICLN.Open,
             SMOG$SMOG.Open,PBW$PBW.Open)
price<-data.frame(price)
KNT<-data.frame(apply(KNT,2,rev))
pd<-rownames(price)
price$Date<-pd
price<-merge(price,KNT,by='Date')
#price<-merge(price,monthly.rf,by='Date')
colnames(price)<-c('Date','ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
for(c in 2:(ncol(price)-1)){
  price[,c]<-as.numeric(price[,c])
}
head(price)
nrow(price)

#last stock price in each month
monthly.price<-price %>% 
  mutate(Date=ymd(Date),
         year=year(Date),
         month=month(Date))%>%
  group_by(year,month)%>%
  arrange(Date)%>%
  filter(row_number()==1) #last day of a month
View(monthly.price)

for(i in 2:(length(monthly.price)-1)){
  monthly.price[,i]<-as.numeric(unlist(monthly.price[,i]))
}
price=monthly.price
#calculate return
return<-price[2:nrow(price),2:(ncol(price)-1)]/price[1:(nrow(price)-1),2:(ncol(price)-1)]-1
head(return)
price<-price[-1,]
return$Date<-price$Date
#get excess return
ex.ret<-return-(monthly.rf$rf)
ex.ret$Date<-return$Date
head(ex.ret)

######NO NEED TO CONSIDER BUDGET IN DOLLARS AND TRANSACTION COSTS AT CURRENT STAGE: we only decide percentage here.

stock.list<-c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
#define column names for all trade
col.n<-c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT','return','var','Date')
all.trade<-data.frame(matrix(0,ncol=length(col.n))) #data frame for trade on all stocks
colnames(all.trade)<-col.n

for(i in 2:nrow(ex.ret)){
  #current week: week i
  date<-ex.ret[i,'Date']
  rtn<-ex.ret[(i-1),1:(ncol(ex.ret)-2)]
  ####decision variables: W, a vector of weights (%) for ('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
  W <- Variable(length(stock.list))
  #####The optimization problem: rebalancing the position to get the highest expected return based on week i-1
  objective<-Maximize(rtn%*%W) #maximize weighted mean. vector multiplication applied
  #####The constraint: sum of weights <=0.9, all weights >=0 (No short)
  constraints <- list(W>=0, sum(W)<=0.9, W<=0.3) #<=0.5 to diversify. sum<=0.9 to leave cash in hand
  #####set as the problem
  prob <- Problem(objective, constraints)
  # Problem solution
  solution<- solve(prob)
  opt.w<-solution$getValue(W)
  opt.rtn<-solution$value
  vari<-opt.w[1,1]*(rtn[1]-opt.rtn)^2+opt.w[2,1]*(rtn[2]-opt.rtn)^2+opt.w[3,1]*(rtn[3]-opt.rtn)^2+opt.w[4,1]*(rtn[4]-opt.rtn)^2+
    opt.w[5,1]*(rtn[5]-opt.rtn)^2+opt.w[6,1]*(rtn[6]-opt.rtn)^2+opt.w[7,1]*(rtn[7]-opt.rtn)^2+opt.w[8,1]*(rtn[8]-opt.rtn)^2
  all.trade[nrow(all.trade)+1,]<-c(opt.w,opt.rtn,vari,date)
}

all.trade<-all.trade[2:nrow(all.trade),] #remove the first row which is all zero
options('scipen' = 40,'digits'=6)
all.trade$Date<-as.Date(all.trade$Date)
View(all.trade)
head(all.trade)

#calculate optimal shares for each stock
all.budget<-data.frame(matrix(0,ncol=length(col.n)))
colnames(all.budget)<-col.n
all.share<-data.frame(matrix(0,ncol=length(col.n)))
colnames(all.share)<-col.n

budget=1000000 #initial budget=1M

for(i in 2:nrow(all.trade)){
  #current week: week i 
  date<-all.trade[i,'Date']
  all.budget[i,1:length(stock.list)]<-as.numeric(all.trade[i,1:length(stock.list)])*budget
  #current budget is the current total portfolio value
  #budget<-all.share[i,1]*all.price[i,2]+all.share[i,2]*all.price[i,3]+all.share[i,3]*all.price[i,4]+all.share[i,4]*all.price[i,5]+
    #all.share[i,5]*all.price[i,6]+all.share[i,6]*all.price[i,7]+all.share[i,7]*all.price[i,8]+all.share[i,8]*all.price[i,9]
  budget=1000000*(0.9*(1+(all.trade$return[i-1]+monthly.rf$rf)[i])+0.1)
}

all.budget$Date<-all.trade$Date
all.price<-data.frame(matrix(0,ncol=length(col.n)))
colnames(all.price)<-col.n
all.price<-merge(price,all.budget,by='Date')
all.price<-all.price[,-(10:21)]


for(i in 1:nrow(all.budget)){
  #current week: week i 
  all.share[i,1:length(stock.list)]<-as.numeric(all.budget[i,1:length(stock.list)])/(as.numeric(all.price[i,2:length(stock.list)+1])*1.02)   
}

#get optimal integer shares
opt.share<-data.frame(floor(all.share[1:length(stock.list)]))
opt.share$Date<-all.trade$Date

#####Return#####
#Last Portfolio Value = shares*price on 11.18
(8469*92.84+5387*36.34+52904*48.71)/0.9-4181*4.66858+4181*7.17
#Mean of monthly excess return
avg_rtn=mean(all.trade$return)
print(avg_rtn)
#####Sharpe Ratio#####
sr<-data.frame(all.trade$return/sqrt(all.trade$var))
colnames(sr) <- c('sr')
sr$Date<-all.trade$Date

plot(sr$Date,sr$sr,type='l',xlab='time',ylab='Sharpe Ratio',
     main='Line plot of Sharpe Ratio over time (Reblancing, TC=2%)')
hist(sr$sr,main='Histogram of Sharpe Ratio (Rebalancing, TC=2%)',
     xlab='Sharpe Ratio')

#####VaR#####
h <- hist(all.trade$return, breaks = 100, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, xlab='Monthly excess return',
     ylab='Probability',main='VaR: Distribution of monthly excess return (Rebalancing, TC=1%)')