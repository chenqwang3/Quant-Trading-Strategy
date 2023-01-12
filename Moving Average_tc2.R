#Clear everything
rm(list=ls())

library(readxl)
library(ggplot2)
library(quantmod)
library(lubridate)
library(tidyverse)
library(plyr)

#set working directory
##PLEASE REMEMBER TO CHANGE IT TO YOUR OWN WORKING DIRECTORY WHEN RUNNING THIS. THANK YOU.
setwd('/Users/catherine/Downloads/EF4328/Project')

#obtain the daily price of the seven assets during the past five years
getSymbols(c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW'), periodicity="daily",from="2017-11-24",to="2022-11-18")
#read the daily open price of KNT during the past five years
KNT<-read_excel('KNT_daily.xlsx')
KNT<-data.frame(KNT)

#merge the open prices of seven assets
price<-cbind(ENPH$ENPH.Open,CELH$CELH.Open,XENE$XENE.Open,QCLN$QCLN.Open,ICLN$ICLN.Open,SMOG$SMOG.Open,PBW$PBW.Open)
price<-data.frame(price)
#Add the open prices of KNT to the merged dataframe
KNT<-data.frame(apply(KNT,2,rev))
pd<-rownames(price)
price$Date<-pd
price<-merge(price,KNT,by='Date')
colnames(price)<-c('Date','ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
nrow(price)
head(price)
               
#define column names for all trades
col.n<-c('Date','stock','order.num','position','execution.price','size','num.shares','cash')
all.trade<-data.frame(matrix(0,ncol=length(col.n))) #dataframe for trades on all assets
colnames(all.trade)<-col.n

tc<-0.02 #set the transaction cost for all trades on eight assets
stocks<-c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')

###########################
######SMA Trading Rule#####
###########################
#define a function to simulate the simple moving average trading on the specified equity/ETF
ind_SMA.trade <- function(st) {
  stock<-price[,c('Date',st)]
  colnames(stock)[2]<-'price'
  stock$price<-as.numeric(stock$price)
  trade<-data.frame(matrix(0,ncol=length(col.n))) #data frame for trade on the stock
  colnames(trade)<-col.n
  #calculate the 5-day and 50-day moving average open prices
  moving.avg5 <- SMA(stock$price, 5)
  moving.avg50 <- SMA(stock$price, 50)
  moving.avg5<-data.frame(moving.avg5)
  moving.avg5$Date<-stock$Date
  moving.avg50<-data.frame(moving.avg50)
  moving.avg50$Date<-stock$Date
  
  num.shares<-0 #initial number of shares owned is 0
  ord.n<-1 #order number for the first trade of the stock 
  cash<-125000 #assign $125,000=$1M/8 to each equity/ETF so initial cash is $125000 
  
  for(i in 50:(nrow(stock)-1)){
    mv5<-moving.avg5[[1]][i]
    mv50<-moving.avg50[[1]][i]
    ep<-stock$price[i+1] #execution price for this trade is the open price on the next trading day
    date<-stock$Date[i+1] #execution date is the next trading day
    
    if(mv5>mv50 & cash>0){ #Enter long when 5 days moving average crosses above 50 days moving average and we have free cash
      t.size<-floor(cash/(ep*(1+tc))) #trading size
      if(t.size>=1){
        cash<-cash-t.size*ep*(1+tc) #remaining cash on hand after this trade
        num.shares<-t.size+num.shares #number of shares holding after this trade
        trade[nrow(trade)+1,]<-c(date, st, ord.n, 'Enter', ep, t.size, num.shares, cash) #record the "Enter" trade with pre-defined variables
        ord.n<-ord.n+1 #add 1 to the order number of this asset
      }
    }else if(mv5<mv50 & num.shares>0){ #Exit long when 5 days moving average crosses below 50 days moving average
      t.size<-num.shares #the trading size equals the total number of shares on hand because we sell all the shares
      cash<-cash+t.size*ep*(1-tc) #remaining cash on hand after this trade
      num.shares=0 #no remaining shares on hand
      trade[nrow(trade)+1,]<-c(date, st, ord.n, 'Exit', ep, t.size, num.shares, cash)#record the "Exit" trade with pre-defined variables
      ord.n<-ord.n+1 #add 1 to the order number of this asset
    }
  }
  trade<-trade[2:nrow(trade),] #remove the first row which is all zero
  return(trade)
}  

for(st in stocks){
  trade<-ind_SMA.trade(st) #apply the ind_SMA.trade() function to obtain all SMA trades for each asset
  all.trade<-rbind(all.trade,trade) #merge the trades on eight assets
}
all.trade<-all.trade[2:nrow(all.trade),] #remove the first row which is all zero
View(all.trade)


###########################
##Performance measurement##
###########################
#get the last stock price in each month
head(price)
monthly.price<-price %>% 
  mutate(Date=ymd(Date),
         year=year(Date),
         month=month(Date))%>%
  group_by(year,month)%>%
  arrange(Date)%>%
  filter(row_number()==n()) #last day of a year
View(monthly.price)

#monthly stock return
for(i in 2:(length(stocks)+1)){
  monthly.price[,i]<-as.numeric(unlist(monthly.price[,i]))
}
monthly.rtn<-monthly.price[2:nrow(monthly.price),2:(length(stocks)+1)]/monthly.price[1:(nrow(monthly.price)-1),2:(length(stocks)+1)]-1
monthly.rtn[,c('year','month')]<-monthly.price[2:nrow(monthly.price),c('year','month')]
View(monthly.rtn) 

#monthly risk free rate
getSymbols('DGS1MO',src = 'FRED') #download 1-month annualized risk free rate (US 1-month treasuary bill)
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
  filter(row_number()==n())%>%
  mutate(DGS1MO=DGS1MO/(100*12)) #monthly risk free rate (percentage)
monthly.rf<-monthly.rf[12:(nrow(monthly.rf)-1),]
colnames(monthly.rf)[1]<-'rf'
View(monthly.rf)

#stock excess return
monthly.ex.rtn<-monthly.rtn[,1:8]-monthly.rf$rf
monthly.ex.rtn<-monthly.ex.rtn[3:nrow(monthly.ex.rtn),]
View(monthly.ex.rtn)

#define a function to obtain the month-end position of each asset
get.monthly.pos<-function(st){
  pos<-all.trade%>%
    filter(stock==st)%>%  #get trade on a single stock
    mutate(year= year(Date),
           month=month(Date)) %>%
    group_by(year,month) %>% # group by month and year
    arrange(Date) %>% # make sure the df is sorted by date
    filter(row_number()==n())%>%
    select(stock, num.shares, cash)
  pos<-data.frame(pos)
  pos<-merge(pos,monthly.price[,c(st,'year','month','Date')], by=c('year','month'), all.y = TRUE)
  pos[is.na(pos)] = 0
  pos$stock<-st
  for(p in 4:nrow(pos)){
    if(pos[p,'cash']==0 & pos[p,'num.shares']==0){
      pos[p,c('cash','num.shares')]=pos[p-1,c('cash','num.shares')]
    }
  }
  pos$val.shares<-as.numeric(pos$num.shares) * as.numeric(pos[,st])
  colnames(pos)[6]<-'price'
  return(pos)
}

monthly.pos<-data.frame(matrix(0,ncol=length(c('year','month','stock','num.shares','cash','price','Date','val.shares')))) #dataframe for position on all stocks
colnames(monthly.pos)<-c('year','month','stock','num.shares','cash','price','Date','val.shares')
for(st in stocks){
  pos<-get.monthly.pos(st)
  monthly.pos<-rbind(monthly.pos,pos)
}
monthly.pos$Date<-as.Date(monthly.pos$Date)
monthly.pos<-monthly.pos[2:nrow(monthly.pos),] #remove the first row which is all zero
#monthly.pos$total.val<-as.numeric(monthly.pos$cash)+as.numeric(monthly.pos$val.shares)
View(monthly.pos)

##################
###Sharpe Ratio###
##################

perc.monthly.pos<-monthly.pos[monthly.pos$stock=='ENPH',c('year','month')]
for(st in stocks){
  temp<-monthly.pos[monthly.pos$stock==st,c('val.shares','cash')] #paste value of shares and cash at the end of each month to perc.monthly.pos
  colnames(temp)<-c(paste(st,'.val.shares',sep=''),paste(st,'.cash',sep=''))
  perc.monthly.pos<-cbind(perc.monthly.pos,temp)
}
for(i in 3:ncol(perc.monthly.pos)){
  perc.monthly.pos[,i]<-as.numeric(perc.monthly.pos[,i])
}
#get total value of portfolio at the end of each month
perc.monthly.pos$port.value<-rowSums(perc.monthly.pos[,3:ncol(perc.monthly.pos)]) #sum of value of shares and cash among all stocks
#share value & cash value divided by total port value to get weights (%)
perc.monthly.pos[,3:(ncol(perc.monthly.pos)-1)]<-perc.monthly.pos[,3:(ncol(perc.monthly.pos)-1)]/perc.monthly.pos$port.value
row.names(perc.monthly.pos)<-c(1:nrow(perc.monthly.pos))
perc.monthly.pos<-perc.monthly.pos[4:nrow(perc.monthly.pos),] #delete first three rows with no return as the start of mv50 is in Feb 2018
View(perc.monthly.pos)

#get weighted expected excess return of portfolio at the end of each month
w.rtn<-perc.monthly.pos[,c('year','month')]
for(st in stocks){
  temp<-perc.monthly.pos[,paste(st,'.val.shares',sep='')]*monthly.ex.rtn[,st] #weighted return of the stock
  w.rtn<-cbind(w.rtn,temp)
  colnames(w.rtn)[ncol(w.rtn)]<-st
}
w.rtn$rtn<-rowSums(w.rtn[,3:ncol(w.rtn)])
View(w.rtn)

#get weighted standard deviation of portfolio return at the end of each month
diff.ex.rtn.mean<-monthly.ex.rtn[,stocks]-w.rtn$rtn #stock excess return - expected port return 
View(diff.ex.rtn.mean)
w.std<-perc.monthly.pos[,c('year','month')]
for(st in stocks){
  temp<-sqrt(perc.monthly.pos[,paste(st,'.val.shares',sep='')]*(diff.ex.rtn.mean[,st])^2) #std
  w.std<-cbind(w.std,temp)
  colnames(w.std)[ncol(w.std)]<-st
}
w.std$std<-rowSums(w.std[,3:ncol(w.std)])
View(w.std)

#calculate sharpe ratio
sr<-perc.monthly.pos[,c('year','month')]
sr$rtn<-w.rtn$rtn
sr$std<-w.std$std
sr$sr<-sr$rtn/sr$std
start_date <- as.Date("2018/02/01")
end_date <- as.Date("2022/11/30")
# generating range of dates
range <- seq(start_date, end_date,"months")
length(range)
sr$Date<-range
for (i in 1:nrow(sr)){
  sr[is.na(sr)] = 0
}
View(sr)

plot(sr$Date,sr$sr,type='l',xlab='time',ylab='Sharpe Ratio',
     main='Line plot of Sharpe Ratio over time (Moving average, TC=2%)')
hist(sr$sr,main='Histogram of Sharpe Ratio (Moving average, TC=2%)',
     xlab='Sharpe Ratio')

##Average of monthly excess return
mean(sr$rtn)

#number of transactions
nrow(all.trade)

#########
###VaR###
#########

h <- hist(sr$rtn, breaks = 100, plot=FALSE)
h$counts=h$counts/sum(h$counts)
max(h$counts)
plot(h, xlab='Monthly excess return',
     ylab='Probability',main='VaR: Distribution of monthly excess return (Moving Average, TC=2%)')
axis(side=2, at=c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16), labels = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16))


benchmark<-read_excel('benchmark.xlsx')
ben.rtn<-benchmark[1,'PX_OPEN']/benchmark[nrow(benchmark),'PX_OPEN']-1 #benchmark return 0.3075244
total.port.value<-perc.monthly.pos[nrow(perc.monthly.pos),'port.value']
total.port.value #portfolio value $4,548,476
total.port.rtn<-total.port.value/1000000-1 #portfolio return 3.548476
ben.rtn
total.port.rtn





