#Clear everything
rm(list=ls())
library(readxl)
library(ggplot2)
library(quantmod)
library(lubridate)
library(tidyverse)
library(plyr)

#set working directory
##PLEASE REMEMBER TO CHANGE IT TO YOUR OWN WORKING DIRECTORY WHEN RUNNING THIS 
##SCRIPT ON YOUR OWN COMPUTER. THANK YOU.
setwd('Documents/EF4328/Project')

getSymbols(c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW'), periodicity="daily",from="2017-11-24",to="2022-11-18")
KNT<-read_excel('KNT_daily.xlsx')
KNT<-data.frame(KNT)
nrow(KNT)
nrow(ENPH)

price<-cbind(ENPH$ENPH.Open,CELH$CELH.Open,XENE$XENE.Open,QCLN$QCLN.Open,ICLN$ICLN.Open,
             SMOG$SMOG.Open,PBW$PBW.Open)
price<-data.frame(price)
KNT<-data.frame(apply(KNT,2,rev))
pd<-rownames(price)
price$Date<-pd
price<-merge(price,KNT,by='Date')
colnames(price)<-c('Date','ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
nrow(price)
head(price)
#########################################################################
##STRATEGY: assign $125,000=$1M/8 to each equity/ETF                   ##
##Num of shares at each trade of each equity/ETF=$125,000/highest price##
#########################################################################
stocks<-c('ENPH','CELH','XENE','QCLN','ICLN','SMOG','PBW','KNT')
#define column names for all trade
col.n<-c('stock','order.num','position','Date','open.price','execution.price',
         'size','num.shares','cash','status')
all.trade<-data.frame(matrix(0,ncol=length(col.n))) #dataframe for trade on all stocks
colnames(all.trade)<-col.n
tc<-0.02 #transaction cost for all stocks

reversal <- function(st) {
  num.shares<-0 #number of shares owned
  ord.n<-1 #order number for the first trade of the stock 
  cash<-125000 #initial cash of $125000 
  stock<-price[,c('Date',st)]
  colnames(stock)[2]<-'price'
  stock$price<-as.numeric(stock$price)
  #define size of each trade on stock st:
  t.size<-floor(125000/(max(stock$price)*(1+tc)))
  trade<-data.frame(matrix(0,ncol=length(col.n))) #data frame for trade on the stock
  colnames(trade)<-col.n
  for(i in 1:(nrow(stock)-1)){
    op<-stock[i,'price'] #opening price of that period (current price)
    period<-stock[i,'Date']
    ep<-stock[i+1,'price'] #opening price of that period (price of next day)
    ##check if current price is higher than execution price of any previous unsold long orders
    ##if so, sell the earliest one of such order at opening price of next day (i.e., price of next period)
    if(!empty(trade[(trade$status=='entered')&(trade$execution.price<op),])){
      prev.long<-trade[(trade$status=='entered')&(trade$execution.price<op),]
      lp<-prev.long[1,'execution.price'] #execution price of the long order to be exited now
      ln<-prev.long[1,'order.num'] #order number of the long order to be sold now
      cash<-cash+t.size*op*(1-tc) #cash obtained from exit minus transaction cost. executed at open price of today
      num.shares<-num.shares-t.size
      trade[nrow(trade)+1,]<-c(st, ord.n,'Exit',period,lp,op,-t.size,
                               num.shares,cash,'to.exit')#period is period of execution price (day i+1) 
      trade[trade$order.num==ln,'status']<-'exited' #change the status of the exited long order to "exited"
      ord.n<-ord.n+1
    }
    if((op>ep) & (cash>=(ep*t.size*(1+tc)))){ #buy when open price > execution price & current cash > cash needed for the potential long order
      num.shares<-num.shares+t.size #number of shares owned increases by trading size
      cash<-cash-ep*t.size*(1+tc) #reduce cash by trading size * execution price *(1+ percent of transaction cost)
      trade[nrow(trade)+1,]<-c(st, ord.n,'Enter',period,op,ep,t.size,
                               num.shares,cash,'entered') #period is period of opening price
      ord.n<-ord.n+1
    }
  }
  trade<-trade[2:nrow(trade),] #remove the first row which is all zero
  return(trade)
}

for(st in stocks){
  trade<-reversal(st)
  all.trade<-rbind(all.trade,trade)
}
all.trade<-all.trade[2:nrow(all.trade),] #remove the first row which is all zero
View(all.trade)

###########################
##Performance measurement##
###########################
#last stock price in each month
head(price)
monthly.price<-price %>% 
  mutate(Date=ymd(Date),
         year=year(Date),
         month=month(Date))%>%
  group_by(year,month)%>%
  arrange(Date)%>%
  filter(row_number()==n()) #last day of a month
View(monthly.price)

#monthly stock return
for(i in 2:(length(stocks)+1)){
  monthly.price[,i]<-as.numeric(unlist(monthly.price[,i]))
}
monthly.rtn<-monthly.price[2:nrow(monthly.price),2:(length(stocks)+1)]/monthly.price[1:(nrow(monthly.price)-1),2:(length(stocks)+1)]-1
monthly.rtn[,c('year','month')]<-monthly.price[2:nrow(monthly.price),c('year','month')]
View(monthly.rtn) 

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
  filter(row_number()==n())%>%
  mutate(DGS1MO=DGS1MO/(100*12)) #monthly risk free rate (percentage)
monthly.rf<-monthly.rf[12:(nrow(monthly.rf)-1),]
colnames(monthly.rf)[1]<-'rf'
View(monthly.rf)

#stock excess return
monthly.ex.rtn<-monthly.rtn[,1:8]-monthly.rf$rf
View(monthly.ex.rtn)

#set function to get position at the end of each month
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
  for(p in 1:nrow(pos)){
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
perc.monthly.pos<-perc.monthly.pos[2:nrow(perc.monthly.pos),] #delete first row since the stock return at nov 2017 is not provided
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
#get weighted standard deviation of portfolio at the end of each month
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
start_date <- as.Date("2017/12/01")
end_date <- as.Date("2022/11/30")
# generating range of dates
range <- seq(start_date, end_date,"months")
length(range)
sr$Date<-range
View(sr)

plot(sr$Date,sr$sr,type='l',xlab='time',ylab='Sharpe Ratio',
     main='Line plot of Sharpe Ratio over time (Reversal, TC=2%)')
hist(sr$sr,main='Histogram of Sharpe Ratio (Reversal, TC=2%)',
     xlab='Sharpe Ratio')

#total value of portfolio at the end of trading period
perc.monthly.pos[nrow(perc.monthly.pos),'port.value'] #715235.8

##Average of monthly excess return
mean(sr$rtn) #0.01485537

#number of transactions
nrow(all.trade)

#########
###VaR###
#########

h <- hist(sr$rtn, breaks = 100, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, xlab='Monthly excess return',
     ylab='Probability',main='VaR: Distribution of monthly excess return (Reversal, TC=2%)')


benchmark<-read_excel('benchmark.xlsx')
ben.rtn<-benchmark[1,'PX_OPEN']/benchmark[nrow(benchmark),'PX_OPEN']-1 #benchmark return 0.3075244
total.port.rtn<-perc.monthly.pos[nrow(perc.monthly.pos),'port.value']/1000000-1 
ben.rtn #0.3075244
total.port.rtn #-0.2847642 #cumulative return


