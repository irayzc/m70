project004 <- function(stock=1, hd=100, vd=365, ad=1, amv=10, pd=1000)   #hd: history depth in term of days # vd verification depth #amv: moving average #ad: order of difference
{
# load package with ARIMA function
#if (!require("tseries")) install.packages("tseries")
#if (!require("dplyr")) install.packages("dplyr") # use for table data frame
library(tseries)
library(dplyr)
# stocks data given in class
symb=c("HPQ","HD","MS","M","VZ","T","S","C","TGT","WMT","GM","XOM","F","YHOO","IBM","GOOGL","MSFT")
nmST=c("Hewlett-Packard","Home Depo","Morgan Stanley","Macys","Verizon","AT&T","Sprint","Citigroup","Target","Walmart","General Motors","Exxon","Ford Motor","Yahoo","IBM","Google","Microsoft")
ns=length(symb)
all=as.data.frame(matrix(NA,ncol=ns,nrow=10000))
names(all)=symb
min.ni=10000

# import all stock data
for(i in 1:ns)
{ 
  directory = "E:\\360cloud\\MATH\\MATH 70\\R\\hw7\\" # Ira's computer
  #directory = "C:/Users/Adan/Dropbox/School/Senior Year/16S/MATH 70/stocks/" # Adan's computer
	tabi= read.csv(paste(directory,symb[i],".csv",sep=""),stringsAsFactors=F)
	ni=nrow(tabi)
	if(min.ni>ni) min.ni=ni
	all[1:ni,i]=tabi[,7]
}

# reverse order of data from ascending order (newest data at bottom)
all=all[seq(from=min.ni,to=1,by=-1),]

# log return of stocks
LRT=log(all[2:min.ni,]/all[1:(min.ni-1),])
n=nrow(LRT);m=ncol(LRT)
Y <- LRT[(n-vd-hd+1):n,stock]
yy=roundUp(amv/4,to=1)
print(yy)
par(mfrow=c(4,yy),mar=c(3,3,2,1),omi=c(0,0,.25,0))
#par(mfrow=c(2,3),mar=c(3,3,2,1),omi=c(0,0,.25,0))
for (mv in 1:amv)
{
# ARIMA(1,ad,mv) forecasting
mydata.arima101 <- arima(Y, order = c(1,ad,mv))
mydata.pred1 <- predict(mydata.arima101, n.ahead=pd)
plot ((n-vd-hd+1):n,Y, xlab="days", ylab="log returns",xlim=c((n-vd-hd+1),(n+pd)),ylim=c(min(Y,mydata.pred1$pred),max(Y,mydata.pred1$pred)))
print(mv)
lines((n+1):(n+pd),mydata.pred1$pred, col="blue")
lines((n+1):(n+pd),mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines((n+1):(n+pd),mydata.pred1$pred-2*mydata.pred1$se, col="red")
}
}