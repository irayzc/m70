project003 <- function(stock=1, hd=100, vd=365, ad=10, amv=10 pd=1000)   #hd: history depth in term of days # vd verification depth #amv: moving average #ad: order of difference
{
# load package with ARIMA function
if (!require("tseries")) install.packages("tseries")
if (!require("dplyr")) install.packages("dplyr") # use for table data frame

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
  #directory = "E:\\360cloud\\MATH\\MATH 70\\R\\hw7\\" # Ira's computer
  directory = "C:/Users/Adan/Dropbox/School/Senior Year/16S/MATH 70/stocks/" # Adan's computer
	tabi= tbl_df(read.csv(paste(directory,symb[i],".csv",sep=""),stringsAsFactors=F))
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
par(mfrow=c(2,2),mar=c(3,3,2,1),omi=c(0,0,.25,0))

# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=pd)
plot ((n-vd-hd+1):n,Y, xlab="days", ylab="log returns",xlim=c((n-vd-hd+1),(n+pd)),ylim=c(min(Y,mydata.pred1$pred),max(Y,mydata.pred1$pred)))
print(length(mydata.pred1$pred))
print (length((n+1):(n+pd)))
lines((n+1):(n+pd),mydata.pred1$pred, col="blue")
lines((n+1):(n+pd),mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines((n+1):(n+pd),mydata.pred1$pred-2*mydata.pred1$se, col="red")

# ARIMA(1,1,1) forecasting
mydata.arima111 <- arima(Y, order = c(1,1,1))
mydata.pred1 <- predict(mydata.arima111, n.ahead=pd)
plot ((n-vd-hd+1):n,Y, xlab="days", ylab="log returns",xlim=c((n-vd-hd+1),(n+pd)),ylim=c(min(Y,mydata.pred1$pred),max(Y,mydata.pred1$pred)))
lines((n+1):(n+pd),mydata.pred1$pred, col="blue")
lines((n+1):(n+pd),mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines((n+1):(n+pd),mydata.pred1$pred-2*mydata.pred1$se, col="red")
}
}