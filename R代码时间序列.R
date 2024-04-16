setwd("D:/R studio/RStudio/R作业数据")
e<-read.table("时间序列.csv",sep=",",header = T)
x<-ts(e$旅游总收入.亿元,start = 2012,frequency = 1)
plot(x,type="o",pch=17,col=2,main = "2012-2023年福建省旅游总收入")

#1阶差分，并绘制出差分后序列的时序图
x.dif<-diff(x)
plot(x.dif,type="o",pch=17,col=2,main = "2012-2023年福建省旅游总收入的一阶差分")
#ADF检验
for (i in 1:3) print(adfTest(x.dif,lag=i,type='nc'))

#2阶差分，并绘制出差分后序列的时序图
x.dif2<-diff(x,1,2)
plot(x.dif2,type="o",pch=17,col=2,main = "2012-2023年福建省旅游总收入的二阶差分")
#ADF检验
for (i in 1:3) print(adfTest(x.dif2,lag=i,type='nc'))


#绘制差分后序列自相关图和偏自相关图
par(mfrow=c(2,1))  #设置2个图
acf(x.dif2)
pacf(x.dif2)
par(mfrow=c(1,1))  

#识别模型
#ARIMA(0,2,1)
x.fit_ar<-arima(x.dif2,order=c(0,2,1),include.mean = T, method = "ML")
x.fit_ar
#ARIMA(0,2,0)
x.fit_ar2<-arima(x.dif2,order=c(0,2,0),include.mean = T, method = "ML")
x.fit_ar2

#做5期预测，并绘制预测图
x.fore<-forecast(x.fit_ar,h=5,level = 95)
x.fore
plot(x.fore)