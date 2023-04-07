#Creating a data frame out of dataset
mydata<-read.csv("C:\\Users\\MANOJ PATEL\\Documents\\opsd_germany_daily.csv",header = TRUE,row.names = "Date")
head(mydata)
tail(mydata)
View(mydata)
dim(mydata)
str(mydata)
head(mydata$Date)
head(mydata$Consumption)
row.names(mydata)
mydata["2006-01-01",]
mydata[c('2006-01-01','2007-01-01'),]
mydata2<-read.csv("C:\\Users\\MANOJ PATEL\\Documents\\opsd_germany_daily.csv",header = TRUE)
dim(mydata2)
str(mydata2$Date)
x<-as.Date(mydata2$Date)
head(x)
class(x)
str(x)
year<-as.numeric(format(x,'%Y'))
head(year)
month<-as.numeric(format(x,'%m'))
head(month)
day<-as.numeric(format(x,'%d'))
head(day)
mydata2<-cbind(mydata2,year,month,day)
head(mydata2)
mydata2[1:3,]
head(sample(mydata2,8))
#Visualise the data
plot(mydata2$year,mydata2$Consumption,type = "l",xlab = 'Year',
     ylab = 'Consumption',xlim = c(2006,2016),ylim=c(800,1800))
plot(mydata2[,2])
plot(mydata2[,2],xlab = 'Year',
     ylab = 'Consumption',type = 'l',
     lwd=2,col='green',xlim = c(2006,2018))
library(ggplot2)
ggplot(mydata2,type='o')+geom_line(aes(x=year,
                                       y=Consumption,group=1))
str(mydata2)
x<-as.Date(mydata2$Date)
head(x)
moddate<-as.Date(x,format="%m/%d/%Y")
str(moddate)
head(moddate)
mydata3<-cbind(moddate,mydata2)
head(mydata3)
str(mydata3)
mydata4 = subset(mydata3,
                 subset=mydata3$moddate>='2017-01-01' & mydata3$moddate<='2017-12-31')
head(mydata4)
plot4<-plot(mydata4[,1],mydata4[,3],xlab = 'Year',
            ylab = 'Daily Totals',type = 'l',
            lwd=1,main = 'Consumption',col="orange")
mydata41 = subset(mydata3,
                 subset=mydata3$moddate>='2017-01-01' & mydata3$moddate<='2017-02-28')

plot5<-plot(mydata41[,1],mydata41[,3],xlab = 'Year',
            ylab = 'Daily Totals',type = 'l',
            lwd=2,main = 'Consumption',col="orange")

grid()
abline(h=c(1300,1500,1600))
abline(v=seq(min(mydata41[,1],na.rm=T),max(mydata41[,1],na.rm=T),7),lty=2,col='blue')
boxplot(mydata3$Consumption)
boxplot(mydata3$Solar)
boxplot(mydata3$Wind)
quantile(mydata3$Consumption,probs = c(0,0.25,0.5,0.75,1))
boxplot(mydata3$Consumption~mydata3$year,main='Consumption',
        ylab = 'Consumption',xlab = 'years',
        ylim=c(600,1800))
boxplot(mydata3$Consumption~mydata3$month,main='Consumption',
        ylab = 'Consumption',xlab = 'Months',
        ylim=c(600,800),las=1)


par(mfrow=c(3,1))
boxplot(mydata3$Consumption~mydata3$year,main='Consumption',
        ylab = 'Consumption',xlab = 'years',
        ylim=c(600,1800),las=1,col='blue')
boxplot(mydata3$Solar~mydata3$year,main='Solar',
        ylab = 'Solar',xlab = 'years',
        ylim=c(0,200),las=1,col='red')
boxplot(mydata3$Wind~mydata3$year,main='Wind',
        ylab = 'Wind',xlab = 'years',
        ylim=c(0,900),las=1,col='green')

par(mfrow=c(3,1))
boxplot(mydata3$Consumption~mydata3$month,main='Consumption',
        ylab = 'Consumption',xlab = 'month',
        ylim=c(600,1800),las=1,col='blue')
boxplot(mydata3$Solar~mydata3$month,main='Solar',
        ylab = 'Solar',xlab = 'month',
        ylim=c(0,200),las=1,col='red')
boxplot(mydata3$Wind~mydata3$month,main='Wind',
        ylab = 'Wind',xlab = 'month',
        ylim=c(0,900),las=1,col='green')

par(mfrow=c(1,1))
boxplot(mydata3$Consumption~mydata3$day)
library(dplyr)
summary(mydata3)
colSums(!is.na(mydata3))
sum(is.na(mydata3$Solar))
#Frequencies
xmin<-min(mydata3[,1],na.rm=T)
xmin
freq1<-seq(from=xmin,by='month',length.out=5)
freq1
freq2<-seq(from=xmin,by='year',length.out=5)
freq2
#let's select data which does not have na values for wind
selwind1<-mydata3[which(!is.na(mydata3$Wind)),
                  names(mydata3) %in% c('moddate','Consumption','Wind','Solar')]
selwind1[1:10,]
View(selwind1)
#let's select data  have na values for wind
selwind2<-mydata3[which(is.na(mydata3$Wind)),
                  names(mydata3) %in% c('moddate','Consumption','Wind','Solar')]
selwind2[1:10,]
View(selwind2)
#let's select data  have na values for wind
selwind3<-mydata3[which((mydata3$year=='2011')),
                  names(mydata3) %in% c('moddate','Consumption','Wind','Solar')]
selwind3[1:10,]
View(selwind3)
nrow(selwind3)
sum(is.na(mydata3$Wind[which(mydata3$year=='2011')]))
sum(!is.na(mydata3$Wind[which(mydata3$year=='2011')]))
str(selwind1)
selwind4<-selwind3[which(is.na(selwind3$Wind))
                   ,names(selwind3) %in% c('moddate','Consumption','Wind','Solar')]
selwind4
test1<-selwind3[which(selwind3$moddate>'2011-12-12' & selwind3$moddate<'2011-12-16'),
                names(selwind3) %in% c('moddate','Consumption','Wind','Solar')]
test1
