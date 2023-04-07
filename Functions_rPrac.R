 manoj<-function(arg_1,arg2){
   for (i in 1:arg_1){
     print(i)
   }
   print(paste('hello',arg2))
   return(arg_1+arg2)
 }
print(manoj(4,6) )
normd<-rnorm(1000,1,10)
mean(normd)
hist(normd,50)
v<-"I am global variable"
#Switch function
HRA<-function(city){
  hra_amt<-switch (toupper(city),
    BLR = 7500,
    MUM = 1000,
    DEL = 8000,
    CHN = 7500,
    5000
  )
  return(hra_amt)
}
HRA('blr')
#Repeat
time<-15
repeat{
  message('Heloo')
  if(time>=20) break
  time=time+1
}
#Built In function 
x=seq(0,50,2)
x
sort(x,TRUE)
text<-'R is a programming Language for Data science'
grep('Language',text)
# Factors in R
bloodgroup<-c('B','AB',"O","O","A","B",'AB','A')
blood_fac<-factor((bloodgroup))
blood_fac
str(blood_fac)
#Working with timestamps
Sys.Date()
install.packages('dplyr')
library(dplyr)
install.packages('nycflights13')
library('nycflights13') 
View(flights)
head(flights)
man<-data.frame(
  ID=c(1:6),
  Face.1=c(5:10),
  Face.2=c(10:15),
  Face.3=c(15:20)
)
View(man)
lo<-man%>%gather(face,Rsa,Face.1:Face.3)
View(lo)
mul<-lo%>%separate(face,c("Target","Number"))
View(mul)



plot(ChickWeight)













