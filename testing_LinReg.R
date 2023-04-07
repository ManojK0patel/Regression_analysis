mydata<-read.csv("C:\\Users\\MANOJ PATEL\\ED5340 Lab\\mobile_data (1).csv")
head(mydata)
tail(mydata)
View(mydata)
dim(mydata)
X=mydata$Pixel.Density
Y=mydata$Rating
print(X[[1]][1])
print(dim(X)[1])
m<-c(X[1])
m
for (i in 1:dim(X)[1]){
  
}
model1<-lm(Y~X)
coef(model1)[1]

