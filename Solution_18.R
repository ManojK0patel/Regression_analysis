x<-c(50.1, 74.1, 19.3,22.9,82.4,40.1,185.9,26.9,20.4,166.2,27,45.6,154.9,5,49.7,26.9,5.7,7.6,9.2,32.4,6.1)
#making a vector containing the Returned_Impression
y<-c(32.1, 99.6, 11.7,21.9,60.8,78.6,92.4,50.7,21.4,40.1,40.8,10.4,88.9,12,29.2,38,10,12.3,23.4,71.1,4.4)
firms <-c("Miller Lite","Pepsi","Stroh ","Federal Express","Burger King","Coca-Cola","McDonalds","MCI","DietCola","Ford","Levis","Bud Lite","ATT Bell","Calvin Klein","Wendys","Polaroid","Shasta","Meow Mix","Oscar Meyer","Crest","KibblesNBits"
)
#making dataframe out of given Amount_Spends and Returned_Impression
dfRV<-data.frame(Amount_Spend=x,Returned_Impression=y) 
rownames(dfRV) <- firms
dfRV
mean_x=mean(x)  #Computiong the mean of Amount_Spends
mean_y=mean(y)  #Computing the mean of Returned_Impression
#Adding a column in dataframe containg the values 'x_i-mean of x'
dfRV[["x-mean_x"]]<- x-mean_x
#Adding a column in dataframe containg the values 'y_i-mean of y'
dfRV[["y-mean_y"]]<- y-mean_y
dfRV
#calculating S_xx
S_xx<-0
for(i in 1:length(x)){
  S_xx=S_xx+dfRV[,"x-mean_x"][i]*dfRV[,"x-mean_x"][i]
}
S_xx

#calculating S_xy
S_xy<-0
for(i in 1:length(x)){
  S_xy=S_xy+dfRV[,"x-mean_x"][i]*dfRV[,"Returned_Impression"][i]
}
S_xy
beta_1<-S_xy/S_xx #beta_1_hat calculated 
beta_1
beta_0<-mean_y-beta_1*mean_x #beta_0_hat calculated 
beta_0
#Scatter plot between Amount_Spends and Viscosities
plot(x,y,main='scatter',xlab="Amount_Spend", ylab="Returned_Impression")
par(new=TRUE)  #To get the next plot in the same graph
#Plotting the fitted line for the given data.->beta_0+beta_1*x
plot(x,beta_0+beta_1*x[1:length(x)],type='l', xlab="Amount_Spend", ylab="Returned_Impression")
y_hat<-c(beta_0+beta_1*x[1:length(x)]) #Predicted values 
dfRV[["Y_Hat"]]<-y_hat
dfRV
dfRV[,'y-mean_y']-mean_y
SS_T<-0  #Calculating Sum Squared Total
for(i in dfRV[,'y-mean_y']){
  SS_T=SS_T+i*i
}
SS_T
SS_R<-0  #Calculating Sum Squared Regressor
for(i in y_hat-mean_y){
  SS_R=SS_R+i*i
}
SS_R
#Calculating sum squared residuals
SS_Res<-0
for (i in y-y_hat){
  SS_Res=SS_Res+i*i
}
SS_Res
#Calculating Mean Squared residuals
MS_Res<-SS_Res/6
MS_Res
#Calculating F_statistics
F_ststistics<-SS_R/MS_Res
F_ststistics
#Calculation R squared 
R_squared<-SS_R/SS_T
R_squared
#Calculating Standard Error of the slope
SE_Of_beta_1<-sqrt(MS_Res/S_xx)
SE_Of_beta_1
#Calculating t_0 for Slope
t_0_Slope<-beta_1/SE_Of_beta_1
t_0_Slope
#Calculating the standard error in the intercept
SE_Of_beta_0<-sqrt(MS_Res*(1/8+mean_x^2/S_xx))
SE_Of_beta_0
#Calculating t_0 for Intercept
t_0_Intercept<-beta_0/SE_Of_beta_0
t_0_Intercept
# value for the t distributio
qt(0.025,6,lower.tail = FALSE)
#Calculation the lower fit values for the given confidence interval 95%
lower_fit_values<-y_hat-qt(0.025,6,lower.tail = FALSE)*sqrt(MS_Res*(1/8+((x[1:length(x)]-mean_x)^2)/S_xx))
lower_fit_values
#Calculation the Upper fit values for the given confidence interval 95%
Upper_fit_values<-y_hat+qt(0.025,6,lower.tail = FALSE)*sqrt(MS_Res*(1/8+((x[1:length(x)]-mean_x)^2)/S_xx))
Upper_fit_values

#Calculation the lower fit values for prediction with 95% confidence 
lower_fit_values.prediction<-y_hat-qt(0.025,6,lower.tail = FALSE)*sqrt(MS_Res*(1+1/8+((x[1:length(x)]-mean_x)^2)/S_xx))
lower_fit_values.prediction
#Calculation the Upper fit values for prediction with 95% confidence 
Upper_fit_values.prediction<-y_hat+qt(0.025,6,lower.tail = FALSE)*sqrt(MS_Res*(1+1/8+((x[1:length(x)]-mean_x)^2)/S_xx))
Upper_fit_values.prediction
#Plotting
plot(type='o',x,lower_fit_values,col = "red",xlab="Amount_Spend", ylab="Returned_Impression")
par(new=TRUE)
plot(type='o',x,Upper_fit_values,col = "red",xlab="Amount_Spend", ylab="Returned_Impression")
par(new=TRUE)  #To get the next plot in the same graph
plot(type='o',x,lower_fit_values.prediction,col = 'green',xlab="Amount_Spend", ylab="Returned_Impression")
par(new=TRUE)
plot(type='o',x,Upper_fit_values.prediction,col = "green",xlab="Amount_Spend", ylab="Returned_Impression")
par(new=TRUE)
#Plotting the fitted line for the given data.->beta_0+beta_1*x
plot(x,y_hat,type='o', xlab="Amount_Spend", ylab="Returned_Impression",col='blue')
#Summary of the model.
Summary_dfRV<-data.frame(Beta_0=(beta_0),Beta_1=(beta_1),R_Squared=(R_squared),F_Statistic=(F_ststistics))
Summary_dfRV
model = lm(y~x)
summary(model)
plot(x,y,main='scatterplot')
predict(model,level=0.95,interval ="confidence" )
plot(model)
y_hat
