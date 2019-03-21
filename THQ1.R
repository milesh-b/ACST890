#question 1
#*************************

#Github username:milesh-b
#Repository:ACST890
#File name:THQ1.R


#this code must be run in interaction mode
#you will be prompted to input the n interest rates and press the return key on each entry eg 0.06 etc
price<-function(c,f,n){
  int<-0
for(i in 1:n){
    int[i]<-readline(prompt = "input the i rate effective half yearly and press the return key on each input,  " )
  }
  int<-as.numeric(int)
  coupon<-0
  for(k in 1:n){
    coupon<-coupon+(c*exp(-int[k]*k))
  }
  face<-f*exp(-int[n]*n)
bond<-coupon+face
cat("The price of the bond is: ",bond,"\n","inputted i rates are: ",int)  
}

#question 3
#****************************

#Github username:milesh-b
#Repository:ACST890
#File name:THQ1.R


#3a
dataset <- read.csv("~/Desktop/singapore.economy.csv", stringsAsFactors=FALSE)

#b
dataset<-na.omit(dataset)

#c
plot(y = dataset$gdp, x =dataset$time,xlab = "Time",ylab = "GDP (%)", main = "Singapore GDP growth")

#d
mean_1<-mean(dataset$gdp[dataset$period==1])
sd_1<-sd(dataset$gdp[dataset$period==1])
mean_2<-mean(dataset$gdp[dataset$period==2])
sd_2<-sd(dataset$gdp[dataset$period==2])
mean_3<-mean(dataset$gdp[dataset$period==3])
sd_3<-sd(dataset$gdp[dataset$period==3])
stat.table<-data.frame(mean=c(mean_1,mean_2,mean_3), sd=c(sd_1,sd_2,sd_3),row.names = c(1,2,3))
stat.table
#e
pairs(~ gdp+exp+epg+crd+hpr+gdpus+oil+bci,data=dataset)

#f
linear<-lm(gdp~exp,data = dataset)
summary(linear)
#The p-value for the exp component is quite significant in the regression analysis.
#The adjusted R^2 is quite low (0.2813) which indicates that the model fit is not good.

#g
multiple_linear<-lm(gdp~exp+epg+hpr+oil+gdpus+crd,data=dataset)
summary(multiple_linear)
#The p-values for the exp,epg and hpr components are quite significant in the regression analysis
#The adjusted R^2 is quite low (0.3354) which indicates that the model fit is not good, 
#however it is better fitting than the simple linear regression.

#h

#calculating quantile
quantile<-quantile(dataset$gdp,probs = 0.05)
quantile

#creating a factor vector "state"
state<-rep("normal",nrow(dataset))
state[dataset$gdp<quantile]<-"crisis"

#combining the "state" vector to the original dataset
dataset<-data.frame(dataset,state)

#splitting data into train and test
train<-dataset[(dataset$time)<2008,]
test<-dataset[(dataset$time)>=2008,]
#applying the logistic regression and using it to compute the confusion matrix
logistic<-glm(state~bci,data=train, family=binomial)
summary(logistic)
logistic_predict<-predict(logistic,test,type="response")
logistic.pred<-rep("crisis",nrow(test))
logistic.pred[logistic_predict>0.5]<-"normal"
confusion_matrix<-table(test$state,logistic.pred) #confusion matrix
confusion_matrix
 
