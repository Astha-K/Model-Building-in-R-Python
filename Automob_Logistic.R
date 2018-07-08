library(ggplot2)
library(gridExtra)
#Auto <- read.csv(file.choose()) 
Auto=read.csv("C:/R/R_Files/Automobile.csv")
View(Auto)
names(Auto)
str(Auto)
sapply(Auto,function(x) sum(is.na(x))) 
# No missing values shown as the missing-values are in the form of '?' -Question mark.
#####Replacing Special Character(?) with NA
Auto$num.of.doors=as.factor(Auto$num.of.doors)
Auto$symboling=as.factor(Auto$symboling)
Auto$normalized.losses=as.numeric(gsub("[\\?,]","",Auto$normalized.losses))
Auto$num.of.doors=gsub("[\\?,]","",Auto$num.of.doors)
Auto$bore=as.numeric(gsub("[\\?,]","",Auto$bore))
Auto$stroke=as.numeric(gsub("[\\?,]","",Auto$stroke))
Auto$horsepower=as.numeric(gsub("[\\?,]","",Auto$horsepower))
Auto$peak.rpm=as.numeric(gsub("[\\?,]","",Auto$peak.rpm))
Auto$price=as.numeric(gsub("[\\?,]","",Auto$price))
View(Auto)
str(Auto)
#####Sum of Missing Values
sapply(Auto,function(x) sum(is.na(x)))
###OR
sapply(Auto, function(x) sum(length(which(is.na(x)))))  
###Replace NA with Mean
#Replacing missing values with Mean and Mode depends on condition whether it is Numeric or Categorical respectively.
for(i in 2:ncol(Auto)){
  if (is.numeric(Auto[,i]))
  {
    #print('Numeric')
    Auto[is.na(Auto[,i]), i] <- mean(Auto[,i], na.rm = TRUE)
  }
  else
  {
    #print('Catogorical')
    Auto[is.na(Auto[,i]), i] <- mode(Auto[,i])
  }
}
sapply(Auto,function(x) sum(is.na(x)))
##Making a subset of all the numerical values in the dataset
Automob<-(Auto[c("normalized.losses","wheel.base","length","width","height","curb.weight",
                 "engine.size","bore","stroke","compression.ratio","horsepower","peak.rpm",
                 "city.mpg","highway.mpg","price")])
Automob
#Data Normalization - HISTOGRAM
library(rcompanion)
T_log = log(Automob)
plotNormalHistogram(T_log)
###Replace NA in price variable BY MEDIAN
# Auto$normalized.losses[is.na(Auto$normalized.losses)]=median(Auto$normalized.losses[!is.na(Auto$normalized.losses)])
# Auto$bore[is.na(Auto$bore)]=median(Auto$bore[!is.na(Auto$bore)])
# Auto$stroke[is.na(Auto$stroke)]=median(Auto$stroke[!is.na(Auto$stroke)])
# Auto$horsepower[is.na(Auto$horsepower)]=median(Auto$horsepower[!is.na(Auto$horsepower)])
# Auto$peak.rpm[is.na(Auto$peak.rpm)]=median(Auto$peak.rpm[!is.na(Auto$peak.rpm)])
# Auto$price[is.na(Auto$price)]=median(Auto$price[!is.na(Auto$price)])
# Auto$num.of.doors[is.na(Auto$num.of.doors)]=mode(Auto$num.of.doors)
# sapply(Auto,function(x) sum(is.na(x)))
# Automob<-(Auto[c("normalized.losses","wheel.base","length","width","height","curb.weight",
#                  "engine.size","bore","stroke","compression.ratio","horsepower","peak.rpm",
#                  "city.mpg","highway.mpg","price")])
# Automob
# Data was more Normalized when replaced by Mean instead of Median.
# library(rcompanion)
# T_log = log(Automob)
# plotNormalHistogram(Automob)
###Boxplot For Detecting OUTLIERS
boxplot(Automob)
summary(Automob)
###Treating OUTLIERS
### converting outlier to NA (MISSING VALUE) then replacing it with mean
A=22000
Auto$price[Auto$price>A]<-NA
sapply(Auto,function(x) sum(is.na(x)))
#replacing with MEAN
Auto$price[is.na(Auto$price)]=mean(Auto$price[!is.na(Auto$price)])
#
sapply(Auto,function(x) sum(is.na(x)))
boxplot(Auto$price)
###CORRELATION
cor(Automob)
###Logistic Regression Model
###y=symboling where 0-"less-risky"  & 1-"more-risky"
#Splitting the Dataset in 70:30 ratio
#2-ways
######1st way
# library(caret)
# Train<-createDataPartition(Auto$symboling,p=0.7,list=FALSE)  #data to be divided on row basis
# training<-Auto[Train,]
# testing<-Auto[-Train,]
######2nd way
library(caTools)
spl=sample.split(Auto$symboling,0.7)
training=subset(Auto,spl==TRUE)
testing=subset(Auto, spl==FALSE)
#GLM (Logistic Regression Model)
logit<-step(glm(symboling ~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+
                  bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg+price+
                  aspiration+make+fuel.type+body.style+engine.location+engine.type+num.of.cylinders+
                  fuel.system+num.of.doors+drive.wheels,data=Auto,family='binomial'))
summary(logit)
###Remaining variables after step function
###Model 
logit<-glm(symboling ~ wheel.base + length + width + engine.size + compression.ratio +horsepower +
           city.mpg + highway.mpg + aspiration +num.of.doors+drive.wheels,data=Auto,family='binomial')
###Summary of Model
summary(logit)
#Re-running the Model with (p-values<0.05) or variables with have impact on y
logit<-glm(symboling ~ wheel.base + width ,data=Auto,family='binomial')
###Summary of Model
summary(logit)
###vif
library(car)
vif(logit)
###Accuracy of model 
###Concordance/Discordance/Tied-Pairs
###PREDICTION
testing.probs<-predict(logit,testing,type='response')
pred.logit<-rep(0,length(testing.probs))
pred.logit[testing.probs>=0.5]<-1
###Confusion Matrix
table(pred.logit,testing$symboling)
library(InformationValue)
confusionMatrix(testing$symboling,pred.logit)
Accuracy=(21+33)/(21+33+1+7)
Accuracy #0.8709677
###ROC CURVE
library(ROCR)
#make predictions on training set
predictTrain=predict(logit,testing,type='response')
#prediction Function
ROCRpred=prediction(predictTrain,testing$symboling)
#performance function
ROCRperf=performance(ROCRpred,'tpr','fpr')
#plot Roc Curve
plot(ROCRperf)
library(ROCR) #how much data are we explaining
pred=prediction(testing.probs,testing$symboling)
as.numeric(performance(pred,'auc')@y.values)
Accuracy=0.8445378
