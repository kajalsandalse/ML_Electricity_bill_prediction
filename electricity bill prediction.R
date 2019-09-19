mydata<-read.csv(file.choose())
View(mydata)
head<-head(mydata)
head
summary(head)
names(mydata)


#Data Visualization & Analysis 
Reactive_power<-head(mydata$Global_reactive_power)
Reactive_power
sub_meter_reading<-head(mydata$sub_metering)
sub_meter_reading
dotchart(Reactive_power,sub_meter_reading,main = "avg_reactive_power VS submeter_reading",xlab = "reactive power in KW",ylab = "sub_metering_")

globalintensity<-head(mydata$Global_intensity)
globalintensity
plot(globalintensity,sub_meter_reading,main = "global_intensity VS Submeter_reading",col=c(1,2,3,4,5))


# to replace 0 valus
r=max(mydata$Global_reactive_power)
r

#replacing 0 values
mydata$Global_reactive_power=ifelse(mydata$Global_reactive_power==0,r,mydata$Global_reactive_power)
tail(mydata)
mydata

cor(mydata$Global_active_power,mydata$sub_metering)
cor(mydata$Global_reactive_power,mydata$sub_metering)
cor(mydata$Voltage,mydata$sub_metering)
cor(mydata$Global_intensity,mydata$sub_metering)

summary(mydata)
names(mydata)


#TRAINING & testing

library(caTools)
split<-sample.split(mydata,SplitRatio = 0.9)

training<-subset(mydata,split==TRUE)
testing<-subset(mydata,split==FALSE)
head(testing)
head(training)
summary(training)
summary(testing)

# input & output variables

Input<-mydata[c("Global_reactive_power","Voltage","Global_intensity")]
Output<-mydata$sub_metering

#building a model
regresser<-lm(sub_metering ~Global_reactive_power+Voltage+Global_intensity,data = mydata)
new<-data.frame("Global_reactive_power","Voltage","Global_intensity")
summary(regresser)

# Sample predicting resultes

Global_reactive_power<-0.528
Voltage<-235.68
Global_intensity<-18.4

predict(regresser,new)
a=predict(regresser,mydata)

#Accuracy

p=mean(abs(a-mydata$sub_metering))
p







