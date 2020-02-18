train<-read.csv(file = "https://raw.githubusercontent.com/jesford/bike-sharing/master/train.csv",stringsAsFactors = T)
test<-read.csv(file = "https://raw.githubusercontent.com/jesford/bike-sharing/master/test.csv")


write.csv(train,file="train.csv",row.names=FALSE)
write.csv(test,file="test.csv",row.names=FALSE)

colnames(train)
test$casual<-0
test$registered<-0
test$count<-0
data<-rbind(train,test)
summary(data)
head(data)
str(data)
table(is.na(data))

par(mfrow=c(4,2))
par(mar=rep(2,4))

hist(data$season)
hist(data$weather)
hist(data$temp)
hist(data$atemp)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$windspeed)


prop.table(table(data$weather))
prop.table(table(data$holiday))
prop.table(table(data$workingday))
prop.table(table(data$season))

data$season<-as.factor(data$season)
data$holiday<-as.factor(data$holiday)
data$weather<-as.factor(data$weather)
data$workingday<-as.factor(data$workingday)

str(data)

data$hour<-substr(data$datetime,12,13)
data$hour<-as.factor(data$hour)

prop.table(table(data$hour))

train<-data[as.integer(substr(data$datetime,9,10))<20,]
test<-data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count~train$hour)
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")

boxplot(train$casual~train$hour,xlab="hour", ylab="count of casual user")
boxplot(train$registered~train$hour,xlab="hour", ylab="count of register user")

boxplot(log(train$count)~train$hour,xlab="hour",ylab="log(count)")

date<-substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day<-days


#plot the register,count and casual user with respect to days

boxplot(data$registered~data$day,xlab="Days", ylab="count of register user")
boxplot(data$casual~data$day,xlab="Days", ylab="count of casual user")
boxplot(data$count~data$day,xlab="Days", ylab="count of count user")

#plot the register,count and casual user with respect to weather

boxplot(data$registered~data$weather,xlab="weather", ylab="count of register user")
boxplot(data$casual~data$weather,xlab="weather", ylab="count of casual user")
boxplot(data$count~data$weather,xlab="weather", ylab="count of count user")


sub=data.frame(train$registered,train$casual,train$count,
               train$temp,train$humidity,train$atemp,train$windspeed)
cor(sub)

data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")
boxplot(train$registered~train$year,xlab="year", ylab="registered")
boxplot(train$casual~train$year,xlab="year", ylab="casual")


train$hour=as.integer(train$hour) # convert hour to integer
test$hour=as.integer(test$hour) # modifying in both train and test data set


library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)
d=rpart(registered~hour,data=train)
fancyRpartPlot(d)
prp(d)

data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

dcasual=rpart(casual~hour,data=train)
fancyRpartPlot(dcasual)
prp(dcasual)

data$dp_cas=0
data$dp_cas[data$hour<=8]=1
data$dp_cas[data$hour==9]=2
data$dp_cas[data$hour >=10 & data$hour<=19]=3
data$dp_cas[data$hour>19]=4

dtemp=rpart(casual~temp,data=train)
fancyRpartPlot(dtemp)
prp(dtemp)

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp_cas>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4

dreg=rpart(registered~temp,data=train)
fancyRpartPlot(dreg)
prp(dreg)

data$temp_reg =0
data$temp_reg [data$temp<13]=1
data$temp_reg [data$temp>=13 & data$temp <23]=2
data$temp_reg [data$temp_cas>=23 & data$temp<30]=3
data$temp_reg [data$temp>=30]=4


data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1


#model building

#Convert discrete variables into factor (weather, season, hour, holiday, working day, month, day)

data$weather<-as.factor(data$weather)
data$season<-as.factor(data$season)
data$hour<-as.factor(data$hour)
data$holiday<-as.factor(data$holiday)
data$workingday<-as.factor(data$workingday)
data$day<-as.factor(data$day)
data$year<-as.factor(data$year)
data$dp_reg<-as.factor(data$dp_reg)
data$dp_cas<-as.factor(data$dp_cas)
data$year_part<-as.factor(data$year_part)
data$day_type<-as.factor(data$day_type)
data$weekend<-as.factor(data$weekend)
data$temp_cas<-as.factor(data$temp_cas)
data$temp_reg<-as.factor(data$temp_reg)

#y1=log(casual+1) and y2=log(registered+1)
data$logreg<-log(data$registered +1)
data$logcas<-log(data$casual + 1)

str(data)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
library(randomForest)
colnames(train)
fit1<-randomForest(logreg ~ hour+workingday+day+holiday+ day_type +temp_reg+
                     humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,
                   data=train,importance=TRUE,ntree=250)

summary(fit1)
pred1<-predict(fit1,test)
test$logreg<-pred1

fit2<-randomForest(logcas ~ hour+workingday+day+holiday+ day_type +temp_reg+
                     humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,
                   data=train,importance=TRUE,ntree=250)

summary(fit2)
pred2<-predict(fit2,test)
test$logcas<-pred2


test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered
s<-data.frame(datetime=test$datetime,count=test$count)
write.csv(s,file="submit.csv",row.names=FALSE)


#linear regression
model1<-lm(logreg~hour+workingday+day+holiday+ day_type +temp_reg+
             humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,
           data=train)
summary(model1)
str(train)
step(model1)
