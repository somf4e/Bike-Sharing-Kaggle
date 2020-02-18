library(neuralnet)
#install.packages("FNN")
library(randomForest)
library(lubridate)
library(e1071)
library(FNN)

evaluation<-function(prediction,actual){
  n<-length(prediction)
  res<-sqrt((1/n)*sum((log(prediction+1) - log(actual+1))^2))
  return(res)
}
loadData<-function(filePath){
  data<- read.csv(filePath)
  data.datetime2<-strptime(data$datetime, format="%Y-%m-%d %H:%M:%S")
  data$weekday<-weekdays(data.datetime2)
  data$year<-year(data.datetime2)
  data$month<-month(data.datetime2)
  data$day<-day(data.datetime2)
  data$hour<-hour(data.datetime2)
  data$month2 <- 12*(data$year-2011) + data$month
  
  
  return(data)
}

asFactor<-function(data){
  # make factor -- Turn numerical data in categorical.
  data$weather[data$weather == 4] <-3;
  data$season <- as.factor(data$season) 
  data$workingday <- as.factor(data$workingday) 
  data$weather <- as.factor(data$weather)
  data$year <- as.factor(data$year) 
  data$day <- as.factor(data$day)
  data$hour <- as.factor(data$hour)
  data$weekday<-as.factor(data$weekday)
  return(data)
}

NeuralNet<-function(train,validation,h=c(15,16,17,16,15),t=.04){ 
  
  
  #create matrix
  trainmat <- model.matrix(count~season+workingday+weather+year+hour+weekday,data=train)
  testmat <- model.matrix(~season+workingday+weather+year+hour+weekday,data=validation)
  
  trainmat <- as.data.frame(trainmat)
  testmat <- as.data.frame(testmat)
  
  trainmat<-trainmat[names(trainmat) %in% names(testmat)]
  #scale count
  scale <- 10000
  count <- train$count/scale
  
  #add count to trainmat
  trainmat <- cbind(trainmat,count)
  
  #Write formula
  formula <- count ~ season2+season3+season4+workingday1+weather2+weather3+year2012+hour1+hour2+hour3+hour4+hour5+hour6+hour7+hour8+hour9+hour10+hour11+hour12+hour13+hour14+hour15+hour16+hour17+hour18+hour19+hour20+hour21+hour22+hour23+weekdayMonday+weekdaySaturday+weekdaySunday+weekdayThursday+weekdayTuesday+weekdayWednesday
  
  #train your data.  note that this is a neural network with 5 hidden layers of 7, 8, 9, 8, and 7 respectively.
  
  fit <- neuralnet(formula,data=trainmat,hidden=h,threshold=t,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T)
  
  #remove intecerpt from test matrix and predict
  predict <- compute(fit,testmat[,2:37])
  
  #Assign predictions to variable because compute produces more than we need
  
  predict2<- predict$net.result*scale
  predict2[predict2 < 3] <- 3; #if the number is less than 3 just guess 3
  result<-data.frame("datetime" = as.character(validation$datetime),"count" = rep(0,nrow(testmat)),stringsAsFactors = F)
  result$count<-predict2
  
  return(result)
  
}

otherModels<-function(train,validation,md="pcr"){
  features <- c(
    "workingday",
    "weather",
    "atemp",
    "humidity",
    "windspeed",
    "hour","weekday") 
  
  trainRF<-as.data.frame(train)
  trainRF<-trainRF[,features]
  
  validationRF<-as.data.frame(validation)
  validationRF<-validationRF[,features]
  trainRF$count <- train$count
  
  result<-data.frame("datetime" = as.character(validation$datetime),"count" = rep(0,nrow(validationRF)),stringsAsFactors = F)
  if(md=="linear"){
    linear<-lm(count~.,data=trainRF)
    result$count<- predict(linear, validationRF)
  }
  
  else if(md=="pcr"){
    pcr.fit<-pcr(count~.,data = trainRF,scale=TRUE,validation="CV")
    result$count<-predict(pcr.fit,validationRF)
  }
  
  else if(md=="svm"){
    svm.fit<-svm(count~.,data = trainRF,kernel="linear")
    result$count<-predict(svm.fit,validationRF)
  }
  
  else{ #knn
    trainRF$weekday<-NULL
    validationRF$weekday<-NULL
    trainRF$count <- NULL
    
    m.knn = knn.reg(trainRF,validationRF,train$count,k=5,algorithm=c("kd_tree", "cover_tree", "brute"))
    
    #get result
    result$count <- m.knn$pred
  }
  
  result$count[result$count < 0] <- 0
  return (result)
  
}

runRF<-function(train,validation,byHour=F,qty=100,features = c("season", "holiday","workingday","weather","atemp","humidity","windspeed","hour","weekday","month","day")){
  set.seed(89)
  
  trainRF<-as.data.frame(train)
  trainRF<-trainRF[,features]
  #trainRF$weekday <- as.factor(trainRF$weekday)
  validationRF<-as.data.frame(validation)
  validationRF<-validationRF[,features]
  #validationRF$weekday <- as.factor(validationRF$weekday)
  
  trainRF$registered<-train$registered
  result<-data.frame("datetime" = as.character(validation$datetime),"count" = rep(0,nrow(validationRF)))
  if (byHour){
    for (j in 0:23){
      rfReg <- randomForest(registered~.,data=trainRF[train$hour == j,],importance=TRUE,ntree=qty)
      pReg<- predict(rfReg, validationRF[validation$hour ==j,])
      result[validation$hour ==j,]$count<-pReg
    }
    trainRF$registered<-NULL
    trainRF$casual<-train$casual
    for (j in 0:23){
      rfReg <- randomForest(casual~.,data=trainRF[train$hour == j,],importance=TRUE,ntree=qty)
      pReg<- predict(rfReg, validationRF[validation$hour ==j,])
      result[validation$hour ==j,]$count<-result[validation$hour ==j,]$count+pReg
    }
  }
  else{
    rfReg <- randomForest(registered~.,data=trainRF,importance=TRUE,ntree=qty)
    pReg<- predict(rfReg, validationRF)
    trainRF$registered<-NULL
    trainRF$casual<-train$casual
    rfCasual <- randomForest(casual~.,data=trainRF,importance=TRUE,ntree=qty)
    pCasual<- predict(rfCasual, validationRF)
    result$count<-pCasual+pReg
  }
  
  
  
  return(result)
}

##Separating sets
run<-function(train,validation,model="RF",hidden=c(15,16,17,16,15),threshold=.001,ntree=100,submit=F){
  
  f<-function(i){return(subset(train,month2<=i))}#Only gets data from the past
  t<-function(i){return(subset(validation,month2==i))}#predict the current month
  
  if(model == "RF")
  {
    validationSet<-sapply(1:24,t)
    trainSet<-sapply(1:24,f)
    p<-function(i){return(runRF(trainSet[,i],validationSet[,i],byHour=(i>3), qty=ntree))}
  }
  else if (model == "NeuralNet"){
    train<-asFactor(train)
    validation<-asFactor(validation)
    validationSet<-sapply(1:24,t)
    trainSet<-sapply(1:24,f)
    p<-function(i){return(NeuralNet(trainSet[,i],validationSet[,i],h=hidden,t=threshold))}
  }
  else if (model == "comb")
  {
    train2<-asFactor(train)
    validation2<-asFactor(validation)
    f2<-function(i){return(subset(train2,month2<=i))}
    t2<-function(i){return(subset(validation2,month2==i))}
    validationSet2<-sapply(1:24,t2)
    trainSet2<-sapply(1:24,f2)
    
    validationSet<-sapply(1:24,t)
    trainSet<-sapply(1:24,f)
    p<-function(i){
      if (i<5){
        return(NeuralNet(trainSet2[,i],validationSet2[,i],h=hidden,t=threshold))
      }
      else{
        return(runRF(trainSet[,i],validationSet[,i],byHour= T, qty=ntree))
      }
      
    }
    
    
  }
  else if (model == "comb2")
  {
    validationSet<-sapply(1:24,t)
    trainSet<-sapply(1:24,f)
    p<-function(i){
      if (i<5){
        return(runRF(trainSet[,i],validationSet[,i],byHour= T, qty=ntree,features=c("holiday","workingday","weather","atemp","humidity","windspeed","weekday","month","day")))
      }
      else{
        return(runRF(trainSet[,i],validationSet[,i],byHour= T, qty=ntree))
      }
      
    }
    
    
  }
  else{
    validationSet<-sapply(1:24,t)
    trainSet<-sapply(1:24,f)
    
    p<-function(i){return(otherModels(trainSet[,i],validationSet[,i],md= model))}
    pred1<-sapply(1:4,p)
    pred2<-sapply(5:24,p)
  }
  
  
  k<-24
  pred<-sapply(1:k,p)
  if (submit){
    #datestr<-paste(validation$year,"-",validation$month,"-",validation$day," ",validation$hour,":00:00",sep="")
    result<-data.frame(row.names=F)
    for (i in 1:k){
      result<-rbind(result,as.data.frame(pred[,i],stringsAsFactors = F))
    }
    write.csv(result,paste(model,".csv",sep = ""),row.names = F)
    return(result)
  }
  else{
    err<- rep(0,k)
    for (i in 1:k){
      v<-validationSet[,i]
      predict<-as.data.frame(pred[,i],stringsAsFactors = F)
      err[i] <- evaluation(predict$count,v$count)
    }
    results<-list("predictions" = pred,"error" = err, "MeanError" = mean(err[!is.na(err)]), "hidden" = hidden, "threshold" = threshold)
    return(results)
  }
  
}

if (exists("train") == F)
  train<-loadData("train.csv")

if (exists("validation") == F)
  validation<-loadData("validation.csv")

if (exists("test") == F)
  test<-loadData("test2.csv")

if (exists("trainI") == F)
  trainI<-loadData("train2_bruno.csv")

if (exists("train2") == F)
  train2<-loadData("train.csv")


# if (exists("predLinear") == F)
#    #MeanError: 1.136874215
#   predLinear<-run(train,validation,"linear",submit = F)
# 
# if (exists("predLinearS") == F)
#   #Kaggle result: scored 1.16255 - 10comps
#   predLinearS<-run(train2,test,"linear",submit = T)
# 
# if (exists("predPCR") == F)
#   #MeanError = 1.136319435
#   predPCR<-run(train,validation,"pcr",submit = F)
# 
# if (exists("predPCRS") == F)
#   #Kaggle Result: 1.15471
#   predPCRS<-run(train2,test,"pcr",submit = T)
# 
# if (exists("predSVM") == F)
#   #MeanError = 1.10
#   predSVM<-run(train,validation,"svm",submit = F)
# 
# if (exists("predSVMS") == F)
#   #Kaggle result
#   predSVMS<-run(train2,test,"svm",submit = T)

# if (exists("predNN") == F)
#   #MeanError = 0.5096917474
#   predNN<-run(train,validation,threshold = 0.001,model = "NeuralNet",submit = F)

# if (exists("predNNs") == F)
#   #Kaggle result = 0.56911
#   predNNs<-run(train2,validation,threshold = 0.001,model = "NeuralNet",submit = T)

# if (exists("predRF") == F)
#   #MeanError = 1.10, qty = 100
#   predRF<-run(train,validation,"RF",ntree=1000,byHour=T,submit = F)
# 
# if (exists("predRFS") == F)
#   #Kaggle result, qty = 100
#   predRFS<-run(train2,test,model ="RF",ntree=1000,submit = T)

# if (exists("predK") == F)
#   #MeanError = 0.8998028225
#   predK<-run(train,validation,"knn",submit = F)
# 
# if (exists("predKS2") == F)
#   #   #Kaggle result = 0.88074
#   predKS2<-run(trainI,test,model="knn2",submit = T)


# if (exists("predComb") == F)
#   #MeanError = 
#   predComb<-run(train,validation,threshold = 0.0001,model = "comb",ntree=1500,submit = F)

# if (exists("predCombS") == F)
#   #Kaggle result: 0.53367
#   predCombS<-run(train2,test,threshold = 0.0001,model = "comb",ntree=1500,submit = T)

# if (exists("predCombS") == F)
#   #Kaggle result: 0.53732 
#   predCombS<-run(trainI,test,hidden=c(7,8,9,8,7),threshold=.04,model = "comb",ntree=1500,submit = T)

###


