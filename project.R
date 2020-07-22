data<-read.csv(choose.files(),header=T) #uploading the data
data1<-data[data[,1]<=13 & data[,6]<=70,]
boxplot(data1[1:6])
sum(data1[,7])
cor(data1, method = c("pearson"))
pairs(data1[,1:7] ,cex.labels = 2)
car::vif(mylogit)

mylogit <- glm(Outcome ~ pre + glu + bp + bmi + dpf + age, data = data1, family = "binomial")
summary(mylogit)

chiSt<-mylogit$null.deviance-mylogit$deviance
df<-mylogit$df.null-mylogit$df.residual
ifelse(chiSt>qchisq(0.95,df), print("Reject H0"), print("Accept H0"))
finalModel<-step(mylogit, direction = "backward", k=log(nrow(data1)))
BIC(mylogit)
BIC(finalModel)
summary(finalModel)

plot(residuals(mylogit), fitted(mylogit))

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#-----Transformations------
data2<-data1

mylogit2 <- glm(Outcome ~ sqrt(pre) + glu + bp + bmi + log(dpf) + log(age), data = data1, family = "binomial")
summary(mylogit2)

chiSt<-mylogit2$null.deviance-mylogit2$deviance
df<-mylogit2$df.null-mylogit2$df.residual
ifelse(chiSt>qchisq(0.95,df), print("Reject H0"), print("Accept H0"))
finalModel2<-step(mylogit2, direction = "backward", k=log(nrow(data2)))
BIC(mylogit2)
BIC(finalModel2)
summary(finalModel2)
chiSt<-finalModel2$null.deviance-finalModel2$deviance
df<-finalModel2$df.null-finalModel2$df.residual
ifelse(chiSt>qchisq(0.95,df), print("Reject H0"), print("Accept H0"))


#----Train,Set,k-Folds-----
trainIndex<-createDataPartition(data1$Outcome, p=0.8)$Resample1
trainSet<-data1[trainIndex,-7]
trainLabels<-factor(data1[trainIndex,7])
testSet<-data1[-trainIndex, -7 ]
testLabels<-factor(data1[-trainIndex,7])
set.seed(24)
flds<-createFolds(y=trainLabels,k=10,list = TRUE, returnTrain = FALSE)

#-----Logistic Regression Test--------
TrainSetReg1<-trainSet[,-c(3,6)]
TrainSetReg1[,5]<-trainLabels
beta<-matrix(nrow=10, ncol = 5)
for (k in c(1:10)) {
  dat<-TrainSetReg1[-flds[[k]],]
  model<-glm(V5~., family = 'binomial', data = dat)
  beta[k,]<-model$coefficients
}
betaHat<-colMeans(beta)

finalModel$coefficients<-betaHat

probabilities<-predict(finalModel, testSet, type="response")
predicted.classes <-factor(ifelse(probabilities > 0.5, 1, 0))
accuracy<-sum(testLabels==predicted.classes)/nrow(testSet)
confusionMatrix(predicted.classes, testLabels)



TrainSetReg2<-trainSet[,-c(1,3)]
TrainSetReg2[,5]<-trainLabels
TrainSetReg2[,3]<-log(TrainSetReg2[,3])
TrainSetReg2[,4]<-log(TrainSetReg2[,4])
beta<-matrix(nrow=10, ncol = 5)
for (k in c(1:10)) {
  dat<-TrainSetReg2[-flds[[k]],]
  model<-glm(V5~., family = 'binomial', data = dat)
  beta[k,]<-model$coefficients
}
betaHat<-colMeans(beta)

finalModel2$coefficients<-betaHat

probabilities<-predict(finalModel2, testSet, type="response")
predicted.classes <- factor(ifelse(probabilities > 0.5, 1, 0))
accuracy<-sum(testLabels==predicted.classes)/nrow(testSet)
confusionMatrix(predicted.classes, testLabels)


#-----Trees----
set.seed(24)
trees<-c(100,200,300,400,500,600,700,800,900,1000)
results<-rep(NA,length(trees))
for (i in c(1:length(trees))) {
  scores=0
  for (k in c(1:10)) {
    train<-trainSet[-flds[[k]],]
    response<-factor(trainLabels[-flds[[k]]])
    rf<-randomForest(x=train[,1:6], y=response, ntree = trees[i])
    predicted.classes<-predict(svc, trainSet[flds[[k]],])
    scores=scores+sum(trainLabels[flds[[k]]]==predicted.classes)/nrow(trainSet[flds[[k]],])
  }
  results[i]<-scores/10
}

plot(trees,results, "l")

tree<-trees[which.max(results)]
rf <- randomForest(x=trainSet,y=trainLabels, ntree = tree)
predicted.classes<-predict(rf, testSet[,1:6])
accuracy<-sum(testLabels==predicted.classes)/nrow(testSet)
confusionMatrix(predicted.classes, testLabels)

#-----KNN-----
K<-seq(1,50,1)
results<-rep(NA,50)
for (k in K) {
  scores=0
  for (i in c(1:10)) {
    train<-trainSet[-flds[[i]],]
    response<-factor(trainLabels[-flds[[i]]])
    predicted.classes<-knn(train = train, test =  trainSet[flds[[i]],], cl=response, k=k)
    scores=scores+sum(trainLabels[flds[[i]]]==predicted.classes)/nrow(trainSet[flds[[i]],])
  }
  results[k]<-scores/10
}
k=1
i=1

plot(K, results, "l" ,ylab = 'accuracy')
k=which.max(results)

predicted.classes<-knn(train = trainSet, test = testSet, cl=trainLabels, k=k)
accuracy<-sum(testLabels==predicted.classes)/nrow(testSet)
confusionMatrix(predicted.classes, testLabels)
14/(14+81)

#------Linear SVM------
C<-c(0.001,0.01,0.1,1,10,100)
results<-rep(NA,length(C))

for (i in c(1:length(C))) {
  scores=0
  for (k in c(1:10)) {
    train<-trainSet[-flds[[k]],]
    response<-factor(trainLabels[-flds[[k]]])
    svc<-svm(x=train, y=response, cost = C[i], kernel = 'linear')
    predicted.classes<-predict(svc, trainSet[flds[[k]],])
    scores=scores+sum(trainLabels[flds[[k]]]==predicted.classes)/nrow(trainSet[flds[[k]],])
  }
  results[i]<-scores/10
}
plot(C ,results, "l", ylab = 'accuracy')

c<-C[which.max(results)]
lsvc<-svm(x=trainSet, y=trainLabels, cost = c, kernel = 'linear')
predicted.classes<-predict(lsvc,testSet)
accuracy<-(sum(testLabels==predicted.classes)/nrow(testSet))
confusionMatrix(predicted.classes, testLabels)
1-0.8737

#------Radial SVM------

C<-c(0.001,0.01,0.1,1,10,100)
results<-rep(NA,length(C))

for (i in c(1:length(C))) {
  scores=0
  for (k in c(1:10)) {
    train<-trainSet[-flds[[k]],]
    response<-factor(trainLabels[-flds[[k]]])
    svc<-svm(x=train, y=response, cost = C[i], kernel = 'radial')
    predicted.classes<-predict(svc, trainSet[flds[[k]],])
    scores=scores+sum(trainLabels[flds[[k]]]==predicted.classes)/nrow(trainSet[flds[[k]],])
  }
  results[i]<-scores/10
}
plot(C ,results, "l", ylab = 'accuracy')
c<-C[which.max(results)]
svc<-svm(x=trainSet, y=trainLabels, cost = c, kernel = 'radial')
predicted.classes<-predict(svc,testSet)
accuracy<-(sum(testLabels==predicted.classes)/nrow(testSet))
confusionMatrix(predicted.classes, testLabels)




#------Naive Bayes------

nb<-naiveBayes(x=trainSet, y=trainLabels)
predicted.classes<-predict(nb,testSet)
(accuracy<-(sum(testLabels==predicted.classes)/nrow(testSet)))
confusionMatrix(predicted.classes, testLabels)
1-0.8105
f1(24/(24+18),0.5)

Titanicggplot(mydata1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


probabilities <- predict(mylogit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
sum(data1[,7]==predicted.classes)/719

mydata1 <- data2[,1:6]
predictors <- colnames(mydata1)

mydata1 <- mydata1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)





numLlvs <- 4
confusionMatrix(
  factor(sample(rep(letters[1:numLlvs], 200), 50)),
  factor(sample(rep(letters[1:numLlvs], 200), 50)))  


f1<-function(ppv,tpr){
  return (2*ppv*tpr/(ppv+tpr))
}


f1(19/29,0.3958)
f1(22/34,0.4583)
f1(21/35,0.4375)
f1(31/44,0.6078)
