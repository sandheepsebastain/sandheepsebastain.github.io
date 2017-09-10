#Reading in the data and data processing
dfpmltraining<-read.csv("pml-training.csv")
dfpmlvalidation<-read.csv("pml-testing.csv")

dfpmltraining[dfpmltraining==""]<-NA
dfpmltraining<-dfpmltraining[ , colSums(is.na(dfpmltraining)) == 0]

library(caret)
intrain <- createDataPartition(y = dfpmltraining$classe, p = 0.7, list = FALSE)

trainingdata<-dfpmltraining[intrain,]
testdata<-dfpmltraining[-intrain,]

#Starting to work on the training set
trainingdata<-trainingdata[,-c(1:2,5:7)]
idnearzerovar<-nearZeroVar(trainingdata,saveMetrics = TRUE)

xtraindata=trainingdata[,names(trainingdata) != "classe" & grepl( "^Total" , names( xtraindata ),ignore.case = TRUE ) ]


featurePlot(x=xtraindata,
            y=trainingdata$classe,
            plot="pairs", main="Training Data plot"
            )

#Doing 10 fold cross validation on the training set with 2 reptitions
control = trainControl(method="repeatedcv", number=5, repeats=2)

#Building a classification tree model
modelclass<-train(classe~., data=trainingdata, method="rpart", trControl=control)

#Building a random forest tree model
modelrf<-train(classe~., data=trainingdata, method="rf", trControl=control)

#Prediction on test data set
predictclass <- predict(modelclass,testdata)
predictrf <- predict(modelrf,testdata)
modelrfaccuracy<-confusionMatrix(predictrf, testdata$classe)$overall['Accuracy']
modelclassaccuracy<-confusionMatrix(predictclass, testdata$classe)$overall['Accuracy']

dfAccuracy<-data.frame(RandomForest=modelrfaccuracy,Classification=modelclassaccuracy,row.names = "Accuracy")

#Selecting the random forest model
modelrf$finalModel

#Out of Sample error
rfconfusionmatrix<-confusionMatrix(predictrf, testdata$classe)
rfconfusionmatrix$table

OutOfSampleError <- sum(predictrf != testdata$classe)/length(predictrf)



#Applying the model on the validation test set
validatepredict<-predict(modelrf,dfpmlvalidation)
