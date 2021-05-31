
### kfold CV

```{r}

######Cross Validation########
#install.packages("cvTools") #crossvalidation
library(cvTools) #crossvalidation

#ten fold cross validation
accuracy=numeric(10)
folds <- cvFolds(nrow(data), K=10)
for(i in 1:10) {
  trainData <- data[folds$subsets[folds$which != i], ]
  testData <- data[folds$subsets[folds$which == i], ]
  #training the KNN model
  output=knn(trainData[,-1],testData[,-1],as.factor(trainData[,1]),k=3)
  accuracy[i]=sum(testData[,1]==output)/nrow(testData)
}
mean(accuracy)
sd(accuracy)

```

