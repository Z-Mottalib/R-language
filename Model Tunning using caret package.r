library(mlbench)
data(Sonar)
str(Sonar[, 1:10])
dim(Sonar)



#The function createDataPartition can be used to create a stratified random sample of the data into training and test sets:
  ?createDataPartition

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
dim(training)
testing  <- Sonar[-inTraining,]
dim(testing)


#5.3 Basic Parameter Tuning

#By default, simple bootstrap resampling is used for line 3 in the algorithm above. 
#Others are available, such as repeated K-fold cross-validation, leave-one-out etc. 
#The function trainControl can be used to specifiy the type of resampling:
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

  ?trainControl

  set.seed(825)
  gbmFit1 <- train(Class ~ ., data = training, 
                   method = "gbm", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
  
  
  gbmFit1
  
  rfFit1 <- train(Class ~ ., data = training, 
                   method = "rf", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
  rfFit1
  
  #The tuning parameter grid can be specified by the user. 
  #The argument tuneGrid can take a data frame with columns for each tuning parameter. 
  #The column names should be the same as the fitting function???s arguments. 
  #For the previously mentioned RDA example, the names would be gamma and lambda. 
  #train will tune the model over each combination of values in the rows.
  
  #For the boosted tree model, we can fix the learning rate and evaluate more than three values of n.trees:
  
  gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                          n.trees = (1:30)*50, 
                          shrinkage = 0.1,
                          n.minobsinnode = 20)
  
  
  set.seed(825)
  gbmFit2 <- train(Class ~ ., data = training, 
                  method = "gbm", 
                  trControl = fitControl, 
                  verbose = FALSE, 
                  ## Now specify the exact models 
                  ## to evaluate:
                  tuneGrid = gbmGrid)
  
  
  
  
  rfGrid <-  expand.grid(mtry = c(seq(2,60,by=5),60))
  
  modelLookup("gbm")
  
set.seed(825)
 rfFit2 <- train(Class ~ ., data = training, 
                   method = "rf", 
                   trControl = fitControl, 
                   verbose = T, 
                   ## Now specify the exact models 
                   ## to evaluate:
                   tuneGrid = rfGrid)
  rfFit2
  
  getModelInfo('rf')
  ?getModelInfo
  
  trellis.par.set(caretTheme())
  plot(gbmFit2)
  
  trellis.par.set(caretTheme())
  plot(gbmFit2, metric = "Kappa")
  
  ggplot(gbmFit2)  
  
  
  trellis.par.set(caretTheme())
  plot(rfFit2)  
  
  
  trellis.par.set(caretTheme())
  plot(rfFit2, metric = "Sens")
  
  
  
  #To rebuild the boosted tree model using this criterion, 
  #we can see the relationship between the tuning parameters and the area under the ROC curve using the following code:
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 10,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using 
                             ## the following function
                             summaryFunction = twoClassSummary)
  
  set.seed(825)
  gbmFit3 <- train(Class ~ ., data = training, 
                   method = "gbm", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = gbmGrid,
                   ## Specify which metric to optimize
                   metric = "ROC")
  gbmFit3
  