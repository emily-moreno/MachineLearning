library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)
dataframe2 <- read_delim("Universidad/electiva/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dataframe2)

#--Funcion de normalizacion---#
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#----Cambio a factor mi clase----#
dataframe2$tp <-
  as.factor(dataframe2$tp)
#---ploteo mi dataframe----#
#plot(dataframe2[1:5])

#---miro las proporciones de mi datframe-#
prop.table(table(dataframe2$tp))

#-----#

hist(dataframe2$`sensor1`, breaks = 50)
hist(dataframe2$`sensor2`, breaks = 50)
hist(dataframe2$`sensor3`, breaks = 50)

#--normalizo las variables--#

normData <- dataframe2
standardData <- dataframe2

#--minmax--#
### min-max
normData$sensor1 <-
  normalise(dataframe2$sensor1)

normData$sensor2 <-
  normalise(dataframe2$`sensor2`)

normData$sensor3 <-
  normalise(dataframe2$`sensor3`)

normData$distancia <-
  normalise(dataframe2$`distancia`)

#-- z- score-#
standardData$sensor1 <-
  scale(dataframe2$`sensor1`)
standardData$sensor2 <-
  scale(dataframe2$`sensor2`)
standardData$sensor3 <-
  scale(dataframe2$`sensor3`)
standardData$distancia <-
  scale(dataframe2$`distancia`)
#----Parto mi dataframe----#
sample.index <- sample(1:nrow(dataframe2)
                       ,nrow(dataframe2)*0.7
                       ,replace = F)

#--entrenamiento y test---#
k <- 5
predictors <- c("sensor1","sensor2","sensor3","distancia")

# original data
train.data <-dataframe2[sample.index,c(predictors,"tp"),drop=F]
test.data <-dataframe2[-sample.index,c(predictors,"tp"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors], test = test.data[predictors],cl = train.data$tp, k=k)
#---Gmoldes---#

CrossTable(x = test.data$tp, y = prediction
           
           , prop.chisq = F)


#--------Normalizado--------#

train.data <-normData[sample.index,c(predictors,"tp"),drop=F]
test.data <-normData[-sample.index,c(predictors,"tp"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors]
                  
                  , test = test.data[predictors]
                  ,cl = train.data$tp, k=k)
#---Gmoldes---#
CrossTable(x = test.data$tp, y = prediction
           
           , prop.chisq = F)



#-----------StandarDAta--------#

train.data <-standardData[sample.index,c(predictors,"tp"),drop=F]
test.data <-standardData[-sample.index,c(predictors,"tp"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors]
                  
                  , test = test.data[predictors]
                  ,cl = train.data$tp, k=k)
#---Gmoldes---#
CrossTable(x = test.data$tp, y = prediction
           
           , prop.chisq = F)

#--------------------Metodo K----#
ctrl <- trainControl(method="cv", p=0.7)
knnFit <- train(tp ~ sensor1+sensor2+sensor3
            , data = train.data
                , method = "knn"
                
                , trControl = ctrl
                
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength =20 )

#Output of kNN fit
knnFit
plot(knnFit)

#Get predictions for the testing data
knnPredict <- predict(knnFit, newdata = test.data)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, test.data$tp)



#------------Cross-validation----#

data.shuffled <- dataframe2[sample(nrow(dataframe2)), ]
folds <- cut(seq(1, nrow(data.shuffled)), breaks = 5, labels = FALSE)


errors <- NULL
for (i in 1:5) {
  fold.indexes <- which(folds == i, arr.ind = TRUE)
  test.data <- data.shuffled[fold.indexes, ]
  training.data <- data.shuffled[-fold.indexes, ]
  #creating a model with all predictors
  train.linear <-knn(train = train.data[predictors], test = test.data[predictors],cl = train.data$tp, k=k)

  train.output <- predict(train.linear, test.data)
  
  #error calculated as root-mean-square error
  #NOTE: be sure the correct response variable is used
  #(test.data$charges in this example)
  errors <- c(errors, sqrt(sum(((train.output - test.data$tp)^2/length(train.output)))))
}
errors





#----------------------------------------------------#




sample.index <- sample(1:nrow(dataframe2)
                       
                       , nrow(dataframe2) * 0.75, replace = FALSE)

training.data <- dataframe2[sample.index,, drop=F]
test.data <- dataframe2[-sample.index,, drop=F]
#creating a model with 4 predictors
prediction <- knn(train = training.data[predictors], test = test.data[predictors],cl = training.data$tp, k=k)
# Make predictions and compute the R2, RMSE and MAE using CARET
predictions<-predict(prediction,test.data)
data.frame( R2 = R2(predictions, test.data$charges),
            RMSE = RMSE(predictions, test.data$charges),
            MAE = MAE(predictions, test.data$charges))
means(errors)

##----------------------------##
#crossvalidation
# Define training control
set.seed(1)
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(tp ~ sensor1+sensor2+sensor3 data = data.all, method = "knn"
,trControl = train.control)

# Summarize the results
print(model)

#------------#
