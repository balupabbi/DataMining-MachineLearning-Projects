#Set the base directory
setwd("School/Pattern Recognition and Data Mining/EegHandRecognition/R")

set.seed(100)
options(scipen=999) #no scientific numbers
options (digits = 4) #hold decimal places to 4

#Loading the necessary libraries
library(signal)
library(dplyr)
library(caret)
library(stats)

#Initialize variables
num_subjects <- 1
num_series <- 8
num_test_subjects <- 1

#function definitons

##Function to apply the band-pass filtering
apply_filter <- function(dataMatrix, bfFilters) {
  cols <- ncol(dataMatrix)
  rows <- nrow(dataMatrix)
  
  fMatrix <- matrix(nrow = rows, ncol = cols)
  finalMatrix <- matrix(nrow = rows, ncol = 39)
  
  for(i in 1:cols) {
    d <- dataMatrix[,i]
    fMatrix[,i] <- signal::filter(bfFilters, t(d))
  }
  
  colnames(fMatrix) = c("Fp1", "Fp2", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "TP9", "CP5", "CP1", "CP2", "CP6", "TP10", "P7", "P3", "Pz", "P4", "P8", "PO9", "O1", "Oz", "O2", "PO10")
  
  fMatrix
}

#Reading in the training data
for(i in 1:num_subjects) {
  list_dataMatrix <- list()
  list_eventMatrix <- list()
  for(j in 1 : num_series) {
    print(paste("Analyzing Subject: ", i, "Series:", j))
    subj_series_data <- paste('train/subj', i,'_series', j, '_data.csv', sep='')
    subj_series_event <- paste('train/subj', i,'_series', j, '_events.csv', sep='') 

    list_dataMatrix[[j]] <- read.csv(subj_series_data)
    list_eventMatrix[[j]] <- read.csv(subj_series_event)
  }
  
  
  d <- rbind_all(list_dataMatrix)
  e <- rbind_all(list_eventMatrix)
  
  f<- cbind(d[,-1], e[,-1])
  
  inTrain <- createDataPartition(y = f$HandStart, p = .75, list = FALSE)
  
  training <- f[inTrain, ]
  testing <- f[-inTrain, ]

  rm(list_eventMatrix, list_dataMatrix)
}


#Readin in the testing data
for(i in 1:num_test_subjects) {
  list_dataMatrix <- list()
  list_eventMatrix <- list()
  dataMatrixName <- paste('subj', i, '_testData', sep='')
  for(j in 9 : 10) {
    print(paste("Analyzing Subject: ", i, "Series:", j))
    subj_series_data <- paste('test/subj', i,'_series', j, '_data.csv', sep='') 
    
    list_dataMatrix[[j]] <- read.csv(subj_series_data)
  }
  
  assign(dataMatrixName, rbind_all(list_dataMatrix))
  
  rm(list_dataMatrix)
}



################################ Pre processing #####################################

subj1_trainData <- training[,-c(33:38)]
subj1_trainEvent <- training[,-c(1:32)]

 subj1_testD <- testing[,-c(33:38)]
 subj1_testE <- testing[,-c(1:32)]

#Calculating the band-pass filter
bFilters <- butter(n = 5, W = c(0.028, 0.12), type = "pass", plane = "z")

#APPLYING THE FILTERS TO THE DATA
subj1_bFiltered <- apply_filter(subj1_trainData, bFilters)
rm(subj1_trainData)

subj1_bTest <- apply_filter(subj1_testData[,-1], bFilters)

subj1_tt <- apply_filter(subj1_testD, bFilters)
rm(subj1_trainData)

subj2_bFiltered <- apply_filter(subj2_trainData, bFilters)
rm(subj2_trainData)
subj3_bFiltered <- apply_filter(subj3_trainData, bFilters)
rm(subj3_trainData)
subj4_bFiltered <- apply_filter(subj4_trainData, bFilters)
rm(subj4_trainData)
subj5_bFiltered <- apply_filter(subj5_trainData, bFilters)
rm(subj5_trainData)
subj6_bFiltered <- apply_filter(subj6_trainData, bFilters)
rm(subj6_trainData)
subj7_bFiltered <- apply_filter(subj7_trainData, bFilters)
rm(subj7_trainData)
subj8_bFiltered <- apply_filter(subj8_trainData, bFilters)
rm(subj8_trainData)
subj9_bFiltered <- apply_filter(subj9_trainData, bFilters)
rm(subj9_trainData)
subj10_bFiltered <- apply_filter(subj10_trainData, bFilters)
rm(subj10_trainData)
subj11_bFiltered <- apply_filter(subj11_trainData, bFilters)
rm(subj11_trainData)
subj12_bFiltered <- apply_filter(subj12_trainData, bFilters)
rm(subj12_trainData)

#Apply PCA to reduce the dimensions
pca1 <- prcomp(subj1_bFiltered, retx = TRUE, center = TRUE, scale. = TRUE)
finalSubj1Pca <- pca1$x[,1:15]

finalSubj1Matrix <- cbind(finalSubj1Pca, subj1_trainEvent)

pcaTest <- prcomp(subj1_bTest, retx = TRUE, center = TRUE, scale. = TRUE)
finalTestSubj1Matrix <- pcaTest$x[,1:15]

pca <- prcomp(subj1_tt, retx = TRUE, center = TRUE, scale. = TRUE)
finalSubj1 <- cbind(pca$x[,1:15], subj1_testE)

################################# Logistic Regression ##################################
#v33 -> HandStart
#v34 -> FirstDigitTouch
#v35 -> BothStartLoadPhase
#v36 -> LiftOff
#v37 -> Replace
#v38 -> BothReleased

#formule for regression model 
#formula1 <- HandStart ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10
#formula2 <- FirstDigitTouch ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10
#formula3 <- BothStartLoadPhase ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10
#formula4 <- LiftOff ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10
#formula5 <- Replace ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10
#formula6 <- BothReleased ~ Fp1+Fp2+F7+F3+Fz+F4+F8+FC5+FC1+FC2+FC6+T7+C3+Cz+C4+T8+TP9+CP5+CP1+CP2+CP6+TP10+P7+P3+Pz+P4+P8+PO9+O1+Oz+02+PO10

formula1 <- HandStart ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula2 <- FirstDigitTouch ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula3 <- BothStartLoadPhase ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula4 <- LiftOff ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula5 <- Replace ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula6 <- BothReleased ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15



train_control <- trainControl(method="cv", number=10)

lrFitHS <- train(formula1, data = finalSubj1Matrix, method = "glm", family="binomial", trControl = train_control)
lrClassesHS <- predict(lrFitHS,newdata = finalSubj1, type = "raw" )


lrFitHS <- glm(formula1, data=finalSubj1Matrix,family=binomial)
lrClassesHS <- predict(lrFitHS,newdata=finalSubj1,type='response')

summary(lrFitHS)

finalSubj1$prob = lrClassesHS
#confusionMatrix(data = lrClassesHS, finalTestSubj1Matrix$HandStart)


lrFitFDT <- train(formula2, data = finalSubj1Matrix, method = "glm", family="binomial")
lrClassesFDT <- predict(lrFitFDT,newdata = finalTestSubj1Matrix)



# lrFitBS <- train(formula3, data = finalSubj1Matrix, method = "glm", family="binomial")
# lrClassesBS <- predict(lrFitBS,newdata = finalTestSubj1Matrix)
# 
# lrFitLO <- train(formula4, data = finalSubj1Matrix, method = "glm", family="binomial")
# lrClassesLO <- predict(lrFitLO,newdata = finalTestSubj1Matrix)
# 
# lrFitR <- train(formula4, data = finalSubj1Matrix, method = "glm", family="binomial")
# lrClassesR <- predict(lrFitR,newdata = finalTestSubj1Matrix)
# 
# lrFitBR <- train(formula16, data = finalSubj1Matrix, method = "glm", family="binomial")
# lrClassesBR <- predict(lrFitBR,newdata = finalTestSubj1Matrix)

finalDataFrame <- data.frame(subj1_testData$id, lrClassesHS)
finalDataFrame[is.na(finalDataFrame)] <- 0
names(finalDataFrame) <- c('id','HandStart')

write.csv(finalDataFrame,file='submission1.csv',row.names=FALSE)