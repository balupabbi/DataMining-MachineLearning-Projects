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
num_subjects <- 12
num_series <- 8
num_test_subjects <- 12

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

##Function to apply PCA
apply_pca <-  function(dataMatrix){
  pca <- prcomp(dataMatrix, retx = TRUE, center = TRUE, scale. = TRUE)
  finalDataMatrix <- pca$x
  finalDataMatrix
}

##Function to apply logistic regression
apply_logistic <- function(trainMatrix, cvMatrix, testMatrix, formula){ 
  train_control <- trainControl(method="cv", number=10)
  #fit the training data
  lrFit <- train(formula, data = trainMatrix, method = "glm", family="binomial", trControl = train_control)
  #lrFit <- glm(formula, data = trainMatrix, family = binomial)
  lrClassCV <- predict(lrFit, newdata = cvMatrix, type = 'raw')
  lrClassTest <- predict(lrFit, newdata = testMatrix, type = 'raw')

  listReturn <- list(lrFit, lrClassCV, lrClassTest)
  listReturn
}

#Reading in the training data
for(i in 1:num_subjects) {
  list_dataMatrix <- list()
  list_eventMatrix <- list()

  trainingName <- paste('training', i, sep='')
  testingName <-paste('testing', i, sep='')

  for(j in 1 : num_series) {
    print(paste("Analyzing Subject: ", i, "Series:", j))
    subj_series_data <- paste('train/subj', i,'_series', j, '_data.csv', sep='')
    subj_series_event <- paste('train/subj', i,'_series', j, '_events.csv', sep='') 

    list_dataMatrix[[j]] <- read.csv(subj_series_data)
    list_eventMatrix[[j]] <- read.csv(subj_series_event)
  }
  
  
  d <- rbind_all(list_dataMatrix)
  e <- rbind_all(list_eventMatrix)
  
  f<- cbind(d, e[,-1])
  
  inTrain <- createDataPartition(y = f$HandStart, p = .75, list = FALSE)
  
  assign(trainingName, f[inTrain, ])
  assign(testingName, f[-inTrain, ])

  rm(list_eventMatrix, list_dataMatrix, d, e, f, inTrain)
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

#subj1
subj1_trainLabels <- traingin1[, c(1)]
subj1_trainData <- training1[,c(2:33)]
subj1_trainEvent <- training1[,c(34:39)]

subj1_cvLabels <- testing1[, c(1)]
subj1_cvData <- testing1[,c(2:33)]
subj1_cvEvent <- testing1[,c(34:39)]

sub1_testLabels <- subj1_testData[,c(1)]
subj1_testData <- subj1_testData[,c(2:33)]

rm(training1, testing1)

#subj 2
subj2_trainLabels <- traingin2[, c(1)]
subj2_trainData <- training2[,c(2:33)]
subj2_trainEvent <- training2[,c(34:39)]

subj2_cvLabels <- testing2[, c(1)]
subj2_cvData <- testing2[,c(2:33)]
subj2_cvEvent <- testing2[,c(34:39)]

sub2_testLabels <- subj2_testData[,c(1)]
subj2_testData <- subj2_testData[,c(2:33)]
rm(training2, testing2)

#subj3
subj3_trainLabels <- traingin3[, c(1)]
subj3_trainData <- training3[,c(2:33)]
subj3_trainEvent <- training3[,c(34:39)]

subj3_cvLabels <- testing3[, c(1)]
subj3_cvData <- testing3[,c(2:33)]
subj3_cvEvent <- testing3[,c(34:39)]

sub3_testLabels <- subj3_testData[,c(1)]
subj3_testData <- subj3_testData[,c(2:33)]
rm(training3, testing3)

#subj4
subj4_trainLabels <- traingin4[, c(1)]
subj4_trainData <- training4[,c(2:33)]
subj4_trainEvent <- training4[,c(34:39)]

subj4_cvLabels <- testing4[, c(1)]
subj4_cvData <- testing4[,c(2:33)]
subj4_cvEvent <- testing4[,c(34:39)]

sub4_testLabels <- subj4_testData[,c(1)]
subj4_testData <- subj4_testData[,c(2:33)]
rm(training4, testing4)

#subj5
subj5_trainLabels <- traingin5[, c(1)]
subj5_trainData <- training5[,c(2:33)]
subj5_trainEvent <- training5[,c(34:39)]

subj5_cvLabels <- testing5[, c(1)]
subj5_cvData <- testing5[,c(2:33)]
subj5_cvEvent <- testing5[,c(34:39)]

sub5_testLabels <- subj5_testData[,c(1)]
subj5_testData <- subj5_testData[,c(2:33)]
rm(training5, testing5)

#subj6
subj6_trainLabels <- traingin6[, c(1)]
subj6_trainData <- training6[,c(2:33)]
subj6_trainEvent <- training6[,c(34:39)]

subj6_cvLabels <- testing6[, c(1)]
subj6_cvData <- testing6[,c(2:33)]
subj6_cvEvent <- testing6[,c(34:39)]

sub6_testLabels <- subj6_testData[,c(1)]
subj6_testData <- subj6_testData[,c(2:33)]
rm(training6, testing6)

#subj7
subj7_trainLabels <- traingin7[, c(1)]
subj7_trainData <- training7[,c(2:33)]
subj7_trainEvent <- training7[,c(34:39)]

subj7_cvLabels <- testing7[, c(1)]
subj7_cvData <- testing7[,c(2:33)]
subj7_cvEvent <- testing7[,c(34:39)]

sub7_testLabels <- subj7_testData[,c(1)]
subj7_testData <- subj7_testData[,c(2:33)]
rm(training7, testing7)

#subj1
subj8_trainLabels <- traingin8[, c(1)]
subj8_trainData <- training8[,c(2:33)]
subj8_trainEvent <- training8[,c(34:39)]

subj8_cvLabels <- testing8[, c(1)]
subj8_cvData <- testing8[,c(2:33)]
subj8_cvEvent <- testing8[,c(34:39)]

sub8_testLabels <- subj8_testData[,c(1)]
subj8_testData <- subj8_testData[,c(2:33)]
rm(training8, testing8)

#subj9
subj9_trainLabels <- traingin9[, c(1)]
subj9_trainData <- training9[,c(2:33)]
subj9_trainEvent <- training9[,c(34:39)]

subj9_cvLabels <- testing9[, c(1)]
subj9_cvData <- testing9[,c(2:33)]
subj9_cvEvent <- testing9[,c(34:39)]

subj9_testLabels <- subj9_testData[,c(1)]
subj9_testData <- subj9_testData[,c(2:33)]
rm(training9, testing9)

#subj1
subj10_trainLabels <- traingin10[, c(1)]
subj10_trainData <- training10[,c(2:33)]
subj10_trainEvent <- training10[,c(34:39)]

subj10_cvLabels <- testing10[, c(1)]
subj10_cvData <- testing10[,c(2:33)]
subj10_cvEvent <- testing10[,c(34:39)]

sub10_testLabels <- subj10_testData[,c(1)]
subj10_testData <- subj10_testData[,c(2:33)]
rm(training10, testing10)

#subj1
subj11_trainLabels <- traingin11[, c(1)]
subj11_trainData <- training11[,c(2:33)]
subj11_trainEvent <- training11[,c(34:39)]

subj11_cvLabels <- testing11[, c(1)]
subj11_cvData <- testing11[,c(2:33)]
subj11_cvEvent <- testing11[,c(34:39)]

sub11_testLabels <- subj11_testData[,c(1)]
subj11_testData <- subj11_testData[,c(2:33)]
rm(training11, testing11)

#subj12
subj12_trainLabels <- traingin12[, c(1)]
subj12_trainData <- training12[,c(2:33)]
subj12_trainEvent <- training12[,c(34:39)]

subj12_cvLabels <- testing12[, c(1)]
subj12_cvData <- testing12[,c(2:33)]
subj12_cvEvent <- testing12[,c(34:39)]

sub12_testLabels <- subj12_testData[,c(1)]
subj12_testData <- subj12_testData[,c(2:33)]
rm(training12, testing12)

#Calculating the band-pass filter
bFilters <- butter(n = 5, W = c(0.028, 0.12), type = "pass", plane = "z")

#APPLYING THE FILTERS TO THE DATA
#subject 1
subj1_bfTrainData <- apply_filter(subj1_trainData, bFilters)
subj1_bfCVData <- apply_filter(subj1_cvData, bFilters)
subj1_bfTestData <- apply_filter(subj1_testData, bFilters)
rm(subj1_trainData, subj1_cvData, subj1_testData)

#subject 2
subj2_bfTrainData <- apply_filter(subj2_trainData, bFilters)
subj2_bfCVData <- apply_filter(subj2_cvData, bFilters)
subj2_bfTestData <- apply_filter(subj2_testData, bFilters)
rm(subj2_trainData, subj2_cvData, subj2_testData)

#subject 3
subj3_bfTrainData <- apply_filter(subj3_trainData, bFilters)
subj3_bfCVData <- apply_filter(subj3_cvData, bFilters)
subj3_bfTestData <- apply_filter(subj3_testData, bFilters)
rm(subj3_trainData, subj3_cvData, subj3_testData)

#subject 4
subj4_bfTrainData <- apply_filter(subj4_trainData, bFilters)
subj4_bfCVData <- apply_filter(subj4_cvData, bFilters)
subj4_bfTestData <- apply_filter(subj4_testData, bFilters)
rm(subj4_trainData, subj4_cvData, subj4_testData)

#subject 5
subj5_bfTrainData <- apply_filter(subj5_trainData, bFilters)
subj5_bfCVData <- apply_filter(subj5_cvData, bFilters)
subj5_bfTestData <- apply_filter(subj5_testData, bFilters)
rm(subj5_trainData, subj5_cvData, subj5_testData)

#subject 6
subj6_bfTrainData <- apply_filter(subj6_trainData, bFilters)
subj6_bfCVData <- apply_filter(subj6_cvData, bFilters)
subj6_bfTestData <- apply_filter(subj6_testData, bFilters)
rm(subj6_trainData, subj6_cvData, subj6_testData)

#subject 7
subj7_bfTrainData <- apply_filter(subj7_trainData, bFilters)
subj7_bfCVData <- apply_filter(subj7_cvData, bFilters)
subj7_bfTestData <- apply_filter(subj7_testData, bFilters)
rm(subj7_trainData, subj7_cvData, subj7_testData)

#subject 8
subj8_bfTrainData <- apply_filter(subj8_trainData, bFilters)
subj8_bfCVData <- apply_filter(subj8_cvData, bFilters)
subj8_bfTestData <- apply_filter(subj8_testData, bFilters)
rm(subj8_trainData, subj8_cvData, subj8_testData)

#subject 9
subj9_bfTrainData <- apply_filter(subj9_trainData, bFilters)
subj9_bfCVData <- apply_filter(subj9_cvData, bFilters)
subj9_bfTestData <- apply_filter(subj9_testData, bFilters)
rm(subj9_trainData, subj9_cvData, subj9_testData)

#subject 10
subj10_bfTrainData <- apply_filter(subj10_trainData, bFilters)
subj10_bfCVData <- apply_filter(subj10_cvData, bFilters)
subj10_bfTestData <- apply_filter(subj10_testData, bFilters)
rm(subj10_trainData, subj10_cvData, subj10_testData)

#subject 11
subj11_bfTrainData <- apply_filter(subj11_trainData, bFilters)
subj11_bfCVData <- apply_filter(subj11_cvData, bFilters)
subj11_bfTestData <- apply_filter(subj11_testData, bFilters)
rm(subj11_trainData, subj11_cvData, subj11_testData)

#subject 12
subj12_bfTrainData <- apply_filter(subj12_trainData, bFilters)
subj12_bfCVData <- apply_filter(subj12_cvData, bFilters)
subj12_bfTestData <- apply_filter(subj12_testData, bFilters)
rm(subj12_trainData, subj12_cvData, subj12_testData)


#APPLY PCA
subj1_pcaTrain <- apply_pca(subj1_bfTrainData)
subj1_pcaCv <- apply_pca(subj1_bfCVData)
subj1_pcaTest <- apply_pca(subj1_bfTestData)

subj1_finalTrain <- cbind(subj1_pcaTrain, subj1_trainEvent)
subj1_finalCv <- cbind(subj1_pcaCv, subj1_cvEvent)

rm(subj1_bfTrainData, subj1_bfCVData, subj1_bfTestData, subj1_pcaTrain, subj1_trainEvent, subj1_pcaCv, subj1_cvEvent)



subj2_pcaTrain <- apply_pca(subj2_bfTrainData)
subj2_pcaCv <- apply_pca(subj2_bfCVData)
subj2_pcaTest <- apply_pca(subj2_bfTestData)

subj2_finalTrain <- cbind(subj2_pcaTrain, subj2_trainEvent)
subj2_finalCv <- cbind(subj2_pcaCv, subj2_cvEvent)

rm(subj2_bfTrainData, subj2_bfCVData, subj2_bfTestData, subj2_pcaTrain, subj2_trainEvent, subj2_pcaCv, subj2_cvEvent)


subj3_pcaTrain <- apply_pca(subj3_bfTrainData)
subj3_pcaCv <- apply_pca(subj3_bfCVData)
subj3_pcaTest <- apply_pca(subj3_bfTestData)

subj3_finalTrain <- cbind(subj3_pcaTrain, subj3_trainEvent)
subj3_finalCv <- cbind(subj3_pcaCv, subj3_cvEvent)

rm(subj3_bfTrainData, subj3_bfCVData, subj3_bfTestData, subj3_pcaTrain, subj3_trainEvent, subj3_pcaCv, subj3_cvEvent)


subj4_pcaTrain <- apply_pca(subj4_bfTrainData)
subj4_pcaCv <- apply_pca(subj4_bfCVData)
subj4_pcaTest <- apply_pca(subj4_bfTestData)

subj4_finalTrain <- cbind(subj4_pcaTrain, subj4_trainEvent)
subj4_finalCv <- cbind(subj4_pcaCv, subj4_cvEvent)

rm(subj4_bfTrainData, subj4_bfCVData, subj4_bfTestData, subj4_pcaTrain, subj4_trainEvent, subj4_pcaCv, subj4_cvEvent)


subj5_pcaTrain <- apply_pca(subj5_bfTrainData)
subj5_pcaCv <- apply_pca(subj5_bfCVData)
subj5_pcaTest <- apply_pca(subj5_bfTestData)

subj5_finalTrain <- cbind(subj5_pcaTrain, subj5_trainEvent)
subj5_finalCv <- cbind(subj5_pcaCv, subj5_cvEvent)

rm(subj5_bfTrainData, subj5_bfCVData, subj5_bfTestData, subj5_pcaTrain, subj5_trainEvent, subj5_pcaCv, subj5_cvEvent)


subj6_pcaTrain <- apply_pca(subj6_bfTrainData)
subj6_pcaCv<- apply_pca(subj6_bfCVData)
subj6_pcaTest <- apply_pca(subj6_bfTestData)

subj6_finalTrain <- cbind(subj6_pcaTrain, subj6_trainEvent)
subj6_finalCv <- cbind(subj6_pcaCv, subj6_cvEvent)

rm(subj6_bfTrainData, subj6_bfCVData, subj6_bfTestData, subj6_pcaTrain, subj6_trainEvent, subj6_pcaCv, subj6_cvEvent)



subj7_pcaTrain <- apply_pca(subj7_bfTrainData)
subj7_pcaCv <- apply_pca(subj7_bfCVData)
subj7_pcaTest <- apply_pca(subj7_bfTestData)

subj7_finalTrain <- cbind(subj7_pcaTrain, subj7_trainEvent)
subj7_finalCv <- cbind(subj7_pcaCv, subj7_cvEvent)

rm(subj7_bfTrainData, subj7_bfCVData, subj7_bfTestData, subj7_pcaTrain, subj7_trainEvent, subj7_pcaCv, subj7_cvEvent)


subj8_pcaTrain <- apply_pca(subj8_bfTrainData)
subj8_pcaCv <- apply_pca(subj8_bfCVData)
subj8_pcaTest <- apply_pca(subj8_bfTestData)

subj8_finalTrain <- cbind(subj8_pcaTrain, subj8_trainEvent)
subj8_finalCv <- cbind(subj8_pcaCv, subj8_cvEvent)

rm(subj8_bfTrainData, subj8_bfCVData, subj8_bfTestData, subj8_pcaTrain, subj8_trainEvent, subj8_pcaCv, subj8_cvEvent)


subj9_pcaTrain <- apply_pca(subj9_bfTrainData)
subj9_pcaCv <- apply_pca(subj9_bfCVData)
subj9_pcaTest <- apply_pca(subj9_bfTestData)

subj9_finalTrain <- cbind(subj9_pcaTrain, subj9_trainEvent)
subj9_finalCv <- cbind(subj9_pcaCv, subj9_cvEvent)

rm(subj9_bfTrainData, subj9_bfCVData, subj9_bfTestData, subj9_pcaTrain, subj9_trainEvent, subj9_pcaCv, subj9_cvEvent)


subj10_pcaTrain <- apply_pca(subj10_bfTrainData)
subj10_pcaCv <- apply_pca(subj10_bfCVData)
subj10_pcaTest <- apply_pca(subj10_bfTestData)

subj10_finalTrain <- cbind(subj10_pcaTrain, subj10_trainEvent)
subj10_finalCv <- cbind(subj10_pcaCv, subj10_cvEvent)

rm(subj10_bfTrainData, subj10_bfCVData, subj10_bfTestData,subj10_pcaTrain, subj10_trainEvent, subj10_pcaCv, subj10_cvEvent)


subj11_pcaTrain <- apply_pca(subj11_bfTrainData)
subj11_pcaCv <- apply_pca(subj11_bfCVData)
subj11_pcaTest <- apply_pca(subj11_bfTestData)

subj11_finalTrain <- cbind(subj11_pcaTrain, subj11_trainEvent)
subj11_finalCv <- cbind(subj11_pcaCv, subj11_cvEvent)

rm(subj11_bfTrainData, subj11_bfCVData, subj11_bfTestData, subj11_pcaTrain, subj11_trainEvent, subj11_pcaCv, subj11_cvEvent)


subj12_pcaTrain <- apply_pca(subj12_bfTrainData)
subj12_pcaCv <- apply_pca(subj12_bfCVData)
subj12_pcaTest <- apply_pca(subj12_bfTestData)

subj12_finalTrain <- cbind(subj12_pcaTrain, subj12_trainEvent)
subj12_finalCv <- cbind(subj12_pcaCv, subj12_cvEvent)

rm(subj12_bfTrainData, subj12_bfCVData, subj12_bfTestData, subj12_pcaTrain, subj12_trainEvent, subj12_pcaCv, subj12_cvEvent)


################################# Logistic Regression ##################################

#formulae for regression model 
formula1 <- HandStart ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula2 <- FirstDigitTouch ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula3 <- BothStartLoadPhase ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula4 <- LiftOff ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula5 <- Replace ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15
formula6 <- BothReleased ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15

logisticSubj1_hs <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula1)
logisticSubj2_hs <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula1)
logisticSubj3_hs <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula1)
logisticSubj4_hs <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula1)
logisticSubj5_hs <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula1)
logisticSubj6_hs <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula1)
logisticSubj7_hs <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula1)
logisticSubj8_hs <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula1)
logisticSubj9_hs <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula1)
logisticSubj10_hs <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula1)
logisticSubj11_hs <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula1)
logisticSubj12_hs <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula1)

logisticSubj1_fdt <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula2)
logisticSubj2_fdt <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula2)
logisticSubj3_fdt <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula2)
logisticSubj4_fdt <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula2)
logisticSubj5_fdt <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula2)
logisticSubj6_fdt <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula2)
logisticSubj7_fdt <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula2)
logisticSubj8_fdt <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula2)
logisticSubj9_fdt <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula2)
logisticSubj10_fdt <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula2)
logisticSubj11_fdt <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula2)
logisticSubj12_fdt <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula2)

logisticSubj1_bsl <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula3)
logisticSubj2_bsl <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula3)
logisticSubj3_bsl <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula3)
logisticSubj4_bsl <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula3)
logisticSubj5_bsl <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula3)
logisticSubj6_bsl <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula3)
logisticSubj7_bsl <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula3)
logisticSubj8_bsl <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula3)
logisticSubj9_bsl <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula3)
logisticSubj10_bsl <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula3)
logisticSubj11_bsl <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula3)
logisticSubj12_bsl <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula3)

logisticSubj1_lo <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula4)
logisticSubj2_lo <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula4)
logisticSubj3_lo <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula4)
logisticSubj4_lo <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula4)
logisticSubj5_lo <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula4)
logisticSubj6_lo <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula4)
logisticSubj7_lo <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula4)
logisticSubj8_lo <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula4)
logisticSubj9_lo <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula4)
logisticSubj10_lo <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula4)
logisticSubj11_lo <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula4)
logisticSubj12_lo <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula4)

logisticSubj1_r <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula5)
logisticSubj2_r <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula5)
logisticSubj3_r <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula5)
logisticSubj4_r <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula5)
logisticSubj5_r <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula5)
logisticSubj6_r <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula5)
logisticSubj7_r <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula5)
logisticSubj8_r <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula5)
logisticSubj9_r <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula5)
logisticSubj10_r <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula5)
logisticSubj11_r <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula5)
logisticSubj12_r <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula5)

logisticSubj1_br <- apply_logistic(subj1_finalTrain, subj1_finalCv, subj1_pcaTest, formula6)
logisticSubj2_br <- apply_logistic(subj2_finalTrain, subj2_finalCv, subj2_pcaTest, formula6)
logisticSubj3_br <- apply_logistic(subj3_finalTrain, subj3_finalCv, subj3_pcaTest, formula6)
logisticSubj4_br <- apply_logistic(subj4_finalTrain, subj4_finalCv, subj4_pcaTest, formula6)
logisticSubj5_br <- apply_logistic(subj5_finalTrain, subj5_finalCv, subj5_pcaTest, formula6)
logisticSubj6_br <- apply_logistic(subj6_finalTrain, subj6_finalCv, subj6_pcaTest, formula6)
logisticSubj7_br <- apply_logistic(subj7_finalTrain, subj7_finalCv, subj7_pcaTest, formula6)
logisticSubj8_br <- apply_logistic(subj8_finalTrain, subj8_finalCv, subj8_pcaTest, formula6)
logisticSubj9_br <- apply_logistic(subj9_finalTrain, subj9_finalCv, subj9_pcaTest, formula6)
logisticSubj10_br <- apply_logistic(subj10_finalTrain, subj10_finalCv, subj10_pcaTest, formula6)
logisticSubj11_br <- apply_logistic(subj11_finalTrain, subj11_finalCv, subj11_pcaTest, formula6)
logisticSubj12_br <- apply_logistic(subj12_finalTrain, subj12_finalCv, subj12_pcaTest, formula6)


