setwd("School/Pattern Recognition and Data Mining/EegHandRecognition/R")

library(dplyr)
library(tidyr)
library(stringr)

#initialize my functions
merge_sort <- function (eeg,events){
  # merge events with data
  data <- merge(eeg,events,'id')
  data$value <- NULL
  #seperate id into frame number and id
  data <- separate(data,id,c('subject','series','frame'),sep='_')
  data$frame <- as.numeric(data$frame)
  # order based on id
  data <- data[order(data$frame),]
  #data$seriesno <- NULL
  data <- unite(data,col=id,subject,series,frame,sep='_',remove = TRUE)
  data
}

left_hem <- function(train, test){
  # combine train and test data
  nrow_train <- nrow(train)
  nrow_test <- nrow(test)
  # create event columns in test data set so we can combine rows with training data
  test$HandStart <- 0
  test$FirstDigitTouch <- 0
  test$BothStartLoadPhase <- 0
  test$LiftOff <- 0
  test$Replace <- 0
  test$BothReleased <- 0
  # combine train and test data
  combi <- rbind(train,test)
  # remove all columns with data from right hemisphere
#   combi$Fp2 <- NULL
#   combi$F4 <- NULL
#   combi$F8 <- NULL
#   combi$FC2 <- NULL
#   combi$FC6 <- NULL
#   combi$C4 <- NULL
#   combi$T8 <- NULL
#   combi$CP2 <- NULL
#   combi$CP6 <- NULL
#   combi$TP10 <- NULL
#   combi$P4 <- NULL
#   combi$P8 <- NULL
#   combi$O2 <- NULL
  # split back into train and test data sets
  left_hem_train <- combi[1:nrow_train,]
  left_hem_test <- combi[(nrow_train+1):(nrow_train+nrow_test),]
  # remove event columns from training data
  left_hem_test$HandStart <- NULL
  left_hem_test$FirstDigitTouch <- NULL
  left_hem_test$BothStartLoadPhase <- NULL
  left_hem_test$LiftOff <- NULL
  left_hem_test$Replace <- NULL
  left_hem_test$BothReleased <- NULL
  # return two outputs as a list
  return(list(left_hem_train,left_hem_test)) 
}

glm_regression <- function(train,test){
  
  #formule for regression model
  formula1 <- HandStart ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10   
#   formula2 <- FirstDigitTouch ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10
#   formula3 <- BothStartLoadPhase ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10
#   formula4 <- LiftOff ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10
#   formula5 <- Replace ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10
#   formula6 <- BothReleased ~ Fp1+F7+F3+Fz+FC5+FC1+T7+C3+Cz+TP9+CP5+CP1+P7+P3+Pz+PO9+O1+Oz+PO10
  # train the logistic regression model
  reg_model1 <- glm(formula1, data=train,family=binomial)
#   reg_model2 <- glm(formula2, data=train,family=binomial)
#   reg_model3 <- glm(formula3, data=train,family=binomial)
#   reg_model4 <- glm(formula4, data=train,family=binomial)
#   reg_model5 <- glm(formula5, data=train,family=binomial)
#   reg_model6 <- glm(formula6, data=train,family=binomial)
  # create predictions
  my_predict1 <- predict(reg_model1,newdata=test,type='response')
#   my_predict2 <- predict(reg_model2,newdata=test,type='response')
#   my_predict3 <- predict(reg_model3,newdata=test,type='response')
#   my_predict4 <- predict(reg_model4,newdata=test,type='response')
#   my_predict5 <- predict(reg_model5,newdata=test,type='response')
#   my_predict6 <- predict(reg_model6,newdata=test,type='response')
  # combine predictions to create a single dataframe
  my_solution <- data.frame(test$id,my_predict1)
  my_solution    
  
}

set.seed(100)
options(scipen=999) #no scientific numbers
options (digits = 4) #hold decimal places to 4
# number of subjects you want to analyze
total_subj <- 1
# sub-sample training data to reduce computational load (min 1, max 8)
sub_sample <- 8
#initialize list
list_subj_predictions <- list()

# Loop through subjects
for (j in 1:total_subj){
  print(paste('Currently analysing subject',j))
  list_subj_traindata <- list() # initialize list   
  subject <- j
  # obtain all training data
  for (i in 1:sub_sample) {
    file_name_eeg <- paste('train/subj',subject,'_series',i,
                           '_data.csv',sep='') 
    file_name_events <- paste('train/subj',subject,'_series',i,
                              '_events.csv',sep='') 
    eeg <- read.csv(file_name_eeg, header=TRUE,
                    colClasses=c("character",rep("numeric",32)))#read data.csv based on file name
    events <- read.csv(file_name_events,header=TRUE,
                       colClasses=c("character",rep("numeric",6))) #read events.csv based on file name
    list_subj_traindata[[i]]<-merge_sort(eeg,events)
    rm(eeg,events)
  }
  #merge all training series data in 1 single data frame per subject
  train_variable_name <- paste('subj',subject,'_traindata',sep='')
  assign(train_variable_name,rbind_all(list_subj_traindata))
  #rm(list_subj_traindata)
  
  #obtain and merge test data from all series    
  list_subj_testdata <- list()
  count <- 0
  # i is series number
  for (i in 9:10) {
    count <- count + 1 
    file_name_eeg <- paste('test/subj',subject,'_series',i,
                           '_data.csv',sep='')
    eeg <- read.csv(file_name_eeg, header=TRUE,
                    colClasses=c("character",rep("numeric",32)))
    list_subj_testdata[[count]]<- eeg
    rm(eeg)
  }
  #merge all series data in 1 single data frame per subject
  test_variable_name <- paste('subj',subject,'_testdata',sep='')
  assign(test_variable_name,rbind_all(list_subj_testdata))
  
  
  #rm(list_subj_testdata)    
  
  # pre-processing of data
  # remove all EEG data from right hemisphere
  print('starting preprocessing')
  func_output <- left_hem(get(train_variable_name),get(test_variable_name))
  assign(train_variable_name,as.data.frame(func_output[[1]]))
  assign(test_variable_name,as.data.frame(func_output[[2]]))
  #rm(func_output)
  
  #Logistic regression regression
  print('now performing regression')
  list_subj_predictions[[j]]<- glm_regression(get(train_variable_name),
                                              get(test_variable_name))
  
 # rm(list=ls(pattern="^subj")) #remove variables in workspace to reduce memory usage
  my_predict <- list_subj_predictions[[1]]
  
}

# combine all subject predictions into 1 dataframe for submission

my_submission <- rbind_all(list_subj_predictions)
my_submission[is.na(my_submission)] <- 0
names(my_submission) <- c('id','HandStart')
my_submission[,2:ncol(my_submission)]= round(my_submission[,2:ncol(my_submission)],digits=4) # reduce file size by limiting number of decimal places

write.csv(my_submission,file='submission1.csv',row.names=FALSE)
