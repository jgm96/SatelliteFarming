library(ggplot2)
library(SnowballC)
library(psych)
require(caret)
require(RWeka)

options(warn=-1)

set.seed(1)
datos<-read.csv("ESTIMACION\ CARGA\ FINAL_withData.csv", header=TRUE)

elimina<-which((datos[,"Lote"]=="Millan")|(datos[,"Lote"]=="Rangoni")|(datos[,"Lote"]=="Rincon"))
datos<-datos[-elimina,]
datos<-datos[-c(1843,1844),]
datos$Suelo[datos$Suelo=="Arenoso "]<-"Arenoso" 
datos$Suelo<-factor(datos$Suelo)
est<-summary(datos)

#Variables to use
basicas<-c(8,12,16:18,23,56)
basicasndvi<-c(8,12,16:18,23,56,65,66)
volumenes<-c(8,12,16,17,18,23,57:62,56)
volumenesndvi<-c(8,12,16,17,18,23,57:62,56,65,66)
numerosNSEW<-c(8,12,16:18,23,42:45,56)
numerosNSEWndvi<-c(8,12,16:18,23,42:45,56,65,66)
todo<-c(4,5,6,8,12,16:18,23,57:62,42:46,56)
todondvi<-c(4,5,6,8,12,16:18,23,57:62,42:46,56,65,66)

######################## SELECT VARIABLES TO TRAIN WITH #####################################

variables <- list(basicas, basicasndvi, volumenes, volumenesndvi,numerosNSEW, numerosNSEWndvi, todo, todondvi)

####################### SELECT VARIABLE NAMES TO EXPORT RESULTS #############################

variables_used <- c("basicas", "basicasndvi", "volumenes", "volumenesndvi", "numerosNSEW", "numerosNSEWndvi", "todo", "todondvi")

###################### SELECT METHODS TO TRAIN WITH ##################################

methods.used <- c("M5","knn","svmRadial","cubist","gbm","bayesglm")

#Remove NA values
prediction <- na.omit(datos)

#Splitting data using Caret Training (75%) and test (25%).
trainIndex <- createDataPartition(prediction$Cosecha, p=.75, list = FALSE, times = 1)

prediction.train <- prediction[-trainIndex, ]
prediction.test <- prediction[trainIndex, ]

#Configure train control (10-fold CrossValidation)
train_control <- trainControl(method="cv", number = 10)

parentPath <- paste0(getwd(),"/CrossValidated")

#We create the directories to store results only if they do not exist.

cat("======================================================\n")
cat("CREATING DIRECTORIES FOR OUTPUT DATA\n")
cat("======================================================\n")

#Parent directory
if(dir.exists(parentPath)){
  message(paste0("Directory \"", parentPath, "\" already exists."))
} else {
  cat("Creating directory \"", parentPath, "\" ...", "\n", sep = "")
  dir.create(parentPath, showWarnings = FALSE)
  cat("Done.", "\n")
}

#Child directories
for(selectedMethod in methods.used){
  newDir <- paste0(parentPath, "/", selectedMethod)
  if(dir.exists(newDir)){
    message(paste0("Directory \"", newDir, "\" already exists."))  
  }
  else{
    cat("Creating directory \"", newDir, "\" ...", "\n", sep = "")
    dir.create(newDir, showWarnings = FALSE)
    cat("Done.", "\n")
  }
}

cat("======================================================\n")
cat("RESULTS OBTENTION\n")
cat("======================================================\n")

for(selectedMethod in methods.used){
  index = 1
  cat("Selected method: ", selectedMethod, "\n")
  for(variable in variables){
    garbage <- capture.output(fit <- train(Cosecha ~., data=prediction.train[variable], method=selectedMethod,
                                 trControl = train_control))
    prediction <- predict(fit,prediction.test[variable])
    model <- data.frame(obs = prediction.test[variable]$Cosecha, pred = prediction)
    out <- capture.output(fit, summary(fit), fit$finalModel, defaultSummary(model))
    outputFile <- paste0(parentPath, "/", selectedMethod, "/", variables_used[index],".txt")
    write(out, file = outputFile, sep = " ")
    cat("Results saved in: \"", outputFile, "\"","\n", sep = "")
    index <- index +1
  }
}
cat("Finished with success.")

