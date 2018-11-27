library(ggplot2)
library(SnowballC)
library(psych)
require(caret)
require(RWeka)

# Start measuring time
ptm <- proc.time()

#Suppress warnings in output
options(warn=-1)

set.seed(1)
datos<-read.csv("ESTIMACION\ CARGA\ FINAL_withData.csv", header=TRUE)

remove<-which((data[,"Lote"]=="Millan")|(data[,"Lote"]=="Rangoni")|(data[,"Lote"]=="Rincon"))
data<-data[-remove,]
data<-data[-c(1843,1844),]
data$Suelo[data$Suelo=="Arenoso "]<-"Arenoso" 
data$Suelo<-factor(data$Suelo)
est<-summary(data)

#String variables for outputs
str_prediction <- "\n\n========= Prediction ========\n\n"
str_separator.eq <- "======================================================\n"
str_separator.ln <- "------------------------------------------------------\n"
str_resultsObtention <- "RESULTS OBTENTION\n"
str_fit <- "\n\n========= Fit ========\n\n"
str_finalModel <- "\n\n========= Final Model ========\n\n"


#Variables to use
basics<-c(8,12,16:18,23,56)
basicsndvi<-c(8,12,16:18,23,56,65,66)
volumes<-c(8,12,16,17,18,23,57:62,56)
volumesndvi<-c(8,12,16,17,18,23,57:62,56,65,66)
numbersNSEW<-c(8,12,16:18,23,42:45,56)
numbersNSEWndvi<-c(8,12,16:18,23,42:45,56,65,66)
all<-c(4,5,6,8,12,16:18,23,57:62,42:46,56)
allndvi<-c(4,5,6,8,12,16:18,23,57:62,42:46,56,65,66)

######################## CONFIGURATION #####################################

#Select variables to train with
variables <- list(basics, basicsndvi, volumes, volumesndvi, numbersNSEW, numbersNSEWndvi, all, allndvi)

#Enter variable names to export results
variables_used <- c("basicas", "basicasndvi", "volumenes", "volumenesndvi", "numerosNSEW", "numerosNSEWndvi", "todo", "todondvi")

#Select methods to train with
#methods.used <- c("M5","knn","svmRadial","cubist","gbm","bayesglm", "rf")
methods.used <- c("M5","knn","svmRadial","cubist","gbm", "rf")
#methods.used <- c("rf")

#Indicate whether plot graphs
plotting.fit <- "Yes"
plotting.model <- "Yes"

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

#Creates directories

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

############### RESULTS OBTENTION ################

cat(str_separator.eq)
cat(str_resultsObtention)
cat(str_separator.eq)


for(selectedMethod in methods.used){
  index = 1
  cat("Selected method: ", selectedMethod, "\n")
  for(variable in variables){
    
    #Training, capturing output to maintain the console clean for methods such as gbm.
    garbage <- capture.output(fit <- train(Cosecha ~., data=prediction.train[variable], method=selectedMethod,
                                 trControl = train_control))
    
    #Predict using fit and the selected variable
    prediction <- predict(fit,prediction.test[variable])
    
    #Model build
    model <- data.frame(obs = prediction.test[variable]$Cosecha, pred = prediction)
    
    #Plotting fit if required
    if(plotting.fit == "Yes"){
      outputFile.image <- paste0(parentPath, "/", selectedMethod, "/", variables_used[index], "_fit", ".png")
     
      png(filename = outputFile.image)
      capture.output(print(plot(fit)))
      dev.off()
      
      cat("Fit plot image saved to: \"", outputFile.image, "\"","\n", sep = "")
    }
    
    #Plotting model if required
    if(plotting.model == "Yes"){
      outputFile.image <- paste0(parentPath, "/", selectedMethod, "/", variables_used[index], "_model", ".png")

      png(filename = outputFile.image)
      plot(model)
      abline(lm(model$obs ~ model$pred))
      dev.off()

      cat("Model plot image saved to: \"", outputFile.image, "\"","\n", sep = "")
    }
    
    #Build path to save results.
    outputFile.text <- paste0(parentPath, "/", selectedMethod, "/", variables_used[index],".txt")
    
    #Capture output results.
    out <- capture.output( cat(str_prediction), defaultSummary(model), cat(str_fit), fit, summary(fit), cat(str_finalModel), fit$finalModel)
    
    #Store results in a file.
    write(out, file = outputFile.text, sep = " ")
    
    cat("Results saved to: \"", outputFile.text, "\"","\n", sep = "")
    index <- index +1
    
    cat(str_separator.ln)
  }
}
cat("Finished with success.\n")

print(proc.time() - ptm)



