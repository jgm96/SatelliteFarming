library(ggplot2)
library(SnowballC)
library(psych)
require(caret)
require(RWeka)
library(dplyr)

# Start measuring time
ptm <- proc.time()

#Suppress warnings in output
options(warn=-1)

set.seed(1)
data<-read.csv("ESTIMACION\ CARGA\ FINAL_withData.csv", header=TRUE)

remove<-which((data[,"Batch"]=="Millan")|(data[,"Batch"]=="Rangoni")|(data[,"Batch"]=="Rincon"))
data<-data[-remove,]
data<-data[-c(1843,1844),]
data$Soil[data$Soil=="Arenoso "]<-"Arenoso" 
data$Soil<-factor(data$Soil)
est<-summary(data)

#String variables for outputs
str_prediction <- "\n\n========= Prediction ========\n\n"
str_separator.eq <- "======================================================\n"
str_separator.ln <- "------------------------------------------------------\n"
str_separator.hy <- "______________________________________________________\n"
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
variables <- lst(basics, basicsndvi, volumes, volumesndvi, numbersNSEW, numbersNSEWndvi, all, allndvi)

#Enter variable names to export results
variables_used <- names(variables)

#Select methods to train with
methods.used <- c("bayesglm", "M5","knn","svmRadial","gbm", "cubist")

#Indicate whether plot graphs
plotting.fit <- "Yes"
plotting.model <- "Yes"
plotting.varImp <- "Yes"

#Indicate whether is necessary to export to txt
export_txt <- "Yes"

#Indicate which measurement is the prefered
metric <- "RMSE"

#############################################################################

#Splitting data using Caret Training (75%) and test (25%).
trainIndex <- createDataPartition(data$Crop, p=.75, list = FALSE, times = 1)

data.test <- data[-trainIndex, ]
data.train <- data[trainIndex, ]

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

############### RESULTS OBTENTION ################

cat(str_separator.eq)
cat("TRAINING PROCESS\n")
cat(str_separator.eq)

#We initialize the data frame for results
results <- as.data.frame(matrix(0, ncol = length(methods.used), nrow = length(variables)), row.names = variables_used)
colnames(results) <- methods.used

#We iterate through the different methods to obtain 
methodIndex = 1
for(selectedMethod in methods.used){
  varIndex = 1
  
  cat("Selected method: ", selectedMethod, "\n")
  cat(str_separator.hy)
  for(variable in variables){
    
    #Training, capturing output to maintain the console clean for methods such as gbm.
    if(selectedMethod == "M5"){
      garbage <- capture.output(fit <- train(Crop ~., data=na.omit(data.train[variable]), method=selectedMethod,
                                             trControl = train_control,
                                             tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No"))))  
    }
    else{
      garbage <- capture.output(fit <- train(Crop ~., data=na.omit(data.train[variable]), method=selectedMethod,
                                             trControl = train_control))  
    }
    
    #Predict using fit and the selected variable
    prediction <- predict(fit,na.omit(data.test[variable]))
    
    #Model build
    model <- data.frame(obs = na.omit(data.test[variable])$Crop, pred = prediction)
    
    #Store results in results vector.
    results[[methodIndex]][[varIndex]] <- defaultSummary(model)[[metric]]
    
    #Plotting fit if required
    if(plotting.fit == "Yes"){
      outputFile.image <- paste0(parentPath, "/", selectedMethod, "/", variables_used[varIndex], "_fit", ".png")
     
      png(filename = outputFile.image)
      tryCatch({
        capture.output(print(plot(fit)))
        cat("Fit plot image saved to: \"", outputFile.image, "\"","\n", sep = "")
        }, error = function(err){
          message(paste("ERROR: ", err))
        })
      dev.off()
    }
    
    #Plotting model if required
    if(plotting.model == "Yes"){
      outputFile.image <- paste0(parentPath, "/", selectedMethod, "/", variables_used[varIndex], "_model", ".png")

      png(filename = outputFile.image)
      plot(model)
      abline(lm(model$obs ~ model$pred))
      dev.off()

      cat("Model plot image saved to: \"", outputFile.image, "\"","\n", sep = "")
    }
    
    #Plotting varImp if required
    if(plotting.varImp == "Yes"){
      outputFile.image <- paste0(parentPath, "/", selectedMethod, "/", variables_used[varIndex], "_varImp", ".png")
      
      png(filename = outputFile.image)
      tryCatch({
        capture.output(print(plot(varImp(fit))))
        cat("VarImp plot image saved to: \"", outputFile.image, "\"","\n", sep = "")
      }, error = function(err){
        message(paste("ERROR: ", err))
      })
      dev.off()
    }
    
    if(export_txt == "Yes"){
      #Build path to save results.
      outputFile.text <- paste0(parentPath, "/", selectedMethod, "/", variables_used[varIndex],".txt")
      
      #Capture output results.
      out <- capture.output( cat(str_prediction), defaultSummary(model), cat(str_fit), fit, summary(fit), cat(str_finalModel), fit$finalModel)
      
      #Store results in a file.
      write(out, file = outputFile.text, sep = " ")
      
      cat("Results saved to: \"", outputFile.text, "\"","\n", sep = "")
    }
    varIndex <- varIndex + 1
    cat(str_separator.ln)
  
  }
  methodIndex <- methodIndex +1
  
}

cat(str_separator.eq)
cat(str_resultsObtention)
cat(str_separator.eq)

#Obtain best method and best variable.
minValue <- min(results)
bestMethod <- names(results)[which(results == minValue, arr.ind = T)[,"row"]]
cat(str_separator.ln)
cat("Best method: ", bestMethod, " with a ", metric, " value of ", minValue, ".\n")

#Export results to .csv
outputFile.csv = paste0(parentPath, "/results_", metric, ".csv") 
out <- capture.output(lapply(results, function(x) write.csv(results, outputFile.csv, append= T, sep=',')))
cat(metric, "results saved to ", outputFile.csv) 

cat("Finished with success in ", (proc.time() - ptm)["elapsed"], "s.\n")




