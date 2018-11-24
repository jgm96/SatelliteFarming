#library(wordcloud)
library(ggplot2)
#library(tm)
library(SnowballC)
library(psych)
require(caret)
#require(party)
#require(rpart)
#require(FSelector)
#require(rattle)
#require(rpart.plot)
#require(partykit)
require(RWeka)



set.seed(1)
datos<-read.csv("/home/jgm/Documentos/Cuarto/Cuatrimestre\ 2/TFG/ESTIMACION\ CARGA\ FINAL_withData.csv", header=TRUE)

elimina<-which((datos[,"Lote"]=="Millan")|(datos[,"Lote"]=="Rangoni")|(datos[,"Lote"]=="Rincon"))
datos<-datos[-elimina,]
datos<-datos[-c(1843,1844),]
datos$Suelo[datos$Suelo=="Arenoso "]<-"Arenoso" 
datos$Suelo<-factor(datos$Suelo)
est<-summary(datos)
#print(est)

#Variables to use
basicas<-c(8,12,16:18,23,56)
basicasndvi<-c(8,12,16:18,23,56,65,66)
volumenes<-c(8,12,16,17,18,23,57:62,56)
volumenesndvi<-c(8,12,16,17,18,23,57:62,56,65,66)
numerosNSEW<-c(8,12,16:18,23,42:45,56)
numerosNSEWndvi<-c(8,12,16:18,23,42:45,56,65,66)
todo<-c(4,5,6,8,12,16:18,23,57:62,42:46,56)
todondvi<-c(8,12,16:18,23,57:62,42:46,56,65,66)

# Old variables
# volumenes_old<-c(4,5,6,8,12,16:18,23,57:62,56)
# numerosNSEW_old<-c(8,12,16:18,23,42:45,56)
# basicas_old<-c(4,5,6,8,12,16:18,23,56)
# numerosNSEWAvg_old<-c(4,5,6,8,12,16:18,23,42:46,56)
# todosinNAvg_old<-c(4,5,6,8,12,16:18,23,57:62,42:45,56)
# todo_old<-c(8,12,16:18,23,57:62,42:46,56)
# ndvi_old<-c(4,5,6,8,12,16:18,23,57:62,42:46,56,65,66)
# todo<-c(4,5,6,8,12,16:18,23,57:62,56))

predictionbasicas <- na.omit(datos[basicas])
predictionbasicasndvi <- na.omit(datos[basicasndvi])
predictionvolumenes <- na.omit(datos[volumenes])
predictionvolumenesndvi <- na.omit(datos[volumenesndvi])
predictionnumerosNSEW <- na.omit(datos[numerosNSEW])
predictionnumerosNSEWndvi <- na.omit(datos[numerosNSEWndvi])
predictiontodo <- na.omit(datos[todo])
predictiontodondvi <- na.omit(datos[todondvi])

# Splitting data using caret


# Basicas
trainIndex.basicas <- createDataPartition(predictionbasicas$Cosecha, p=.75, list = FALSE, times = 1)

predictionbasicas.train <- predictionbasicas[trainIndex.basicas, ]
predictionbasicas.test <- predictionbasicas[-trainIndex.basicas, ]

trainIndex.basicasndvi <- createDataPartition(predictionbasicasndvi$Cosecha, p=.75, list = FALSE, times = 1)

predictionbasicasndvi.train <- predictionbasicas[trainIndex.basicasndvi, ]
predictionbasicasndvi.test <- predictionbasicas[-trainIndex.basicasndvi, ]

# Volumenes

trainIndex.volumenes <- createDataPartition(predictionbasicasndvi$Cosecha, p=.75, list = FALSE, times = 1)

predictionvolumenes.train <- predictionvolumenes[trainIndex.volumenes, ]
predictionvolumenes.test <- predictionvolumenes[-trainIndex.volumenes, ]

trainIndex.volumenesndvi <- createDataPartition(predictionbasicasndvi$Cosecha, p=.75, list = FALSE, times = 1)

predictionvolumenesndvi.train <- predictionvolumenesndvi[trainIndex.volumenesndvi, ]
predictionvolumenesndvi.test <- predictionvolumenesndvi[-trainIndex.volumenesndvi, ]

# Numeros NSEW

trainIndex.numerosNSEW <- createDataPartition(predictionnumerosNSEW$Cosecha, p=.75, list = FALSE, times = 1)

predictionnumerosNSEW.train <- predictionvolumenes[trainIndex.numerosNSEW, ]
predictionnumerosNSEW.test <- predictionvolumenes[-trainIndex.numerosNSEW, ]

trainIndex.numerosNSEWndvi <- createDataPartition(predictionnumerosNSEWndvi$Cosecha, p=.75, list = FALSE, times = 1)

predictionnumerosNSEWndvi.train <- predictionvolumenesndvi[trainIndex.numerosNSEWndvi, ]
predictionnumerosNSEWndvi.test <- predictionvolumenesndvi[-trainIndex.numerosNSEWndvi, ]

# Todo

trainIndex.todo <- createDataPartition(predictiontodo$Cosecha, p=.75, list = FALSE, times = 1)

predictiontodo.train <- predictiontodo[trainIndex.todo, ]
predictiontodo.test <- predictiontodo[-trainIndex.todo, ]

trainIndex.todondvi <- createDataPartition(predictiontodo$Cosecha, p=.75, list = FALSE, times = 1)

predictiontodondvi.train <- predictiontodo[trainIndex.todondvi, ]
predictiontodondvi.test <- predictiontodo[-trainIndex.todondvi, ]

train_control <- trainControl(method="cv", number = 10)
#train_control <- trainControl(method="none")
#train_control <- trainControl(method="repeatedcv", repeats = 3)
#train_control <- trainControl(method="adaptative_cv", number = 10)

# M5 Calculations 
# m5fit<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#                trControl = train_control)
# m5fit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#               trControl = train_control)
# m5fit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#               trControl = train_control)
# predictM5.basicas <- predict(m5fit.basicas,predictionbasicas.test)
# modelM5.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictM5.basicas)
# 
# m5fit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="M5",
#                       trControl = train_control)
# predictM5.basicasndvi <- predict(m5fit.basicas,predictionbasicasndvi.test)
# modelM5.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictM5.basicasndvi)
# 
# m5fit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="M5",
#                       trControl = train_control)
# predictM5.volumenes <- predict(m5fit.basicasndvi,predictionvolumenes.test)
# modelM5.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictM5.volumenes)
# 
# m5fit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="M5",
#                           trControl = train_control)
# predictM5.volumenesndvi <- predict(m5fit.basicasndvi,predictionvolumenesndvi.test)
# modelM5.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictM5.volumenesndvi)
# 
# m5fit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="M5",
#                       trControl = train_control)
# predictM5.numerosNSEW <- predict(m5fit.numerosNSEW,predictionnumerosNSEW.test)
# modelM5.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictM5.numerosNSEW)
# 
# m5fit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="M5",
#                           trControl = train_control)
# predictM5.numerosNSEWndvi <- predict(m5fit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
# modelM5.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictM5.numerosNSEWndvi)
# 
# m5fit.todo<-train(Cosecha ~., data=predictiontodo.train, method="M5",
#                       trControl = train_control)
# predictM5.todo <- predict(m5fit.todo,predictiontodo.test)
# modelM5.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictM5.todo)
# 
# m5fit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="M5",
#                           trControl = train_control)
# predictM5.todondvi <- predict(m5fit.todondvi ,predictiontodondvi.test)
# modelM5.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictM5.todondvi)
# m5fit<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#               tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# m5fit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#              tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# m5fit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="M5",
#              tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.basicas <- predict(m5fit.basicas,predictionbasicas.test)
# modelM5.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictM5.basicas)
# 
# m5fit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="M5",
#                      tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.basicasndvi <- predict(m5fit.basicas,predictionbasicasndvi.test)
# modelM5.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictM5.basicasndvi)
# 
# m5fit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="M5",
#                      tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.volumenes <- predict(m5fit.basicasndvi,predictionvolumenes.test)
# modelM5.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictM5.volumenes)
# 
# m5fit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="M5",
#                          tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.volumenesndvi <- predict(m5fit.basicasndvi,predictionvolumenesndvi.test)
# modelM5.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictM5.volumenesndvi)
# 
# m5fit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="M5",
#                      tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.numerosNSEW <- predict(m5fit.numerosNSEW,predictionnumerosNSEW.test)
# modelM5.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictM5.numerosNSEW)
# 
# m5fit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="M5",
#                          tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.numerosNSEWndvi <- predict(m5fit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
# modelM5.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictM5.numerosNSEWndvi)
# 
# m5fit.todo<-train(Cosecha ~., data=predictiontodo.train, method="M5",
#                      tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.todo <- predict(m5fit.todo,predictiontodo.test)
# modelM5.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictM5.todo)
# 
# m5fit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="M5",
#                          tuneGrid = expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("Yes","No")), trControl = train_control)
# predictM5.todondvi <- predict(m5fit.todondvi ,predictiontodondvi.test)
# modelM5.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictM5.todondvi)
# # M5 Output capture
# 
# out <- capture.output(print(m5fit.basicas), summary(m5fit.basicas), varImp(m5fit.basicas), m5fit.basicas$finalModel, defaultSummary(modelM5.basicas))
# write(out, file = "CrossValidated/M5/M5basicas.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.basicasndvi), summary(m5fit.basicasndvi), varImp(m5fit.basicasndvi), m5fit.basicasndvi$finalModel, defaultSummary(modelM5.basicasndvi))
# write(out, file = "CrossValidated/M5/M5basicasndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.volumenes), summary(m5fit.volumenes), varImp(m5fit.volumenes), m5fit.volumenes$finalModel, defaultSummary(modelM5.volumenes))
# write(out, file = "CrossValidated/M5/M5volumenes.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.volumenesndvi), summary(m5fit.volumenesndvi), varImp(m5fit.volumenesndvi), m5fit.volumenesndvi$finalModel, defaultSummary(modelM5.volumenesndvi))
# write(out, file = "CrossValidated/M5/M5volumenesndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.numerosNSEW), summary(m5fit.numerosNSEW), varImp(m5fit.numerosNSEW), m5fit.numerosNSEW$finalModel, defaultSummary(modelM5.numerosNSEW))
# write(out, file = "CrossValidated/M5/M5numerosNSEW.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.numerosNSEWndvi), summary(m5fit.numerosNSEWndvi), varImp(m5fit.numerosNSEWndvi), m5fit.numerosNSEWndvi$finalModel, defaultSummary(modelM5.numerosNSEWndvi))
# write(out, file = "CrossValidated/M5/M5numerosNSEWndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.todo), summary(m5fit.todo), varImp(m5fit.todo), m5fit.todo$finalModel, defaultSummary(modelM5.todo))
# write(out, file = "CrossValidated/M5/M5todo.txt",
#       sep = " ")
# 
# out <- capture.output(print(m5fit.todondvi), summary(m5fit.todondvi), varImp(m5fit.todondvi), m5fit.todondvi$finalModel, defaultSummary(modelM5.todondvi))
# write(out, file = "CrossValidated/M5/M5todondvi.txt",
#       sep = " ")


#K-NN calculations
# knnfit<-train(Cosecha ~., data=predictionbasicas.train, method="knn",
#                       trControl = train_control)
# knnfit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="knn",
#                       trControl = train_control)
# predictknn.basicas <- predict(knnfit.basicas,predictionbasicas.test)
# modelknn.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictknn.basicas)
# 
# knnfit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="knn",
#                           trControl = train_control)
# predictknn.basicasndvi <- predict(knnfit.basicasndvi,predictionbasicasndvi.test)
# modelknn.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictknn.basicasndvi)
# 
# knnfit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="knn",
#                         trControl = train_control)
# predictknn.volumenes <- predict(knnfit.volumenes,predictionvolumenes.test)
# modelknn.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictknn.volumenes)
# 
# knnfit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="knn",
#                             trControl = train_control)
# predictknn.volumenesndvi <- predict(knnfit.volumenesndvi,predictionvolumenesndvi.test)
# modelknn.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictknn.volumenesndvi)
# 
# knnfit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="knn",
#                           trControl = train_control)
# predictknn.numerosNSEW <- predict(knnfit.numerosNSEW,predictionnumerosNSEW.test)
# modelknn.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictknn.numerosNSEW)
# 
# knnfit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="knn",
#                               trControl = train_control)
# predictknn.numerosNSEWndvi <- predict(knnfit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
# modelknn.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictknn.numerosNSEWndvi)
# 
# knnfit.todo<-train(Cosecha ~., data=predictiontodo.train, method="knn",
#                    trControl = train_control)
# predictknn.todo <- predict(knnfit.todo,predictiontodo.test)
# modelknn.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictknn.todo)
# 
# knnfit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="knn",
#                        trControl = train_control)
# predictknn.todondvi <- predict(knnfit.todondvi,predictiontodondvi.test)
# modelknn.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictknn.todondvi)
# 
# # K-NN Output capture
# 
# out <- capture.output(print(knnfit.basicas), summary(knnfit.basicas), varImp(knnfit.basicas), knnfit.basicas$finalModel, defaultSummary(modelknn.basicas))
# write(out, file = "CrossValidated/KNN/knnbasicas.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.basicasndvi), summary(knnfit.basicasndvi), varImp(knnfit.basicasndvi), knnfit.basicasndvi$finalModel, defaultSummary(modelknn.basicasndvi))
# write(out, file = "CrossValidated/KNN/knnbasicasndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.volumenes), summary(knnfit.volumenes), varImp(knnfit.volumenes), knnfit.volumenes$finalModel, defaultSummary(modelknn.volumenes))
# write(out, file = "CrossValidated/KNN/knnvolumenes.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.volumenesndvi), summary(knnfit.volumenesndvi), varImp(knnfit.volumenesndvi), knnfit.volumenesndvi$finalModel, defaultSummary(modelknn.volumenesndvi))
# write(out, file = "CrossValidated/KNN/knnvolumenesndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.numerosNSEW), summary(knnfit.numerosNSEW), varImp(knnfit.numerosNSEW), knnfit.numerosNSEW$finalModel, defaultSummary(modelknn.numerosNSEW))
# write(out, file = "CrossValidated/KNN/knnnumerosNSEW.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.numerosNSEWndvi), summary(knnfit.numerosNSEWndvi), varImp(knnfit.numerosNSEWndvi), knnfit.numerosNSEWndvi$finalModel, defaultSummary(modelknn.numerosNSEWndvi))
# write(out, file = "CrossValidated/KNN/knnnumerosNSEWndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.todo), summary(knnfit.todo), varImp(knnfit.todo), knnfit.todo$finalModel, defaultSummary(modelknn.todo))
# write(out, file = "CrossValidated/KNN/knntodo.txt",
#       sep = " ")
# 
# out <- capture.output(print(knnfit.todondvi), summary(knnfit.todondvi), varImp(knnfit.todondvi), knnfit.todondvi$finalModel, defaultSummary(modelknn.todondvi))
# write(out, file = "CrossValidated/KNN/knntodondvi.txt",
#       sep = " ")

# SVM 
svmfit<-train(Cosecha ~., data=predictionbasicas.train, method="svmRadial",
                      trControl = train_control)
svmfit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="svmRadial",
                      trControl = train_control)
predictsvm.basicas <- predict(svmfit.basicas,predictionbasicas.test)
modelsvm.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictsvm.basicas)

svmfit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="svmRadial",
                          trControl = train_control)
predictsvm.basicasndvi <- predict(svmfit.basicasndvi,predictionbasicasndvi.test)
modelsvm.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictsvm.basicasndvi)

svmfit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="svmRadial",
                        trControl = train_control)
predictsvm.volumenes <- predict(svmfit.volumenes,predictionvolumenes.test)
modelsvm.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictsvm.volumenes)

svmfit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="svmRadial",
                            trControl = train_control)
predictsvm.volumenesndvi <- predict(svmfit.volumenesndvi,predictionvolumenesndvi.test)
modelsvm.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictsvm.volumenesndvi)

svmfit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="svmRadial",
                          trControl = train_control)
predictsvm.numerosNSEW <- predict(svmfit.numerosNSEW,predictionnumerosNSEW.test)
modelsvm.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictsvm.numerosNSEW)

svmfit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="svmRadial",
                              trControl = train_control)
predictsvm.numerosNSEWndvi <- predict(svmfit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
modelsvm.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictsvm.numerosNSEWndvi)

svmfit.todo<-train(Cosecha ~., data=predictiontodo.train, method="svmRadial",
                   trControl = train_control)
predictsvm.todo <- predict(svmfit.todo,predictiontodo.test)
modelsvm.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictsvm.todo)

svmfit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="svmRadial",
                       trControl = train_control)
predictsvm.todondvi <- predict(svmfit.todondvi,predictiontodondvi.test)
modelsvm.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictsvm.todondvi)

# SVM Output capture

out <- capture.output(print(svmfit.basicas), summary(svmfit.basicas), varImp(svmfit.basicas), svmfit.basicas$finalModel, defaultSummary(modelsvm.basicas))
write(out, file = "CrossValidated/SVM/SVMbasicas.txt",
      sep = " ")

out <- capture.output(print(svmfit.basicasndvi), summary(svmfit.basicasndvi), varImp(svmfit.basicasndvi), svmfit.basicasndvi$finalModel, defaultSummary(modelsvm.basicasndvi))
write(out, file = "CrossValidated/SVM/svmbasicasndvi.txt",
      sep = " ")

out <- capture.output(print(svmfit.volumenes), summary(svmfit.volumenes), varImp(svmfit.volumenes), svmfit.volumenes$finalModel, defaultSummary(modelsvm.volumenes))
write(out, file = "CrossValidated/SVM/svmvolumenes.txt",
      sep = " ")

out <- capture.output(print(svmfit.volumenesndvi), summary(svmfit.volumenesndvi), varImp(svmfit.volumenesndvi), svmfit.volumenesndvi$finalModel, defaultSummary(modelsvm.volumenesndvi))
write(out, file = "CrossValidated/SVM/svmvolumenesndvi.txt",
      sep = " ")

out <- capture.output(print(svmfit.numerosNSEW), summary(svmfit.numerosNSEW), varImp(svmfit.numerosNSEW), svmfit.numerosNSEW$finalModel, defaultSummary(modelsvm.numerosNSEW))
write(out, file = "CrossValidated/SVM/svmnumerosNSEW.txt",
      sep = " ")

out <- capture.output(print(svmfit.numerosNSEWndvi), summary(svmfit.numerosNSEWndvi), varImp(svmfit.numerosNSEWndvi), svmfit.numerosNSEWndvi$finalModel, defaultSummary(modelsvm.numerosNSEWndvi))
write(out, file = "CrossValidated/SVM/svmnumerosNSEWndvi.txt",
      sep = " ")

out <- capture.output(print(svmfit.todo), summary(svmfit.todo), varImp(svmfit.todo), svmfit.todo$finalModel, defaultSummary(modelsvm.todo))
write(out, file = "CrossValidated/SVM/svmtodo.txt",
      sep = " ")

out <- capture.output(print(svmfit.todondvi), summary(svmfit.todondvi), varImp(svmfit.todondvi), svmfit.todondvi$finalModel, defaultSummary(modelsvm.todondvi))
write(out, file = "CrossValidated/SVM/svmtodondvi.txt",
      sep = " ")

# Cubist
# cubistfit<-train(Cosecha ~., data=predictionbasicas.train, method="cubist",
#               trControl = train_control)
# cubistfit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="cubist",
#                       trControl = train_control)
# predictcubist.basicas <- predict(cubistfit.basicas,predictionbasicas.test)
# modelcubist.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictcubist.basicas)
# 
# cubistfit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="cubist",
#                           trControl = train_control)
# predictcubist.basicasndvi <- predict(cubistfit.basicasndvi,predictionbasicasndvi.test)
# modelcubist.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictcubist.basicasndvi)
# 
# cubistfit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="cubist",
#                         trControl = train_control)
# predictcubist.volumenes <- predict(cubistfit.volumenes,predictionvolumenes.test)
# modelcubist.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictcubist.volumenes)
# 
# cubistfit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="cubist",
#                             trControl = train_control)
# predictcubist.volumenesndvi <- predict(cubistfit.volumenesndvi,predictionvolumenesndvi.test)
# modelcubist.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictcubist.volumenesndvi)
# 
# cubistfit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="cubist",
#                           trControl = train_control)
# predictcubist.numerosNSEW <- predict(cubistfit.numerosNSEW,predictionnumerosNSEW.test)
# modelcubist.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictcubist.numerosNSEW)
# 
# cubistfit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="cubist",
#                               trControl = train_control)
# predictcubist.numerosNSEWndvi <- predict(cubistfit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
# modelcubist.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictcubist.numerosNSEWndvi)
# 
# cubistfit.todo<-train(Cosecha ~., data=predictiontodo.train, method="cubist",
#                    trControl = train_control)
# predictcubist.todo <- predict(cubistfit.todo,predictiontodo.test)
# modelcubist.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictcubist.todo)
# 
# cubistfit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="cubist",
#                        trControl = train_control)
# predictcubist.todondvi <- predict(cubistfit.todondvi,predictiontodondvi.test)
# modelcubist.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictcubist.todondvi)
# 
# # Cubist Output capture
# 
# out <- capture.output(print(cubistfit.basicas), summary(cubistfit.basicas), varImp(cubistfit.basicas), cubistfit.basicas$finalModel, defaultSummary(modelcubist.basicas))
# write(out, file = "CrossValidated/cubist/cubistbasicas.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.basicasndvi), summary(cubistfit.basicasndvi), varImp(cubistfit.basicasndvi), cubistfit.basicasndvi$finalModel, defaultSummary(modelcubist.basicasndvi))
# write(out, file = "CrossValidated/cubist/cubistbasicasndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.volumenes), summary(cubistfit.volumenes), varImp(cubistfit.volumenes), cubistfit.volumenes$finalModel, defaultSummary(modelcubist.volumenes))
# write(out, file = "CrossValidated/cubist/cubistvolumenes.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.volumenesndvi), summary(cubistfit.volumenesndvi), varImp(cubistfit.volumenesndvi), cubistfit.volumenesndvi$finalModel, defaultSummary(modelcubist.volumenesndvi))
# write(out, file = "CrossValidated/cubist/cubistvolumenesndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.numerosNSEW), summary(cubistfit.numerosNSEW), varImp(cubistfit.numerosNSEW), cubistfit.numerosNSEW$finalModel, defaultSummary(modelcubist.numerosNSEW))
# write(out, file = "CrossValidated/cubist/cubistnumerosNSEW.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.numerosNSEWndvi), summary(cubistfit.numerosNSEWndvi), varImp(cubistfit.numerosNSEWndvi), cubistfit.numerosNSEWndvi$finalModel, defaultSummary(modelcubist.numerosNSEWndvi))
# write(out, file = "CrossValidated/cubist/cubistnumerosNSEWndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.todo), summary(cubistfit.todo), varImp(cubistfit.todo), cubistfit.todo$finalModel, defaultSummary(modelcubist.todo))
# write(out, file = "CrossValidated/cubist/cubisttodo.txt",
#       sep = " ")
# 
# out <- capture.output(print(cubistfit.todondvi), summary(cubistfit.todondvi), varImp(cubistfit.todondvi), cubistfit.todondvi$finalModel, defaultSummary(modelcubist.todondvi))
# write(out, file = "CrossValidated/cubist/cubisttodondvi.txt",
#       sep = " ")

# GBM
# 
# gbmfit<-train(Cosecha ~., data=predictionbasicas.train, method="gbm",
#                  trControl = train_control)
# gbmfit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="gbm",
#                          trControl = train_control)
# predictgbm.basicas <- predict(gbmfit.basicas,predictionbasicas.test)
# modelgbm.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictgbm.basicas)
# 
# gbmfit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="gbm",
#                              trControl = train_control)
# predictgbm.basicasndvi <- predict(gbmfit.basicasndvi,predictionbasicasndvi.test)
# modelgbm.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictgbm.basicasndvi)
# 
# gbmfit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="gbm",
#                            trControl = train_control)
# predictgbm.volumenes <- predict(gbmfit.volumenes,predictionvolumenes.test)
# modelgbm.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictgbm.volumenes)
# 
# gbmfit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="gbm",
#                                trControl = train_control)
# predictgbm.volumenesndvi <- predict(gbmfit.volumenesndvi,predictionvolumenesndvi.test)
# modelgbm.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictgbm.volumenesndvi)
# 
# gbmfit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="gbm",
#                              trControl = train_control)
# predictgbm.numerosNSEW <- predict(gbmfit.numerosNSEW,predictionnumerosNSEW.test)
# modelgbm.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictgbm.numerosNSEW)
# 
# gbmfit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="gbm",
#                                  trControl = train_control)
# predictgbm.numerosNSEWndvi <- predict(gbmfit.numerosNSEWndvi,predictionnumerosNSEWndvi.test)
# modelgbm.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictgbm.numerosNSEWndvi)
# 
# gbmfit.todo<-train(Cosecha ~., data=predictiontodo.train, method="gbm",
#                       trControl = train_control)
# predictgbm.todo <- predict(gbmfit.todo,predictiontodo.test)
# modelgbm.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictgbm.todo)
# 
# gbmfit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="gbm",
#                           trControl = train_control)
# predictgbm.todondvi <- predict(gbmfit.todondvi,predictiontodondvi.test)
# modelgbm.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictgbm.todondvi)
# 
# # GBM Output capture
# 
# out <- capture.output(print(gbmfit.basicas), summary(gbmfit.basicas), gbmfit.basicas$finalModel, defaultSummary(modelgbm.basicas))
# write(out, file = "CrossValidated/gbm/gbmbasicas.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.basicasndvi), summary(gbmfit.basicasndvi), gbmfit.basicasndvi$finalModel, defaultSummary(modelgbm.basicasndvi))
# write(out, file = "CrossValidated/gbm/gbmbasicasndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.volumenes), summary(gbmfit.volumenes), gbmfit.volumenes$finalModel, defaultSummary(modelgbm.volumenes))
# write(out, file = "CrossValidated/gbm/gbmvolumenes.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.volumenesndvi), summary(gbmfit.volumenesndvi), gbmfit.volumenesndvi$finalModel, defaultSummary(modelgbm.volumenesndvi))
# write(out, file = "CrossValidated/gbm/gbmvolumenesndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.numerosNSEW), summary(gbmfit.numerosNSEW), gbmfit.numerosNSEW$finalModel, defaultSummary(modelgbm.numerosNSEW))
# write(out, file = "CrossValidated/gbm/gbmnumerosNSEW.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.numerosNSEWndvi), summary(gbmfit.numerosNSEWndvi), gbmfit.numerosNSEWndvi$finalModel, defaultSummary(modelgbm.numerosNSEWndvi))
# write(out, file = "CrossValidated/gbm/gbmnumerosNSEWndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.todo), summary(gbmfit.todo), gbmfit.todo$finalModel, defaultSummary(modelgbm.todo))
# write(out, file = "CrossValidated/gbm/gbmtodo.txt",
#       sep = " ")
# 
# out <- capture.output(print(gbmfit.todondvi), summary(gbmfit.todondvi), gbmfit.todondvi$finalModel, defaultSummary(modelgbm.todondvi))
# write(out, file = "CrossValidated/gbm/gbmtodondvi.txt",
#       sep = " ")

# BRNN 

# brnnfit<-train(Cosecha ~., data=predictionbasicas.train, method="brnn",
#               trControl = train_control)
# brnnfit.basicas<-train(Cosecha ~., data=predictionbasicas.train, method="brnn",
#                       trControl = train_control)
# predictbrnn.basicas <- predict(brnnfit.basicas,predictionbasicas.test)
# modelbrnn.basicas <- data.frame(obs = predictionbasicas.test$Cosecha, pred = predictbrnn.basicas)
# 
# brnnfit.basicasndvi<-train(Cosecha ~., data=predictionbasicasndvi.train, method="brnn",
#                           trControl = train_control)
# predictbrnn.basicasndvi <- predict(brnnfit.basicasndvi,predictionbasicasndvi.test)
# modelbrnn.basicasndvi <- data.frame(obs = predictionbasicasndvi.test$Cosecha, pred = predictbrnn.basicasndvi)
# 
# brnnfit.volumenes<-train(Cosecha ~., data=predictionvolumenes.train, method="brnn",
#                         trControl = train_control)
# predictbrnn.volumenes <- predict(brnnfit.volumenes,predictionvolumenes.test)
# modelbrnn.volumenes <- data.frame(obs = predictionvolumenes.test$Cosecha, pred = predictbrnn.volumenes)
# 
# brnnfit.volumenesndvi<-train(Cosecha ~., data=predictionvolumenesndvi.train, method="brnn",
#                             trControl = train_control)
# predictbrnn.volumenesndvi <- predict(brnnfit.volumenesndvi,predictionvolumenesndvi.test)
# modelbrnn.volumenesndvi <- data.frame(obs = predictionvolumenesndvi.test$Cosecha, pred = predictbrnn.volumenesndvi)
# 
# brnnfit.numerosNSEW<-train(Cosecha ~., data=predictionnumerosNSEW.train, method="brnn",
#                           trControl = train_control)
# predictbrnn.numerosNSEW <- predict(brnnfit.numerosNSEW,predictionnumerosNSEW.test)
# modelbrnn.numerosNSEW <- data.frame(obs = predictionnumerosNSEW.test$Cosecha, pred = predictbrnn.numerosNSEW)
# 
# brnnfit.numerosNSEWndvi<-train(Cosecha ~., data=predictionnumerosNSEWndvi.train, method="brnn",
#                               trControl = train_control)
# predictbrnn.numerosNSEWndvi <- predict(brnnfit.numerosNSEWndvi, predictionnumerosNSEWndvi.test)
# modelbrnn.numerosNSEWndvi <- data.frame(obs = predictionnumerosNSEWndvi.test$Cosecha, pred = predictbrnn.numerosNSEWndvi)
# 
# brnnfit.todo<-train(Cosecha ~., data=predictiontodo.train, method="brnn",
#                    trControl = train_control)
# predictbrnn.todo <- predict(brnnfit.todo,predictiontodo.test)
# modelbrnn.todo <- data.frame(obs = predictiontodo.test$Cosecha, pred = predictbrnn.todo)
# 
# brnnfit.todondvi<-train(Cosecha ~., data=predictiontodondvi.train, method="brnn",
#                        trControl = train_control)
# predictbrnn.todondvi <- predict(brnnfit.todondvi,predictiontodondvi.test)
# modelbrnn.todondvi <- data.frame(obs = predictiontodondvi.test$Cosecha, pred = predictbrnn.todondvi)
# 
# # BRNN Output capture
# 
# out <- capture.output(print(brnnfit.basicas), summary(brnnfit.basicas), brnnfit.basicas$finalModel, defaultSummary(modelbrnn.basicas))
# write(out, file = "CrossValidated/brnn/brnnbasicas.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.basicasndvi), summary(brnnfit.basicasndvi), brnnfit.basicasndvi$finalModel, defaultSummary(modelbrnn.basicasndvi))
# write(out, file = "CrossValidated/brnn/brnnbasicasndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.volumenes), summary(brnnfit.volumenes), brnnfit.volumenes$finalModel, defaultSummary(modelbrnn.volumenes))
# write(out, file = "CrossValidated/brnn/brnnvolumenes.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.volumenesndvi), summary(brnnfit.volumenesndvi), brnnfit.volumenesndvi$finalModel, defaultSummary(modelbrnn.volumenesndvi))
# write(out, file = "CrossValidated/brnn/brnnvolumenesndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.numerosNSEW), summary(brnnfit.numerosNSEW), brnnfit.numerosNSEW$finalModel, defaultSummary(modelbrnn.numerosNSEW))
# write(out, file = "CrossValidated/brnn/brnnnumerosNSEW.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.numerosNSEWndvi), summary(brnnfit.numerosNSEWndvi), brnnfit.numerosNSEWndvi$finalModel, defaultSummary(modelbrnn.numerosNSEWndvi))
# write(out, file = "CrossValidated/brnn/brnnnumerosNSEWndvi.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.todo), summary(brnnfit.todo), brnnfit.todo$finalModel, defaultSummary(modelbrnn.todo))
# write(out, file = "CrossValidated/brnn/brnntodo.txt",
#       sep = " ")
# 
# out <- capture.output(print(brnnfit.todondvi), summary(brnnfit.todondvi), brnnfit.todondvi$finalModel, defaultSummary(modelbrnn.todondvi))
# write(out, file = "CrossValidated/brnn/brnntodondvi.txt",
#       sep = " ")
