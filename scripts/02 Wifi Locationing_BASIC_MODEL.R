#BASIC MODEL

####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools, e1071)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

#Loading the data

#training<- read.csv("./datasets/trainingData.csv")
#validation<- read.csv("./datasets/validationData.csv")

training2<- readRDS("./datasets/training2.rds")
# Load a model
rf_reg_caret<-readRD("./models/RF_Model.rds")

#### 1. Sampling the data ####

Training_sample <- training2 %>% group_by(FLOOR, BUILDINGID) %>% sample_n(100)
table(Training_sample$FLOOR)
table(Training_sample$BUILDINGID)

Training_sample$BUILDINGID <- as.character(Training_sample$BUILDINGID)


#### 2.RANDOM FOREST Model based on Sample ####

# Saving the waps in a vector
WAPs<-grep("WAP", names(Training_sample), value=T)
# Get the best mtry
bestmtry_rf<-tuneRF(Training_sample[WAPs], Training_sample$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Train a random forest using that mtry
system.time(rf_reg<-randomForest(y=Training_sample$BUILDINGID,x=Training_sample[WAPs],importance=T,method="rf", ntree=100, mtry=6))
# Train a random forest using caret package
#system.time(rf_reg_caret<-train(y=Training_sample$BUILDINGID, x=Training_sample[WAPs], data = Training_sample, method="rf", ntree=100,tuneGrid=expand.grid(.mtry=6)))
# Save a model CARET WITH n=200 samples best model (took 15 minutes), next one is n=200 no caret 
#saveRDS(rf_reg_caret, file="./models/RF_Model.rds")

# CHECK MODEL WITH VALIDATION 

#PREDICTION
predicted_building <- predict(rf_reg, validation)
predicted_building <- predict(rf_reg_caret, validation)


#confusionMatrix
confusionMatrix(table(predicted_building, validation$BUILDINGID))

####3. SVM for BUILDING####
# Load the packages


# Saving the waps in a vector
WAPs<-grep("WAP", names(Training_sample), value=T)

# Train two classification svm (with svm and train)
system.time(svm_clasif_caret<-train(y=Training_sample$BUILDINGID, x=Training_sample[WAPs], data = Training_sample, method="svmLinear"))
# Train two regression svm (with svm and train)
system.time(svm_reg <- svm(y = training$LONGITUDE, x=training[WAPs]))

system.time(svm_reg_caret<-train(y=stand_dataset$LONGITUDE, x=as.matrix(stand_dataset[WAPs], data = stand_dataset, method="svmLinear")))

#PREDICTION
predicted_building <- predict(svm_reg, validation)
#confusionMatrix
confusionMatrix(table(predicted_building, validation$BUILDINGID))


#### SAVE MODEL ####

# Save a model
#saveRDS(RF_Model, file="RF_Model.rds")

# Load a model
#final_model<-readRD("RF_Model.rds")