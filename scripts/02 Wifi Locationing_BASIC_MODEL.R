#BASIC MODEL

####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools, e1071,
               randomForest)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

#Loading the data

training<- read.csv("./datasets/trainingData.csv")
validation<- read.csv("./datasets/validationData.csv")
validation$LOCATION<- validation$SPACEID*100 + validation$FLOOR*10 + validation$BUILDINGID

training2<- readRDS("./datasets/training2.rds")
# Load a model FOR BUILDING
rf_reg_caret<-readRDS("./models/RF_Model.rds")

#### 1. Sampling the data ####

Training_sample <- training2 %>% group_by(FLOOR, BUILDINGID) %>% sample_n(100)
table(Training_sample$FLOOR)
table(Training_sample$BUILDINGID)

Training_sample$BUILDINGID <- as.factor(Training_sample$BUILDINGID)

#### 2.RANDOM FOREST ####

# Saving the waps in a vector
WAPs<-grep("WAP", names(Training_sample), value=T)
# Get the best mtry
bestmtry_rf<-tuneRF(Training_sample[WAPs], Training_sample$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)

# Train a random forest using that mtry
#system.time(rf_reg<-randomForest(y=Training_sample$BUILDINGID,x=Training_sample[WAPs],importance=T,method="rf", ntree=100, mtry=bestmtry_rf[,1]))
# Train a random forest using caret package
#system.time(rf_reg_caret<-train(y=Training_sample$BUILDINGID, x=Training_sample[WAPs], data = Training_sample, method="rf", ntree=100,tuneGrid=expand.grid(.mtry=bestmtry_rf[,1])))
# Save a model CARET WITH n=200 samples best model (took 15 minutes), next one is n=200 no caret
# saveRDS(rf_reg_caret2, file="./models/RF_Model.rds")

# CHECK MODEL WITH VALIDATION

#PREDICTION
predicted_building <- predict(rf_reg_caret, validation)
#confusionMatrix
confusionMatrix(table(predicted_building, validation$BUILDINGID))

####3. KNN ####

#Initialise
set.seed(123)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv", number = 5,repeats = 2, preProc = c("center", "scale", "range"))

validation$BUILDINGID <- as.character(validation$BUILDINGID)
validation$BUILDINGID <- as.factor(validation$BUILDINGID)


#STANDARIZE
WAPs<-grep("WAP", names(Training_sample), value=T)
preprocessParams<-preProcess(Training_sample[WAPs], method=c("center", "scale"))
valid_waps<-predict(preprocessParams, validation[WAPs])
stand_waps<-predict(preprocessParams, Training_sample[WAPs])
stand_dataset<-cbind(stand_waps, BUILDINGID=Training_sample$BUILDINGID, LONGITUDE=Training_sample$LONGITUDE)

library("class")

#KNN Building
knn1 <- class::knn(train=stand_dataset[WAPs], test=valid_waps[WAPs], cl=stand_dataset$BUILDINGID, k=1 )
#Metrics
validation$BUILDINGID <- as.factor(validation$BUILDINGID)
confusionMatrix(table(knn1, validation$BUILDINGID))
postResample(knn1, validation$BUILDINGID)

#KNN Caret
system.time(knn_clasif_caret<-train(y=stand_dataset$BUILDINGID, x=stand_dataset[WAPs], data = stand_dataset, method="knn"))

#### SAVE MODEL ####

#SAVE PredictedBuilding as BuildingID

#include predicted building in validation dataset
predicted_building <- predict(rf_reg_caret, validation)
confusionMatrix(table(predicted_building, validation$BUILDINGID)) 
names(validation)[names(validation) == "BUILDINGID"] <- "Building_Original"
predicted_building<- as.data.frame(predicted_building)
validation<-cbind(validation, predicted_building)
names(validation)[names(validation) == "predicted_building"] <- "BUILDINGID"

#saveRDS(validation, file = "./datasets/validation2.rds")

# Save a model
#saveRDS(KNN_Model_Building, file="KNN_Model_Building.rds")

# Load a model
#final_model<-readRD("RF_Model.rds")