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

#Training_sample$BUILDINGID <- as.character(Training_sample$BUILDINGID)


#### 2.BULIDING (RANDOM FOREST) ####

# Saving the waps in a vector
# WAPs<-grep("WAP", names(Training_sample), value=T)
# Get the best mtry
# bestmtry_rf<-tuneRF(Training_sample[WAPs], Training_sample$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)

# Train a random forest using that mtry
# system.time(rf_reg<-randomForest(y=Training_sample$BUILDINGID,x=Training_sample[WAPs],importance=T,method="rf", ntree=100, mtry=6))
# Train a random forest using caret package
# system.time(rf_reg_caret<-train(y=Training_sample$BUILDINGID, x=Training_sample[WAPs], data = Training_sample, method="rf", ntree=100,tuneGrid=expand.grid(.mtry=6)))
# Save a model CARET WITH n=200 samples best model (took 15 minutes), next one is n=200 no caret
# saveRDS(rf_reg_caret, file="./models/RF_Model.rds")

# CHECK MODEL WITH VALIDATION

#PREDICTION
predicted_building <- predict(rf_reg_caret, validation)
#confusionMatrix
confusionMatrix(table(predicted_building, validation$BUILDINGID))

####3. FLOOR ####

#include predicted building in validation dataset
names(validation)[names(validation) == "BUILDINGID"] <- "Building_Original"
predicted_building<- as.data.frame(predicted_building)
validation<-cbind(validation, predicted_building)
names(validation)[names(validation) == "predicted_building"] <- "BUILDINGID"

#Group by building
building0<- training2 %>% filter(BUILDINGID==0) %>% group_by(SPACEID) %>% sample_n(19) #78 Rooms 4Floors
building1<- training2 %>% filter(BUILDINGID==1) %>% group_by(SPACEID) %>% sample_n(10) #86 Rooms 4Floors
building2<- training2 %>% filter(BUILDINGID==2) %>% group_by(SPACEID) %>% sample_n(10) #97 Rooms 5Floors

validation0 <- validation %>% filter(BUILDINGID==0)

#check metrics of each building
#how many rooms, floors and samples per room
training2 %>% group_by(BUILDINGID) %>% summarize(rooms=length(unique(SPACEID)))
training2 %>% group_by(BUILDINGID) %>% summarize(rooms=length(unique(FLOOR)))
room_samples<-training2 %>% group_by(BUILDINGID, SPACEID) %>% summarize(samples=n())

aa20<-room_samples %>% filter(samples<=20)


#PREDICTION
set.seed(123)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv", number = 5,repeats = 2, preProc = c("center", "scale", "range"))

## Building 0 Floor Prediction
kNNFloor0 <- train(FLOOR ~ .-LONGITUDE-LATITUDE-BUILDINGID-SPACEID-RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-LOCATION, data=building0, method = "knn",trControl = kNNcontrol, preProcess = "zv")

validation0$BUILDINGID <- as.character(validation0$BUILDINGID)
validation0$BUILDINGID <- as.numeric(validation0$BUILDINGID)

predkNN0Floor <- predict(kNNFloor0, validation0)

postResample(predkNN0Floor, validation0$FLOOR) -> kNN0FloorMetrics



table(predkNN0Floor, validation0$FLOOR)


#### SAVE MODEL ####

# Save a model
#saveRDS(RF_Model, file="RF_Model.rds")

# Load a model
#final_model<-readRD("RF_Model.rds")