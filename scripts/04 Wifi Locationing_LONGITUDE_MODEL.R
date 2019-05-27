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

#training_original<- read.csv("./datasets/trainingData.csv")
#validation_original<- read.csv("./datasets/validationData.csv")

train<- readRDS("./datasets/training2.rds")
validation<- readRDS("./datasets/validation2.rds")

# Load MODELS 
rf_reg_caret<-readRDS("./models/RF_Model.rds")
Building0_Model_GBM<-readRDS("./models/GBM_Floor_Building0.rds")

#LOCATION Variable
validation$LOCATION<- validation$SPACEID*100 + validation$FLOOR*10 + validation$BUILDINGID

#### 1. Sampling the data ####

Training_sample <- training2 %>% group_by(FLOOR, BUILDINGID) %>% sample_n(20)
Training_sample$BUILDINGID<-as.factor(Training_sample$BUILDINGID)
validation$BUILDINGID<-as.factor(Training_sample$BUILDINGID)


#### 2.BULIDING (RANDOM FOREST) ####

WAPs<-grep("WAP", names(Training_sample), value=T)
bestmtry_rf<-tuneRF(Training_sample[WAPs], Training_sample$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)
bestmtry_rf[,1]
RF<-randomForest(y=Training_sample$BUILDINGID,x=Training_sample[WAPs],importance=T,method="rf", ntree=100, mtry=bestmtry_rf[,1])
prediction<-predict(RF, validation)

prediction<-as.factor(prediction)
confusionMatrix( table(prediction, validation$BUILDINGID))

##### KNN Longitude #####
library(caret)
WAPs<-grep("WAP", names(Training_sample), value=T)
preprocessParams<-preProcess(Training_sample[WAPs], method=c("center", "scale"))

valid_waps<-predict(preprocessParams, validation[WAPs])
stand_waps<-predict(preprocessParams, Training_sample[WAPs])

stand_dataset<-cbind(stand_waps, BUILDINGID=Training_sample$BUILDINGID, LONGITUDE=Training_sample$LONGITUDE)
                     
# Train two classification knn (with knn3 and train)

stand_dataset$BUILDINGID<-as.factor(stand_dataset$BUILDINGID)

system.time(knn_clasif <- knn(BUILDINGID ~ as.matrix(stand_dataset[WAPs]), data = stand_dataset))

prediction<-predict(knn_clasif, validation)

library("class")

#KNN Building
knn1 <- class::knn(train=stand_dataset[WAPs], test=valid_waps[WAPs], cl=stand_dataset$BUILDINGID, k=1 )

confusionMatrix(knn1, validation$BUILDINGID)
system.time(knn_clasif_caret<-train(y=stand_dataset$BUILDINGID, x=stand_dataset[WAPs], data = stand_dataset, method="knn"))

#KNN Longitude
knn_reg<-knnreg(x=as.matrix(stand_dataset[WAPs]), y=stand_dataset$LONGITUDE, k=1)
knn_predict<-predict(knn_reg, valid_waps)
postResample( knn_predict, validation$LONGITUDE)


