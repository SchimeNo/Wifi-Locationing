#BASIC MODEL

####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools, e1071,
               randomForest, h2o)
h2o.init( )

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

#### Subsetting####

building0<- train %>% filter(BUILDINGID==0) %>% mutate( FLOOR=as.character(FLOOR))   #78 Rooms 4Floors
building1<- train %>% filter(BUILDINGID==1) %>% mutate( FLOOR=as.character(FLOOR))
building2<- train %>% filter(BUILDINGID==2) %>% mutate( FLOOR=as.character(FLOOR))

validation0 <- validation %>% filter(BUILDINGID==0) %>%  mutate( FLOOR=as.character(FLOOR))
validation1 <- validation %>% filter(BUILDINGID==1)%>%  mutate( FLOOR=as.character(FLOOR))
validation2 <- validation %>% filter(BUILDINGID==2)%>%  mutate( FLOOR=as.character(FLOOR))

building0 <- building0 %>% mutate(FLOOR=as.factor(FLOOR))
building1 <- building1 %>% mutate(FLOOR=as.factor(FLOOR))
building2 <- building2 %>% mutate(FLOOR=as.factor(FLOOR))

validation0 <- validation0 %>% mutate(FLOOR=as.factor(FLOOR))
validation1 <- validation1 %>% mutate(FLOOR=as.factor(FLOOR))
validation2 <- validation2 %>% mutate(FLOOR=as.factor(FLOOR))

##### STANDARIZE #####

Training_sample <- building0
validation<- validation0

library(caret)
WAPs<-grep("WAP", names(Training_sample), value=T)
preprocessParams<-preProcess(Training_sample[WAPs], method=c("center", "scale"))

valid_waps<-predict(preprocessParams, validation[WAPs])
stand_waps<-predict(preprocessParams, Training_sample[WAPs])

stand_dataset<-cbind(stand_waps, BUILDINGID=Training_sample$BUILDINGID, LONGITUDE=Training_sample$LONGITUDE, FLOOR=Training_sample$FLOOR)
                     
####KNN####

library("class")

#KNN Longitude
system.time(knn_reg<-knnreg(x=as.matrix(stand_dataset[WAPs]), y=stand_dataset$LONGITUDE, k=1))
system.time(knn_predict<-predict(knn_reg, valid_waps))
postResample( knn_predict, validation$LONGITUDE)

#saveRDS(knn_reg, file="./models/KNN_Building0_LONGITUDE.rds")

####H2O RANDOM FOREST####

#SELECT BUILDING
train.h2o <- as.h2o(building0)
test.h2o <- as.h2o(validation0)

#dependent variable (LONGITUDE)
y.dep <- 1
#independent variables (WAPS)
x.indep <- c(3,11:452)

#Random Forest#
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, stopping_rounds = 2,     
                                              ntrees = 1000, mtries = -1, max_depth = 15, seed = 1122, min_rows = 10))
h2o.performance(rforest.model)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
postResample(predict.rforest, validation2$LONGITUDE)
h2o.saveModel(object=rforest.model, path=paste0(getwd(),"/", "models"), force=TRUE)

####SVM####
# Load the packages
library(e1071)
library(caret)

Training_sample <- building0
validation<- validation0
# Saving the waps in a vector
WAPs<-grep("WAP|FLOOR" , names(Training_sample), value=T)


# Train two classification svm (with svm and train)
system.time(svm_clasif <- svm(y = Training_sample$LONGITUDE, x=Training_sample[WAPs]))

svm_pred<-predict(svm_reg, validation[WAPs])
postResample(svm_pred, validation$LONGITUDE) 
# Train two regression svm (with svm and train)
system.time(svm_reg <- svm(y = stand_dataset$LONGITUDE, x=stand_dataset[WAPs]))

system.time(svm_reg_caret<-train(y=stand_dataset$LONGITUDE, x=as.matrix(stand_dataset[WAPs], data = stand_dataset, method="svmLinear")))
