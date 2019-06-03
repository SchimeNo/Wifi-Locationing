#FLOOR PREDICTION#


####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools, e1071,
               randomForest, h2o, gbm)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

#Loading the data

training_original<- read.csv("./datasets/trainingData.csv")
validation_original<- read.csv("./datasets/validationData.csv")

training2<- readRDS("./datasets/training2.rds")
validation<- readRDS("./datasets/validation2.rds")

# Load MODELS 
rf_reg_caret<-readRDS("./models/RF_Model.rds")

#LOCATION Variable
validation$LOCATION<- validation$SPACEID*100 + validation$FLOOR*10 + validation$BUILDINGID

#### 1. Sampling the data ####
# 
# Training_sample <- training2 %>% group_by(FLOOR, BUILDINGID) %>% sample_n(10)
# table(Training_sample$FLOOR)
# table(Training_sample$BUILDINGID)

####1.1 STANDARIZATION #####
WAPs<-grep("WAP", names(training2), value=T)
preprocessParams<-preProcess(training2[WAPs], method=c("center", "scale"))

valid_waps<-predict(preprocessParams, validation[WAPs])
stand_waps<-predict(preprocessParams, training2[WAPs])

stand_dataset<-cbind(stand_waps, BUILDINGID=training2$BUILDINGID, FLOOR=training2$FLOOR)

####2. FLOOR ####



#Group by building
building0<- training2 %>% filter(BUILDINGID==0) %>% mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  
building1<- training2 %>% filter(BUILDINGID==1) %>% mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  
building2<- training2 %>% filter(BUILDINGID==2) %>% mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  

validation0 <- validation %>% filter(BUILDINGID==0) %>%  mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  
validation1 <- validation %>% filter(BUILDINGID==1)%>%  mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  
validation2 <- validation %>% filter(BUILDINGID==2)%>%  mutate( FLOOR=as.character(FLOOR)) %>% select(FLOOR, WAPs, BUILDINGID)  

building0 <- building0 %>% mutate(FLOOR=as.factor(FLOOR))
building1 <- building1 %>% mutate(FLOOR=as.factor(FLOOR))
building2 <- building2 %>% mutate(FLOOR=as.factor(FLOOR))

validation0 <- validation0 %>% mutate(FLOOR=as.factor(FLOOR))
validation1 <- validation1 %>% mutate(FLOOR=as.factor(FLOOR))
validation2 <- validation2 %>% mutate(FLOOR=as.factor(FLOOR))


#check metrics of each building
#how many rooms, floors and samples per room
training2 %>% group_by(BUILDINGID) %>% summarize(rooms=length(unique(SPACEID)))
training2 %>% group_by(BUILDINGID) %>% summarize(rooms=length(unique(FLOOR)))
room_samples<-training2 %>% group_by(BUILDINGID, SPACEID) %>% summarize(samples=n())

aa20<-room_samples %>% filter(samples<=20)


#KNN Floor 0
set.seed(123)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv", number = 5,repeats = 2, preProc = c("center", "scale", "range"))

## Building 0 Floor Prediction
system.time(kNNFloor0 <- train(FLOOR ~ .-SPACEID, data=building0, method = "knn",trControl = kNNcontrol, preProcess = "zv"))



predkNN0Floor <- predict(kNNFloor0, validation0)
kNN0FloorMetrics<-postResample(predkNN0Floor, validation0$FLOOR) 
confusionMatrix(table(predkNN0Floor, validation0$FLOOR))



###GBM BUILDING 1####

system.time(gbmfit <- gbm(FLOOR ~ .-BUILDINGID, data = building1, distribution = "gaussian",
  n.trees = 300, interaction.depth = 1, shrinkage = 0.001, cv.folds = 5, n.cores = NULL, verbose = FALSE))  

Yhat <- predict(gbmfit, newdata = validation1, n.trees = best.iter, type = "link")

predGBM <- predict(gbmfit, validation1)
Floor1Metrics<-postResample(predGBM, validation1$FLOOR) 
confusionMatrix(table(predkNN0Floor, validation0$FLOOR))


#### Store predicted Floor Variable ####
predGBM <- predict(gbmfit, validation1)

