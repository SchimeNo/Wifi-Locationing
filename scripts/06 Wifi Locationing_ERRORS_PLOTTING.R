####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools, e1071,
               randomForest, h2o, class, parallelSVM)
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
#FLOOR MODELS
Building0_Model_GBM<-readRDS("./models/GBM_Floor_Building0.rds")
Building0_Model <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "RF_Floor_Building0"))
Building1_Model<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "ensemble_FLOOR_B1"))
Building2_Model<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "GBM_Floor_Building2"))

#LONGITUDE MODELS
Building0_LONGITUDE_RF<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "Building0_LONGITUDE_RF"))
Building1_LONGITUDE_RF<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "Building1_LONGITUDE_RF"))
Building2_LONGITUDE_RF<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "Building2_LONGITUDE_RF"))

#LATITUDE MODELS
Building0_LATITUDE_RF<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "Building0_LATITUDE_RF"))
Building1_LATITUDE_KNN<-readRDS("./models/Building1_LATITUDE_KNN.rds")
Building2_LATITUDE_KNN<-readRDS("./models/Building2_LATITUDE_KNN.rds")


#### Subsetting####

building0<- train %>% filter(BUILDINGID==0) %>% mutate( FLOOR=as.character(FLOOR))  
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

WAPs<-grep("WAP|LONGITUDE", names(building1), value=T)
preprocessParams1<-preProcess(building1[WAPs], method=c("center", "scale"))
valid_waps1<-predict(preprocessParams1, validation1[WAPs])
stand_waps1<-predict(preprocessParams1, building1[WAPs])
stand_dataset1<-cbind(stand_waps1, BUILDINGID=building1$BUILDINGID, FLOOR=building1$FLOOR, LATITUDE=building1$LATITUDE)

WAPs<-grep("WAP|LONGITUDE", names(building2), value=T)
preprocessParams2<-preProcess(building2[WAPs], method=c("center", "scale"))
valid_waps2<-predict(preprocessParams2, validation2[WAPs])
stand_waps2<-predict(preprocessParams2, building2[WAPs])
stand_dataset2<-cbind(stand_waps2, BUILDINGID=building2$BUILDINGID, FLOOR=building2$FLOOR, LATITUDE=building2$LATITUDE)


####GENERATING PREDICTIONS####

#Building
PREDICT1<-predict(rf_reg_caret, validation)
postResample(PREDICT1, validation$BUILDINGID)
C<-confusionMatrix(PREDICT1, validation$BUILDINGID)

#FLOOR BY BUILDING

#Building 0 & Longitude
test0.h2o <- as.h2o(validation0)

PRED_FBO <- as.data.frame(h2o.predict(Building0_Model, test0.h2o))
postResample(PRED_FBO, validation0$FLOOR)
table(PRED_FBO$predict, validation0$FLOOR)

PRED_B0_LONG<- as.data.frame(h2o.predict(Building0_LONGITUDE_RF, test0.h2o))
postResample(PRED_B0_LONG, validation0$LONGITUDE)

PRED_B0_LAT<- as.data.frame(h2o.predict(Building0_LATITUDE_RF, test0.h2o))
postResample(PRED_B0_LAT, validation0$LATITUDE)


#Building 1
test1.h2o <- as.h2o(validation1)
PRED_FB1 <- as.data.frame(h2o.predict(Building1_Model, test1.h2o))
postResample(PRED_FB1, validation1$FLOOR)
table(PRED_FB1$predict, validation1$FLOOR)
PRED_B1_LONG<- as.data.frame(h2o.predict(Building1_LONGITUDE_RF, test1.h2o))
postResample(PRED_B1_LONG, validation1$LONGITUDE)
PRED_B1_LAT<-predict(Building1_LATITUDE_KNN, valid_waps1)
postResample(PRED_B1_LAT, validation1$LATITUDE)


#Building 2
test2.h2o <- as.h2o(validation2)
PRED_FB2 <- as.data.frame(h2o.predict(Building2_Model, test2.h2o))
table(PRED_FB2$predict, validation2$FLOOR)
table
PRED_B2_LONG<- as.data.frame(h2o.predict(Building2_LONGITUDE_RF, test2.h2o))
postResample(PRED_B2_LONG, validation2$LONGITUDE)
PRED_B2_LAT<-predict(Building2_LATITUDE_KNN, valid_waps2)
postResample(PRED_B2_LAT, validation2$LATITUDE)


### ---- Error Check ----
## Creating data frames for random forest predictions
Building0pred <- data.frame(building = "0",
                            pred.longitude = PRED_B0_LONG$predict, 
                            pred.latitude = PRED_B0_LAT$predict, 
                            pred.floor = PRED_FBO$predict,
                            valid.longitude = validation0$LONGITUDE,
                            valid.latitude = validation0$LATITUDE,
                            valid.floor = validation0$FLOOR)

Building1pred <- data.frame(building = "1",
                            pred.longitude = PRED_B1_LONG$predict, 
                            pred.latitude = PRED_B1_LAT, 
                            pred.floor = PRED_FB1$predict,
                            valid.longitude = validation1$LONGITUDE,
                            valid.latitude = validation1$LATITUDE,
                            valid.floor = validation1$FLOOR)

Building2pred <- data.frame(building = "2",
                            pred.longitude = PRED_B2_LONG$predict, 
                            pred.latitude = PRED_B2_LAT, 
                            pred.floor = PRED_FB2$predict,
                            valid.longitude = validation2$LONGITUDE,
                            valid.latitude = validation2$LATITUDE,
                            valid.floor = validation2$FLOOR)
## Combining the data frames
ErrorData <- rbind(Building0pred, Building1pred, Building2pred)

## Calculating Errors
ErrorData$err.long <- abs(ErrorData$valid.longitude - ErrorData$pred.longitude)

ErrorData$err.lat <- abs(ErrorData$valid.latitude - ErrorData$pred.latitude)

ErrorData %>% 
  mutate_at(c("valid.floor", "pred.floor"), as.numeric) -> ErrorData

ErrorData$err.floor <- abs(ErrorData$valid.floor - ErrorData$pred.floor)
ErrorData$err.floor <- as.factor(ErrorData$err.floor)

ErrorData %>% 
  mutate_at(c("valid.floor",
              "pred.floor"),
            as.numeric) %>% 
  mutate(diff.floor = ifelse(valid.floor == pred.floor, 0, 1 )) %>% 
  mutate_at(c("valid.floor",
              "pred.floor"),
            as.factor) -> ErrorData

str(ErrorData)

## Checking Errors
confusionMatrix(ErrorData$pred.floor, ErrorData$valid.floor)

## Building 0 Floor Error Check
ErrorData %>% 
  filter(building == 0) %>% 
  group_by(diff.floor) %>% 
  count(valid.floor)

ErrorData%>% 
  filter(building == 2) %>% 
  group_by(pred.floor) %>% 
  ggplot(aes(pred.longitude, pred.latitude)) +
  geom_point(aes(colour = factor(diff.floor))) +
  facet_wrap(~pred.floor) +
  labs(title = "Building 2 by Floor")

## Longitude & Latitude
ErrorData %>% 
  filter(building==0||building==1||building ==2 && pred.floor== 3) %>% 
  ggplot(aes(x = valid.longitude, y = valid.latitude)) +
  geom_point(aes(x = valid.longitude, 
                 y = valid.latitude), 
             color = "red") +
  geom_point(aes(x = pred.longitude, 
                 y = pred.latitude), 
             color = "blue") +
  labs(title = "Data/Predicted") + 
  ylab("Latitude") + 
  xlab("Longitude") 

ErrorData %>%
  group_by(building) %>% 
  ggplot( aes(x=err.long)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

ErrorData0<-ErrorData %>%  filter(building == 0)

#density plot
ggplot(ErrorData0, aes(x = err.long, colour = pred.floor, fill =pred.floor)) +
  geom_density(position="identity", alpha=0.6) +
  scale_x_continuous(name = "MAE (m)",
                     limits=c(0, 60)) +
  scale_y_continuous(name = "Density") +
  ggtitle("Errors Longitude Building 0") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma")) +
  scale_fill_brewer(palette="Accent")

#####PLOTTING####

sample <- train %>% group_by(FLOOR, BUILDINGID) %>% sample_n(10)
sample$BUILDINGID <- as.character(sample$BUILDINGID)


ggplot() +
  geom_point(aes(x = validation0$LONGITUDE, y = a$predict), 
             color = "red") +
  geom_point(aes(x = validation0$LONGITUDE, y = validation0$LATITUDE), 
             color = "blue") +
  labs(title = "Data/Predicted BUILDING 0") + 
  geom_label(aes(x = -7600, y = 4864960, label = "Actual"), 
             color = "red", 
             size = 4) +
  geom_label(aes(x = -7600, y = 4864930, label = "Pred"), 
             color = "blue", 
             size = 4) +
  ylab("Latitude") + 
  xlab("Longitude") 

a<-cbind(predict.rforest, validation0$LONGITUDE)
plot.default(y = a$predict, x = a$`validation0$LONGITUDE`)


