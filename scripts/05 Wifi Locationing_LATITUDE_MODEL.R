####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret,
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

####KNN without standarize####
set.seed(123)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv", number = 6, repeats = 2, preProc = c("center", "range"))


#B0F0
WAPs<-grep("WAP", names(building0), value=T)
kNN_B0 <- train(LATITUDE ~ ., building0, method = "knn",trControl = kNNcontrol, preProcess = "zv")
kNN_B0<-knnreg(x=as.matrix(building0[WAPs]), y=building0$LATITUDE, k=3)
pred_B0<- predict(kNN_B0, validation0)
metrics_B0F0<-postResample( pred_B0, validation0$LATITUDE)
saveRDS(kNN_B0, "./models/Building0_LATITUDE_KNN")


####H2O RANDOM FOREST####

#SELECT BUILDING
train.h2o <- as.h2o(building1)
test.h2o <- as.h2o(validation1)

#dependent variable (LATITUDE)
y.dep <- 2
#independent variables (WAPS)
x.indep <- c(1,3,11:452)

#Random Forest#
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, stopping_rounds = 2,     
                                              ntrees = 500, mtries = -1, max_depth = 15, seed = 1122, min_rows = 10))
h2o.performance(rforest.model)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
postResample(predict.rforest, validation1$LATITUDE)
h2o.saveModel(object=rforest.model, path=paste0(getwd(),"/", "models"), force=TRUE)


####GBM####
system.time(gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 100, max_depth = 4, learn_rate = 0.01, seed = 1122))

h2o.performance(gbm.model)
system.time(predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o)))
postResample(predict.gbm, validation0$LATITUDE)

##### STANDARIZE #####

Training_sample <- building2
validation_sample<- validation2


WAPs<-grep("WAP|LONGITUDE", names(Training_sample), value=T)
preprocessParams<-preProcess(Training_sample[WAPs], method=c("center", "scale"))

valid_waps<-predict(preprocessParams, validation_sample[WAPs])
stand_waps<-predict(preprocessParams, Training_sample[WAPs])

stand_dataset<-cbind(stand_waps, BUILDINGID=Training_sample$BUILDINGID, FLOOR=Training_sample$FLOOR, LATITUDE=Training_sample$LATITUDE)

####KNN####
system.time(knn_reg<-knnreg(x=as.matrix(stand_dataset[WAPs]), y=stand_dataset$LATITUDE, k=9))
system.time(knn_predict<-predict(knn_reg, valid_waps))
postResample( knn_predict, validation_sample$LATITUDE)
saveRDS(preprocessParams, "./models/preprocessParams.rds")

