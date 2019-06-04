#DEFINITIVE MODEL for test set#

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


validation<- read.csv("./datasets/testData.csv")
validation<-validation[,-c(521:529)]
validation$rownumber = 1:nrow(validation)

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
Building1_LATITUDE_KNN<-readRDS("./models/kNN1Lat.rds")
Building2_LATITUDE_KNN<-readRDS("./models/kNN2Lat.rds")


####GENERATING PREDICTIONS####

#Building
PREDICT1<-predict(rf_reg_caret, validation)

validation <- cbind(validation, PREDICT1)
names(validation)[names(validation)=="PREDICT1"] <- "BUILDINGID"

#### Subsetting####

validation0 <- validation %>% filter(BUILDINGID==0) 
validation1 <- validation %>% filter(BUILDINGID==1)
validation2 <- validation %>% filter(BUILDINGID==2)

#FLOOR BY BUILDING

#Building 0 & Longitude
test0.h2o <- as.h2o(validation0)
PRED_FBO <- as.data.frame(h2o.predict(Building0_Model, test0.h2o))
validation0<- cbind(validation0, PRED_FBO$predict)
names(validation0)[names(validation0)=="PRED_FBO$predict"] <- "FLOOR"

test0.h2o <- as.h2o(validation0)
PRED_B0_LONG<- as.data.frame(h2o.predict(Building0_LONGITUDE_RF, test0.h2o))
validation0<-cbind(validation0, PRED_B0_LONG$predict)
names(validation0)[names(validation0)=="PRED_B0_LONG$predict"] <- "LONGITUDE"

test0.h2o <- as.h2o(validation0)
PRED_B0_LAT<- as.data.frame(h2o.predict(Building0_LATITUDE_RF, test0.h2o))
validation0<-cbind(validation0, PRED_B0_LAT$predict)
names(validation0)[names(validation0)=="PRED_B0_LAT$predict"] <- "LATITUDE"


#Building 1
test1.h2o <- as.h2o(validation1)
PRED_FB1 <- as.data.frame(h2o.predict(Building1_Model, test1.h2o))
validation1<- cbind(validation1, PRED_FB1$predict)
names(validation1)[names(validation1)=="PRED_FB1$predict"] <- "FLOOR"

test1.h2o <- as.h2o(validation1)
PRED_B1_LONG<- as.data.frame(h2o.predict(Building1_LONGITUDE_RF, test1.h2o))
validation1<-cbind(validation1, PRED_B1_LONG$predict)
names(validation1)[names(validation1)=="PRED_B1_LONG$predict"] <- "LONGITUDE"


PRED_B1_LAT<-predict(Building1_LATITUDE_KNN, validation1)
validation1<-cbind(validation1, PRED_B1_LAT)
names(validation1)[names(validation1)=="PRED_B1_LAT"] <- "LATITUDE"

colnames(validation1)

#Building 2
test2.h2o <- as.h2o(validation2)
PRED_FB2 <- as.data.frame(h2o.predict(Building2_Model, test2.h2o))
validation2<- cbind(validation2, PRED_FB2$predict)
names(validation2)[names(validation2)=="PRED_FB2$predict"] <- "FLOOR"

test2.h2o <- as.h2o(validation2)
PRED_B2_LONG<- as.data.frame(h2o.predict(Building2_LONGITUDE_RF, test2.h2o))
validation2<-cbind(validation2, PRED_B2_LONG$predict)
names(validation2)[names(validation2)=="PRED_B2_LONG$predict"] <- "LONGITUDE"

PRED_B2_LAT<-predict(Building2_LATITUDE_KNN, validation2)
validation2<-cbind(validation2, PRED_B2_LAT)
names(validation2)[names(validation2)=="PRED_B2_LAT"] <- "LATITUDE"

####MERGING DATA####

test<- rbind(validation0[,521:525], validation1[,521:525], validation2[,521:525])
test<- test[,c(5,4,3)]
write.csv(test,file="test1_SERGI_SUPERMODEL.csv", row.names=FALSE)
