pacman::p_load(readr, h2o, rstudioapi, caret, dplyr)
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

#SELECT FLOOR
train.h2o <- as.h2o(building2)
test.h2o <- as.h2o(validation2)

#dependent variable (Floor)
y.dep <- 3
#independent variables (WAPS)
x.indep <- c(11:452)

#####Linear Model ####
#regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.h2o, family = "gaussian")
#h2o.performance(regression.model)

#make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
postResample(predict.reg, validation1$FLOOR)

####Random Forest####
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, stopping_rounds = 2,     
                                ntrees = 1000, mtries = -1, max_depth = 15, seed = 1122, min_rows = 10))
h2o.performance(rforest.model)
summary(rforest.model)
#making predictions on unseen data
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
postResample(predict.rforest, validation2$FLOOR)
a<- cbind(predict.rforest$predict, validation1$FLOOR)
table(predict.rforest$predict, validation2$FLOOR)

####GBM####
system.time(gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

h2o.performance(gbm.model)
system.time(predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o)))
postResample(predict.gbm, validation2$FLOOR)
table(predict.gbm$predict, validation2$FLOOR)

####MANY MODELS TOGETHER####

system.time(md_rf <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, nfolds = 10, fold_assignment = "Random", keep_cross_validation_predictions = TRUE, 
                                      ntrees = 1000, max_depth = 15, min_rows = 10, nbins = 30, nbins_cats = 64, mtries = 2, sample_rate =1, stopping_tolerance = 1e-3, 
                                      stopping_rounds = 2, seed = 1234 ))
system.time(md_gbm <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, nfolds = 3, fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE, ntrees = 500, max_depth = 7, learn_rate = 0.1, learn_rate_annealing = 1, sample_rate = 1, col_sample_rate = 1, stopping_tolerance = 1e-3, stopping_rounds = 2, seed = 1234 ))

#md_nb<- h2o.naiveBayes(y=y.dep, x=x.indep, training_frame = train.h2o)

#mix the models
md_ens <- h2o.stackedEnsemble(y=y.dep, x=x.indep, training_frame = train.h2o, base_models = list( md_rf@model_id, md_gbm@model_id))

model<-md_nb

h2o.performance(model)
system.time(predict.model <- as.data.frame(h2o.predict(model, test.h2o)))
postResample(predict.model, validation1$FLOOR)
table(predict.model$predict, validation1$FLOOR)

#SAVE GBM MODEL FOR BUILDING 0, (ntrees 10000 time 12 min, ACC 0.9477612 KAPPA 0.9262027 )
#saveRDS(h2o.gbm, file="./models/GBM_Floor_Building0.rds")

#SAVE GBM MODEL FOR BUILDING 1, (time 12 min, ACC 0.7557003 kappa 0.6506229  ) 
#saveRDS(h2o.gbm, file="./models/GBM_Floor_Building1.rds")

#SAVE GBM MODEL FOR BUILDING 12, (time 12 min, ACC 0.8470149 KAPPA 0.7929098) 
#saveRDS(h2o.gbm, file="./models/GBM_Floor_Building2.rds")
