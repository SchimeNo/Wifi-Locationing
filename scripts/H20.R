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
Building0_Model <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "RF_Floor_Building0"))
Building1_Model<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "ensemble_FLOOR_B1"))
Building2_Model<-h2o.loadModel(path=paste0(getwd(),"/","models", "/", "GBM_Floor_Building2"))

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

WAPs<-grep("WAP", names(building0), value=T)
system.time(rf_reg<-randomForest(y=building0$FLOOR,x=building0[WAPs],importance=T,method="rf", ntree=100, mtry=3))
system.time(predicted_building <- predict(rf_reg, validation0))
#confusionMatrix
confusionMatrix(table(predicted_building, validation$BUILDINGID))
####Random Forest####
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, stopping_rounds = 2,     
                                ntrees = 100, mtries = -1, max_depth = 15, seed = 1122, min_rows = 10))

h2o.performance(rforest.model)
summary(rforest.model)
#making predictions on unseen data
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
postResample(predict.rforest, validation2$FLOOR)
table(predict.rforest$predict, validation2$FLOOR)


####GBM####
system.time(gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

h2o.performance(gbm.model)
system.time(predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o)))
postResample(predict.gbm, validation2$FLOOR)
table(predict.gbm$predict, validation2$FLOOR)

model_path <- h2o.saveModel(object=model, path=getwd(), force=TRUE)
print(model_path)



####ENSEMBLE MODEL (GBM + RF)####

#mix the models
system.time(my_gbm <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o,
                              ntrees = 100, max_depth = 3, min_rows = 2, learn_rate = 0.2,
                              nfolds = 5, fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE, seed = 123))

system.time(my_rf <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o,
                          ntrees = 100,nfolds = 5,fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,seed = 123))

system.time(ensemble <- h2o.stackedEnsemble(y=y.dep, x=x.indep, training_frame = train.h2o,
                                model_id = "my_ensemble",
                                base_models = list(my_gbm, my_rf )))

h2o.saveModel(object=gbm.model , path=paste0(getwd(),"/", "models"), force=TRUE)

model<- ensemble

h2o.performance(model)
system.time(predict.model <- as.data.frame(h2o.predict(model, test.h2o)))
postResample(predict.model, validation2$FLOOR)
table(predict.model$predict, validation1$FLOOR)

# saveRDS(my_gbm, file="./models/my_gbm_FLOOR_B1.rds")
# saveRDS(my_rf, file="./models/my_rf_FLOOR_B1.rds")
# saveRDS(ensemble, file="./models/ensemble_FLOOR_B1.rds")



# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_gbm_grid_binomial",
                                base_models = gbm_grid@model_ids)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)


####SAVING THE MODELS

#SAVE RFMODEL FOR BUILDING 0, (ntrees 10000 time 12 min, ACC 0.9495327 K 0.9284430  )
#model_B1F_path <- h2o.saveModel(object=rforest.model, path=paste0(getwd(),"/", "models"), force=TRUE)


#SAVE ENSAMBLE MODEL FOR BUILDING 1, (time 12 min, ACC 0.7759740 kappa 0.6777705   ) 
# saveRDS(ensemble, file="./models/ensemble_FLOOR_B1.rds")

#SAVE GBM MODEL FOR BUILDING 2, (time 12 min, ACC 0.8470149 KAPPA 0.7929098) 
#saveRDS(h2o.gbm, file="./models/GBM_Floor_Building2.rds")


#### Store predicted Floor Variable ####
# Load MODELS 
rf_reg_caret<-readRDS("./models/RF_Model.rds")
Building0_Model<-readRDS("./models/GBM_Floor_Building0.rds")
Building1_Model<-readRDS("./models/ensemble_FLOOR_B1.rds")
Building2_Model<-readRDS("./models/GBM_Floor_Building2.rds")


h2o.performance(Building0_Model)
system.time(predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o)))
postResample(predict.gbm, validation2$FLOOR)
table(predict.gbm$predict, validation2$FLOOR)
