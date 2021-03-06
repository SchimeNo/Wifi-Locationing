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

#### Subsetting####
count<-0
namesT<-NULL
namesV<-NULL
for (b in 0:2){
  for (f in 0:3){
    t<- train %>% filter(BUILDINGID == b, FLOOR == f) %>% select(starts_with("WAP"), LONGITUDE) 
    assign(paste("Train_B",b,"F",f, sep=""),t)
    v<- validation %>% filter(BUILDINGID == b, FLOOR == f) %>% select(starts_with("WAP"), LONGITUDE)
    assign(paste("Validation_B",b,"F",f, sep=""),v)
    namesT[count]<-paste("Train_B",b,"F",f, sep="")
    namesV[count]<-paste("Validation_B",b,"F",f, sep="")
    count<-count+1
  }
}

Train_B2F4 <- train %>% filter(BUILDINGID == 2, FLOOR == 4) %>% select(starts_with("WAP"), LONGITUDE)
Validation_B2F4 <- validation %>% filter(BUILDINGID == 2, FLOOR == 4) %>% select(starts_with("WAP"), LONGITUDE)
namesT[length(namesT)+1]<- "Train_B2F4"
namesV[length(namesV)+1]<- "Validation_B2F4"








set.seed(123)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv", number = 6, repeats = 2, preProc = c("center", "range"))

####KNN For Building 0####
#B0F0
kNN_B0F0 <- train(LONGITUDE ~ ., Train_B0F0, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B0F0<- predict(kNN_B0F0, Validation_B0F0)
metrics_B0F0<-postResample( pred_B0F0, Validation_B0F0$LONGITUDE)

#B0F1
kNN_B0F1 <- train(LONGITUDE ~ ., Train_B0F1, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B0F1<- predict(kNN_B0F1, Validation_B0F1)
metrics_B0F1<-postResample( pred_B0F1, Validation_B0F1$LONGITUDE)

#B0F2
kNN_B0F2 <- train(LONGITUDE ~ ., Train_B0F2, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B0F2<- predict(kNN_B0F2, Validation_B0F2)
metrics_B0F2<-postResample( pred_B0F2, Validation_B0F2$LONGITUDE)

#B0F3
kNN_B0F3 <- train(LONGITUDE ~ ., Train_B0F3, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B0F3<- predict(kNN_B0F3, Validation_B0F3)
metrics_B0F3<-postResample( pred_B0F3, Validation_B0F3$LONGITUDE)



####KNN For Building 1####
#B1F0
kNN_B1F0 <- train(LONGITUDE ~ ., Train_B1F0, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B1F0<- predict(kNN_B1F0, Validation_B1F0)
metrics_B1F0<-postResample( pred_B1F0, Validation_B1F0$LONGITUDE)

#B1F1
kNN_B1F1 <- train(LONGITUDE ~ ., Train_B1F1, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B1F1<- predict(kNN_B1F1, Validation_B1F1)
metrics_B1F1<-postResample( pred_B1F1, Validation_B1F1$LONGITUDE)

#B1F2
kNN_B1F2 <- train(LONGITUDE ~ ., Train_B1F2, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B1F2<- predict(kNN_B1F2, Validation_B1F2)
metrics_B1F2<-postResample( pred_B1F2, Validation_B1F2$LONGITUDE)

#B1F3
kNN_B1F3 <- train(LONGITUDE ~ ., Train_B1F3, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B1F3<- predict(kNN_B1F3, Validation_B1F3)
metrics_B1F3<-postResample( pred_B1F3, Validation_B1F3$LONGITUDE)


####KNN For Building 2####
#B2F0
kNN_B2F0 <- train(LONGITUDE ~ ., Train_B2F0, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B2F0<- predict(kNN_B2F0, Validation_B2F0)
metrics_B2F0<-postResample( pred_B2F0, Validation_B2F0$LONGITUDE)

#B2F1
kNN_B2F1 <- train(LONGITUDE ~ ., Train_B2F1, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B2F1<- predict(kNN_B2F1, Validation_B2F1)
metrics_B2F1<-postResample( pred_B2F1, Validation_B2F1$LONGITUDE)

#B2F2
kNN_B2F2 <- train(LONGITUDE ~ ., Train_B2F2, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B2F2<- predict(kNN_B2F2, Validation_B2F2)
metrics_B2F2<-postResample( pred_B2F2, Validation_B2F2$LONGITUDE)

#B2F3
kNN_B2F3 <- train(LONGITUDE ~ ., Train_B2F3, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B2F3<- predict(kNN_B2F3, Validation_B2F3)
metrics_B2F3<-postResample( pred_B2F3, Validation_B2F3$LONGITUDE)

#B2F4
kNN_B2F4 <- train(LONGITUDE ~ ., Train_B2F4, method = "knn",trControl = kNNcontrol, preProcess = "zv")
pred_B2F4<- predict(kNN_B2F4, Validation_B2F4)
metrics_B2F4<-postResample( pred_B2F4, Validation_B2F4$LONGITUDE)

####METRICS####

Metrics <- data.frame(metrics_B0F0,metrics_B0F1,metrics_B0F2, metrics_B0F3,
                                 metrics_B1F0,metrics_B1F1,metrics_B1F2, metrics_B1F3,
                                 metrics_B2F0,metrics_B2F1,metrics_B2F2, metrics_B2F3,metrics_B2F4)

Metrics <- data.frame(t(Metrics))
Metrics %>% arrange(MAE) 




####TRYING WITH A FOR#####

prediction2<-NULL
count<-1

system.time(
  for (b in 0:2){
    for (f in 0:3){
      t<- train %>% filter(BUILDINGID == b, FLOOR == f) %>% select(starts_with("WAP"), LONGITUDE) 
      
      v<- validation %>% filter(BUILDINGID == b, FLOOR == f) %>% select(starts_with("WAP"), LONGITUDE)
      kNNLong <- train(LONGITUDE ~ ., 
                       t,
                       method = "knn",
                       trControl = kNNcontrol,
                       preProcess = "zv")
      
      predicton<-predict(kNNLong, v)
      aux<- noquote(namesV[count])
      
      noquote(namesV[count])<-cbind( predicton, aux)
      count<-count+1
      
    }
  }
)
