#OWN MODEL BASED ON BUILDING ASSIGNED WAPs

####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi,
               readr, plotly, htmltools)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

#Loading the data

training<- read.csv("./datasets/trainingData.csv")
validation<- read.csv("./datasets/validationData.csv")

#### 1. Applying Model based on Building-Assigned Waps ####

#we will remove the not used WAPs from the training also
#can also be done with the function apply
meansV<-apply(validation[1:520], 2, mean)
meansV<-as.data.frame(meansV)

#delete all the WAPs with a mean of =100
indicesV<-c()
for (i in 1:520){
  if(meansV[i,]==100){
    indicesV[i]<- i
  }
}
validation2<- validation[is.na(indicesV)]


#function that shows mean, max, min


validationWAPs<- validation[1:520]
validationWAPs[ validationWAPs == 100] <-NA


#EXTRACT MAX VALUE OF EVERY ROW
 aa<-colnames(validationWAPs)[apply(validationWAPs,1,which.max)]
 aa<-as.data.frame(aa)

 predicted_building<-c()
 for (i in 1:nrow(aa)){
   for (j in 1:nrow(WAPs_location)){
     if (WAPs_location[j,1]==aa[i, 1]){
       predicted_building[i]<- WAPs_location[j,2]
     }
   }
 }

predicted_building<- as.data.frame(predicted_building)
 PREDICTION<- cbind(predicted_building, validation["BUILDINGID"])

 table(PREDICTION$predicted_building, PREDICTION$BUILDINGID) 
 postResample(pred=PREDICTION$predicted_building, obs = PREDICTION$BUILDINGID)
 
 
#Show NA
subset(PREDICTION,is.na(PREDICTION))
which(is.na(PREDICTION$predicted_building))

bb<-validationWAPs[102,]
bb[which(!is.na(bb))]