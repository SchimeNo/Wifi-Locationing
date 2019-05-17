#BASIC MODEL

####0. Libraries and directories####
pacman::p_load(DT, dplyr, caret, dplyr,
               ggplot2, lattice, rstudioapi)

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

validation[ validation == 100] <-NA

#function that shows mean, max, min
multi.fun <- function(x) {
  c(min = min(x, na.rm=TRUE), mean = mean(x, na.rm=TRUE), max = max(x, na.rm=TRUE))
}
Vmetrics<- sapply(validation, multi.fun)

#### 2.RANDOM FOREST Model based on Sample ####

# Saving the waps in a vector
WAPs<-grep("WAP", names(Training_sample), value=T)

# Get the best mtry
bestmtry_rf<-tuneRF(Training_sample[WAPs], Training_sample$LONGITUDE, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T) 

# Train a random forest using that mtry
system.time(rf_reg<-randomForest(y=Training_sample$LONGITUDE,x=Training_sample[WAPs],importance=T,method="rf", ntree=100, mtry=20))

# Train a random forest using caret package
system.time(rf_reg_caret<-train(y=sample$LONGITUDE, x=sample[WAPs], data = sample, method="rf", ntree=100,tuneGrid=expand.grid(.mtry=22)))
