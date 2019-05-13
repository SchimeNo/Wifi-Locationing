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


####1.Subsetting, sampling and creating new variables####

#LOCATION
#create a single variable for location, building and room
training$LOCATION<- training$SPACEID*0100 + training$FLOOR*10 + training$BUILDINGID
validation$LOCATION<- validation$SPACEID*100 + validation$FLOOR*10 + validation$BUILDINGID

#add zeros on the left
training$LOCATION<-formatC(training$LOCATION, width = 5, format = "d", flag = "0")
validation$LOCATION<-as.numeric(stringr::str_pad(validation$LOCATION, 5, pad = "0"))



#for comfort we will move the last columns to the begining
training <- training %>%
  select("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP","LOCATION", everything())
validation <- validation %>%
  select("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP","LOCATION", everything())

#subset the data so it runs faster for the initial exploration
trainingSAMPLE <- training %>% select(1:11)
validationSAMPLE <- validation %>% select(1:11)


#check the Column Names
colnames(training)
colnames(validation)

#Checking distributions
densityplot(~ LONGITUDE, data = trainingSAMPLE)
densityplot(~LATITUDE, data = trainingSAMPLE)
hist(trainingSAMPLE$FLOOR)



