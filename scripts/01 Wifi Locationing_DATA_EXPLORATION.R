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
#training$LOCATION<-formatC(training$LOCATION, width = 5, format = "d", flag = "0")
#validation$LOCATION<-as.numeric(stringr::str_pad(validation$LOCATION, 5, pad = "0"))

#for comfort we will move the last columns to the begining
training <- training %>%
  select("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP","LOCATION", everything())
validation <- validation %>%
  select("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP","LOCATION", everything())

#subset the data so it runs faster for the initial exploration
#trainingFEATURES <- training %>% select(1:11)
#validationFEATURES <- validation %>% select(1:11)

#check the Column Names
colnames(training)
colnames(validation)

#Checking distributions
#densityplot(~ LONGITUDE, data = trainingSAMPLE)
#densityplot(~LATITUDE, data = trainingSAMPLE)
#hist(trainingSAMPLE$FLOOR)

#Behaviour BUILDING & USERID
ggplot(training, mapping = aes(x=USERID, y=BUILDINGID))+
  geom_point()


####2. REMOVING UNUSED WAPs #####

#Check the means of all the Variables
means<-0
for (i in 1:530){
  means[i]<-mean(training[,i])
}

#Empty the no WAPs means (location, phoneID...)
means[1:11]<-0
means<-as.data.frame(means)

#can also be done with the function apply
#means<-apply(training[11:530], 2, mean)
#means<-as.data.frame(means)

#delete all the WAPs with a mean of =100
indices<-c()
for (i in 11:530){
  if(means[i,]==100){
    indices[i]<- i
  }
}

training2<- training[is.na(indices)]


####3. ASSIGNING A BUILDING TO A WAP ####

#Mean of WAPs by Building
by_building<- training2 %>%
  group_by(BUILDINGID) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

#assign a building to every WAP
for (i in 1:length(by_building)){
  if (by_building[1,i]<by_building[2,i] && by_building[1,i]<by_building[3,i]){
    by_building[4,i]<- as.numeric(0)
    
  }
  else if (by_building[2,i]<by_building[1,i] & by_building[2,i]<by_building[3,i]){
    by_building[4,i]<- as.numeric(1)
  }
   else {
     by_building[4,i]<- as.numeric(2)
   } 
}

#Converting the first row (names) and last row (building) into vectors
WAPs_name<-as.character(as.vector(colnames(by_building)))
WAPs_building<-as.numeric(as.vector(by_building[4,]))

#converting them into dataframes and bining them together (only including WAPs)
WAPs_name<- as.data.frame(WAPs_name)
WAPs_building<- as.data.frame(WAPs_building)
WAPs_location<- cbind(WAPs_name, WAPs_building)
WAPs_location<- WAPs_location[-c(1:10),]

remove(WAPs_name, WAPs_building)

#compare it to BUILDINGID to see if it's correct

#find phones with weak signals
phones <- split(training2, training2$PHONEID)
sapply(phones, function(x) {
  colMeans(x[, c(11:50)]) 
  })

####4.REMOVING LOW VARIANCE ####
#Show WAPs variance
variance <- sapply(training2[,c(11:length(training2))], var)

#VARIANCE CRITERIA
low_variance_WAP <- sapply(variance, function(x){
  (x<5)
})
low_variance_WAP<- which(low_variance_WAP, arr.ind = TRUE)
low_variance_WAP<- as.data.frame(low_variance_WAP)

#Remove WAPs with low variance
indicesVAR<-low_variance_WAP[,1]
training3<- training2[-(indicesVAR)]

#if WAP value is 100 change to -105
#change_WAP_value <- apply(training2[,c(1:465)], 2, function(x) {ifelse(x == 100, -105, x)})


####5.Metrics####
trainingNA<-training2
#Check min, max and mean excluding all the 100
trainingNA[ trainingNA == 100] <-NA

#function that shows mean, max, min
multi.fun <- function(x) {
  c(min = min(x, na.rm=TRUE), mean = mean(x, na.rm=TRUE), max = max(x, na.rm=TRUE), n=nrow(x))
}
metrics<- sapply(trainingNA[,c(11:length(training2))], multi.fun)


####6. SAMPLING #####

Training_sample <- training %>% group_by(FLOOR, BUILDINGID) %>% sample_n(100)
table(Training_sample$FLOOR)
table(Training_sample$BUILDINGID)

