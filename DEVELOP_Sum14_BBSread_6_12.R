# DEVELOP_Sum14_BBSread.r
# Alec Nelson
# 6/12/14
# Loading BBS Data

#If you don't have all these installed, uncomment and use:
# install.packages(c("date", "scales", "plyr", "raster", "rgdal", "rJava", "maptools","sp","zoo","gtools"))

library(plyr)
library(raster)
library(rgdal)
library(rJava)
library(maptools)
library(sp)
library(zoo)
library(gtools)
library(maps)
library(mapproj)
library(ggplot2)

#####################################################################
#load BBS data
setwd("C:/Users/ahnelson/Desktop/BCR14_BBS_Data")

Maine<-read.csv(file="Maine.csv",header = TRUE,sep = ",")
Vermont<-read.csv(file="Vermont.csv",header = TRUE,sep = ",")
NH<-read.csv(file="NHampsh.csv",header = TRUE,sep = ",")
NY<-read.csv(file="NYork.csv",header = TRUE,sep = ",")
NewB<-read.csv(file="NBrunsw.csv",header = TRUE,sep = ",")
NoSco<-read.csv(file="NovaSco.csv",header = TRUE,sep = ",")
Quebec<-read.csv(file="Quebec.csv",header = TRUE,sep = ",")
Mass<-read.csv(file="Massach.csv",header = TRUE,sep = ",")
CT<-read.csv(file="Connect.csv",header = TRUE,sep = ",")
PEI<-read.csv(file="PEI.csv",header = TRUE,sep = ",")

BCR14.2<-read.csv(file="Birds_BCR14_bystop.csv",header = TRUE,sep = ",")

str(BCR14.2)
summary(BCR14.2)

BCR14.10<-rbind(Maine,Vermont,NH,NY,NewB,NoSco,Quebec,Mass,CT,PEI)

#####################################################
#### Check point by point data -----------------------------
MNS.2<-rbind((BCR14.2[BCR14.2$Country==840 & BCR14.2$State==44,]),(BCR14.2[BCR14.2$Country==124 & BCR14.2$State==65,]))

BCR14.10<-rbind(Maine,Vermont,NH,NY,NewB,NoSco,Quebec,Mass,CT)

class(MNS.1)

MNS.2<-(MNS.2[MNS.2$Year>1995 & MNS.2$Year<2013,])
MNS.1<-(MNS.1[MNS.1$Year>1995 & MNS.1$Year<2013,])

MNS.2<-(MNS.2[MNS.2$Aou==5580,])
MNS.1<-(MNS.1[MNS.1$Aou==5580,])

str(Maine)
summary(MNS.1)
unique(Maine$Year)

str(MNS.2)
summary(MNS.2)
unique(MNS.2$Year)

sum(MNS.1$SpeciesTotal)


#Is Data clean of NA's?  YES!!!!! 
sum(is.na(MNS.2[,6:55])==TRUE)
names(MNS.2)
#####################################################
#### Clean up Route Data -----------------------------

routes<-read.csv(file="routes.csv",header = TRUE,sep = ",")

str(routes)

routesX<-read.csv(file="Route_Export.csv",header = TRUE,sep = ",")

str(routesX)

summary(routesX)

routesX$Shape..<-NULL
routesX$GPS<-NULL
routesX$Comments<-NULL
routesX$Portrait<-NULL
routesX$FullName<-NULL
routesX$FID<-NULL
routesX$OBJECTID<-NULL
routesX$StartTime<-NULL
routesX$Portrait<-NULL
routesX$UseGPS<-NULL
routesX$PaperStopD<-NULL
routesX$ARC_LENGTH<-NULL
routesX$rte_length<-NULL
routesX$RTENAME<-NULL
routesX$date_add<-NULL
routesX$Shape_Leng<-NULL
routesX$STATUS<-NULL

names(routesX)
str(routesX)
summary(routesX)
names(routesX)

table(routesX$rteno)
table(routesX$ProvRoute)

which(table(routesX$rteno)>1)
which(table(routesX$ProvRoute)>1)



routesX$ProvRoute[139:nrow(routesX)]<-routesX$rteno[139:nrow(routesX)]

RouteN<-routesX$ProvRoute

length(RouteN)

unique(RouteN)

which(table(RouteN)>1)

TabR<-table(RouteN)

length(TabR[TabR>1])
#####################################################
# Graphing of Abundance---------------------

BirdList<-c(7550,6360,6620,6540,6670,6290,6860,6570,5580,4020)
BirdNames<-c("WOTH","BWWA", "BLWA","BTBW","BTGW","BHVI","CAWA","MAWA","WTSP","YBSA")

BirdN<-function(data,statename,BirdList,BirdNames){
  #Iterate over species
  for(i in 1:length(BirdList)){
    BirdSub.i<-data[data$Aou==(BirdList[i]),]
    
    BirdSum.i<-sum(BirdSub.i$SpeciesTotal)
    
    if(i<2){
      BirdSums<-c(BirdSum.i)}
    if(i>1){
      BirdSums<-c(BirdSums,BirdSum.i)}
    }
  
  Bird.df<-cbind(BirdNames,BirdSums)
  Bird.df<-as.data.frame(Bird.df)
  
  barplot(BirdSums, main=(paste(c(statename,"Sample Bird Abundance"),sep=" ")),
          xlab="Focal Species",names.arg=BirdNames)  
    }


#Run Function
BirdN(Maine,"Maine",BirdList,BirdNames)
BirdN(Vermont,"Vermont",BirdList,BirdNames)
BirdN(NH,"NH",BirdList,BirdNames)
BirdN(NY,"NY",BirdList,BirdNames)
BirdN(NewB,"NewB",BirdList,BirdNames)
BirdN(NoSco,"NoSco",BirdList,BirdNames)
#####################################################
#Route Data:---------------------------

routes<-read.csv(file="routes.csv",header = TRUE,sep = ",")
str(routes)

routes14<-routes[routes$BCR==14,]

str(routes14)
summary(routes14)

points14<-routes14
class(points14$Lati)
#####################################################
#Break down BBS data (both datasets) by our study region and species of interest:

BCR14.10_A<-rbind(Maine,Vermont,NH,NY,NewB,NoSco,Quebec,Mass,CT,PEI)
BCR14.1_B<-BCR14.2

#Set species of interest with Aou numbers
BCR14.10_A<-(BCR14.10_A[BCR14.10_A$Aou== 5580 | BCR14.10_A$Aou==4020| BCR14.10_A$Aou==7550,])
BCR14.1_B<-(BCR14.1_B[BCR14.1_B$Aou== 5580 | BCR14.1_B$Aou==4020| BCR14.1_B$Aou==7550,])
#Set only 101 as the RPID
BCR14.10_A<-(BCR14.10_A[BCR14.10_A$RPID== 101,])
#Create unique RouteID variable
BCR14.10_A$RouteID<-(BCR14.10_A$countrynum*100000)+(BCR14.10_A$statenum*1000)+BCR14.10_A$Route
BCR14.1_B$RouteID<-(BCR14.1_B$Country*100000)+(BCR14.1_B$State*1000)+BCR14.1_B$Route
#Subset RouteID variable for BCR14
BCR14.10_A<-BCR14.10_A[BCR14.10_A$RouteID%in%BCR14.1_B$RouteID,]
#Subset data from 1985 to 1997
BCR14.10_A1985<-(BCR14.10_A[BCR14.10_A$Year>1984 & BCR14.10_A$Year<1998,])
#Subset data from 1997 to 2012
BCR14.10_A<-(BCR14.10_A[BCR14.10_A$Year>1996 & BCR14.10_A$Year<2013,])
BCR14.1_B<-(BCR14.1_B[BCR14.1_B$Year>1996 & BCR14.1_B$Year<2013,])

TEST<-(BCR14.10_A[BCR14.10_A$Year>1984 & BCR14.10_A$Year<2014,])

str(BCR14.10_A1985)
str(BCR14.10_A)
str(BCR14.1_B)

summary(BCR14.10_A1985)
summary(BCR14.10_A)
summary(BCR14.1_B)

length(unique(BCR14.10_A$RouteID))
length(unique(BCR14.10_A1985$RouteID))
length(unique(BCR14.1_B$RouteID))
length(unique(TEST$RouteID))

summary(BCR14.1_B)


####
#Checking disparities between data sources:--------------------
table(BCR14.10_A$Year)
table(BCR14.1_B$Year)

A<-BCR14.10_A$Year
B<-BCR14.1_B$Year

setdiff(A,B)
setdiff(B,A)


A.test<-(BCR14.10_A[BCR14.10_A$Year==2012,])
B.test<-(BCR14.1_B[BCR14.1_B$Year==2012,])

str(A.test)

A.test<-(A.test[A.test$Aou== 5580,])
B.test<-(B.test[B.test$Aou== 5580,])


table(A.test$Aou)
table(B.test$Aou)

A.test<-A.test[order(A.test$RouteID),]
B.test<-B.test[order(B.test$RouteID),]

names(B.test)
B.test$SpeciesTotal<-sum(B.test[6:55])

for(i in 1:length(B.test$SpeciesTotal)){
  B.test$SpeciesTotal[i]<-sum(B.test[i,6:55])
}

(table(B.test$RouteID)>1)
table(A.test$RouteID)
table(B.test$RouteID)

length(unique(A.test$RouteID))
length(unique(B.test$RouteID))


setdiff(B.test$RouteID,A.test$RouteID)
setdiff(A.test$RouteID,B.test$RouteID)

B.test[B.test$RouteID==12465029,]
B.test[B.test$RouteID==12475001,]
A.test[A.test$RouteID==12475001,]
B.test[B.test$RouteID==12476309,]
B.test[B.test$RouteID==84058015,]
B.test[B.test$RouteID==84058023,]

summary(A.test)

#In the Web-based Dataset, there are unique routes in certain Years that
  # observed our species than in the FTP dataset 
  # (more observations in Web than FTP)
###
###############################################################
# Begin change analysis of data over the time periods of 1985 to 1997 to 2012

#1985 to 1997
  #BCR14.10_A1985

#1997 to 2012 (2 datasets)
  #BCR14.10_A
  #BCR14.1_B

WTSP.All<-BCR14.10_A[BCR14.10_A$Aou==5580,]
WOTH.All<-BCR14.10_A[BCR14.10_A$Aou==7550,]
YBSA.All<-BCR14.10_A[BCR14.10_A$Aou==4020,]

WTSP.All<-(WTSP.All[WTSP.All$Year>1984 & WTSP.All$Year<2013,])
WOTH.All<-(WOTH.All[WOTH.All$Year>1984 & WOTH.All$Year<2013,])
YBSA.All<-(YBSA.All[YBSA.All$Year>1984 & YBSA.All$Year<2013,])

summary(WTSP.All)
summary(WOTH.All)
summary(YBSA.All)

###
# Plot abundance of bird in BCR14 from endpoints:

Species<-unique(BCR14.10_A1985$Aou)

#1985 to 1997
WTSP.1985<-BCR14.10_A1985[BCR14.10_A1985$Aou==5580,]
WOTH.1985<-BCR14.10_A1985[BCR14.10_A1985$Aou==7550,]
YBSA.1985<-BCR14.10_A1985[BCR14.10_A1985$Aou==4020,]

Ratio85<-c(sum(WTSP.1985[WTSP.1985$Year==1997,]$SpeciesTotal)/sum(WTSP.1985[WTSP.1985$Year==1985,]$SpeciesTotal),
sum(WOTH.1985[WOTH.1985$Year==1997,]$SpeciesTotal)/sum(WOTH.1985[WOTH.1985$Year==1985,]$SpeciesTotal),
sum(YBSA.1985[YBSA.1985$Year==1997,]$SpeciesTotal)/sum(YBSA.1985[YBSA.1985$Year==1985,]$SpeciesTotal))

#1997 to 2012 (BCR14.10_A)
WTSP.2012<-BCR14.10_A[BCR14.10_A$Aou==5580,]
WOTH.2012<-BCR14.10_A[BCR14.10_A$Aou==7550,]
YBSA.2012<-BCR14.10_A[BCR14.10_A$Aou==4020,]

Ratio2012.A<-c(sum(WTSP.2012[WTSP.2012$Year==2012,]$SpeciesTotal)/sum(WTSP.2012[WTSP.2012$Year==1997,]$SpeciesTotal),
sum(WOTH.2012[WOTH.2012$Year==2012,]$SpeciesTotal)/sum(WOTH.2012[WOTH.2012$Year==1997,]$SpeciesTotal),
sum(YBSA.2012[YBSA.2012$Year==2012,]$SpeciesTotal)/sum(YBSA.2012[YBSA.2012$Year==1997,]$SpeciesTotal))

#1997 to 2012 (BCR14.1_B)
WTSP.2012.B<-BCR14.1_B[BCR14.1_B$Aou==5580,]
WOTH.2012.B<-BCR14.1_B[BCR14.1_B$Aou==7550,]
YBSA.2012.B<-BCR14.1_B[BCR14.1_B$Aou==4020,]

Ratio2012.B<-c(sum(WTSP.2012.B[WTSP.2012.B$Year==2012,][6:55])/sum(WTSP.2012.B[WTSP.2012.B$Year==1997,][6:55]),
sum(WOTH.2012.B[WOTH.2012.B$Year==2012,][6:55])/sum(WOTH.2012.B[WOTH.2012.B$Year==1997,][6:55]),
sum(YBSA.2012.B[YBSA.2012.B$Year==2012,][6:55])/sum(YBSA.2012.B[YBSA.2012.B$Year==1997,][6:55]))

cbind(Species,Ratio85,Ratio2012.A,Ratio2012.B)

str(WTSP.1985)
str(WTSP.2012)

###
# Analyze change over time by year across all routes:

YearbyYear<-function(Alpha.Year,Title){
  Alpha.Year <- Alpha.Year[order(Alpha.Year$Year),] 
  
for(i in 1:length(unique(Alpha.Year$Year))){
  sum.i<-sum(Alpha.Year[Alpha.Year$Year==(unique(Alpha.Year$Year)[i]),]$SpeciesTotal)
  #print(sum.i) 
  if(i==1){
    Delta.i<-c(sum.i)
  } 
  if(i>1) {
    Delta.i<-c(Delta.i,sum.i)
  }
}

Years<-unique(Alpha.Year$Year)

plot(data.frame(Years,Delta.i), main=Title, type="o",col="blue")
abline(lm(Delta.i~Years))
fit <- lm(Delta.i~Years)
summary(fit)}

YearbyYear(WTSP.1985,"WTSP.1985")
YearbyYear(WOTH.1985,"WOTH.1985")
YearbyYear(YBSA.1985,"YBSA.1985")

YearbyYear(WTSP.2012,"WTSP.2012")
YearbyYear(WOTH.2012,"WOTH.2012")
YearbyYear(YBSA.2012,"YBSA.2012")

YearbyYear(WTSP.All,"WTSP.All")
YearbyYear(WOTH.All,"WOTH.All")
YearbyYear(YBSA.All,"YBSA.All")

###
# Analyze change over time by year and by route 
  # on a route-by-route basis:

length(which(table(WTSP.1985$RouteID)>12))
# 36 routes have reported observations for all 13 years
  
###############################################################
# Create Null Observation rows in Data
  # Import excel sheets
  # Determine missing RouteIDs
  # Generate rows of 0's for those RouteIDs

#Cleaning up and Loading Data: -------------------------------
# 1. Take out titles and clean up names (no spaces)
# 2. Make sure that all column names match exactly
# 3. Remove problematic symbols on data sheet
# 4. Save data as .csv, placed in relevant directory/folder

setwd("C:/Users/ahnelson/Desktop/WTSP Year")

#Use "for" function to join the data into one data set:
# Use state abbreviations to keep track of data
# Create State variable
loadCSVs<-function(bsd.dir, var.name){
  files <- dir(bsd.dir)
  for ( i in 1:length(files)){
    fn <- files[i]
    state <- substr(fn, 1, 2)
    df.i <- read.csv(file.path(bsd.dir, fn))
    df.i$state <- state
    if ( i == 1){
      df.out <- df.i
    } else {
      df.out <- rbind(df.out, df.i)
    }
  }
  #Eliminate state variable
  df.out$state<-NULL
  #Check file for accuracy
  str(df.out)
  #Create base dataframe that all other sections will copy from:
  assign(var.name, df.out, envir = .GlobalEnv)
}

#Direct program to YOUR relevant directory/folder
loadCSVs('C:/Users/ahnelson/Desktop/WTSP Year/RouteData',"routes")
loadCSVs('C:/Users/ahnelson/Desktop/WTSP Year/StopData',"stops")

str(routes)
str(stops)

stops$Country<-NULL
stops$State<-NULL
stops$Route<-NULL
stops$StopTotal<-NULL

names(routes)[4]<-"Year"


#Add 0,0,0,0,0 rows to Bird Data
for(i in 1:length(unique(routes$Year))){
  Year.i<-unique(routes$Year)[i]
  Aou.i<-stops$Aou[1]
  routematch<-setdiff(routes[routes$Year==Year.i,]$RouteID,stops[stops$Year==Year.i,]$RouteID)
  
  for(j in 1:length(routematch)){
    stops<-rbind(stops,c(routematch[j],Year.i,Aou.i,0,0,0,0,0,0))
  }
}


setwd("C:/Users/ahnelson/Desktop/WTSP Year")
#write.csv(stops, "WTSP_stopsEDIT_Null.csv", row.names = FALSE)

# for(i in 1:length(unique(stops$Year))){
#   Year.i<-unique(stops$Year)[i]
#   write.csv(stops[stops$Year==Year.i,], paste(c("WTSP_AllStops", Year.i,".csv"), collapse=""), row.names = FALSE)
# }

###############################################################
###############################################################

str(stops)

RoutebyRoute<-function(stops,Title){
  
  Alpha.Year <- stops[order(stops$Year),] 
  
  #ONLY INCLUDE ROUTES WITH FULL YEARS
  routematch.ij<-as.numeric(names(which(table(stops$RouteID)>15)))
    
  Alpha.Year <- Alpha.Year[Alpha.Year$RouteID %in% routematch.ij,]
  Years<-unique(Alpha.Year$Year)
  
  for (j in 1:length(routematch.ij)){
    for(i in 1:length(Years)){
      Year.i<-Years[i]
      Route.j<-routematch.ij[j]
      
      #Get Sum
      sum.ij<-sum(Alpha.Year[Alpha.Year$Year==(Year.i) & 
                               Alpha.Year$RouteID==(Route.j),]$SpeciesTotal)
      #Get Standard Deviation
      dev.ij<-sd(Alpha.Year[Alpha.Year$Year==(Year.i) & 
                              Alpha.Year$RouteID==(Route.j),][,4:8])
      if(i==1 & j==1){
        Delta.Route.j<-data.frame(RouteID=Route.j,Year=Year.i,Sum=sum.ij,Dev=dev.ij)
      } 
      else {
        Delta.Route.j<-rbind(Delta.Route.j,c(Route.j,Year.i,sum.ij,dev.ij))
      }
    }
  }
     
    # Figure out how  to store these route plots and create a par graph and stats
 
  p <- ggplot(Delta.Route.j, aes(Year, Sum))
  p <- p + geom_point(shape=1,colour="black",fill="black")
  p <- p + geom_line(colour="blue",size=1.2)
  p <- p + geom_smooth(method=lm,se=FALSE, colour="red")
  p <- p + geom_errorbar(aes(ymin=Sum-Dev, ymax=Sum+Dev), width=.1)
  p <- p + facet_wrap(~ RouteID,ncol=10, scale="free_y")
  p <- p + theme_bw() + xlab("Year") + ylab("Sum")
  theme(axis.text.x  = element_text(angle=45,hjust = 1,vjust = 1)) + theme(panel.grid.major = element_line(colour = "black", size = 1.5))
  p <- p + ggtitle(Title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  print(p)
  }


#RoutebyRoute(stops,"Routes (full year-to-year coverage)")

#################################

setwd("C:/Users/ahnelson/Desktop")

StopData<-read.csv(file="Birds_BCR14_bystop.csv",header = TRUE,sep = ",")

str(StopData)
summary(StopData)

#Create unique RouteID variable
StopData$RouteID<-(StopData$Country*100000)+(StopData$State*1000)+StopData$Route

#Set species of interest with Aou numbers
StopData.WTSP<-(StopData[StopData$Aou== 5580,])
StopData.WTSP<-(StopData[StopData$Country== 840,])


#Subset to one Route
StopData.WTSP.i<-(StopData.WTSP[StopData.WTSP$RouteID== 84044129,])

unique(StopData.WTSP$RouteID)

#Subset by each RouteID and turn into vectors to calculate histogram
#Use for loops to create vectors for each collection of data by Route

for(i in 1:length(unique(StopData.WTSP$RouteID))){
  Route.i<-unique(StopData.WTSP$RouteID)[i]
  StopData.WTSP.i<-(StopData.WTSP[StopData.WTSP$RouteID== Route.i,])
  for(j in 6:55){
    vector.i<-StopData.WTSP.i[,j]
    vector.i<-vector.i[vector.i!=0]
    if(j<7){
      Hist.WTSP.ij<-vector.i
    } else{
      Hist.WTSP.ij<-c(Hist.WTSP.ij,vector.i)
    }
  }
  print(Route.i)
  print(Hist.WTSP.ij)
  #Save as seperate vectors or add to a dataframe
  if(i<2){
    Routes.ij<-rep(Route.i,length(Hist.WTSP.ij))
    Hist.WTSP<-cbind(Routes.ij,Hist.WTSP.ij)
  }else{
    Routes.ij<-rep(Route.i,length(Hist.WTSP.ij))
    Add.Hist<-cbind(Routes.ij,Hist.WTSP.ij)
    Hist.WTSP<-rbind(Hist.WTSP,Add.Hist)
  }
  
}

Hist.WTSP<-as.data.frame(Hist.WTSP)
str(Hist.WTSP)
summary(Hist.WTSP)

p <- ggplot(Hist.WTSP, aes(x=Hist.WTSP.ij))
p <- p + geom_histogram(binwidth=1,colour="black", fill="white")
p <- p + facet_wrap(~ Routes.ij,ncol=10)  #, scale="free_y")
p <- p + theme_bw() + xlab("Count")
theme(axis.text.x  = element_text(angle=45,hjust = 1,vjust = 1)) + theme(panel.grid.major = element_line(colour = "black", size = 1.5))
p <- p + ggtitle("Route Counts in the US (Country Code 840)") + theme(plot.title = element_text(lineheight=.8, face="bold"))
print(p)
