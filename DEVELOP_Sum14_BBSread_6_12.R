# DEVELOP_Sum14_BBSread.r
# Alec Nelson
# 7/3/14
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
    state.i <- substr(fn, 1, 2)
    df.i <- read.csv(file.path(bsd.dir, fn))
    df.i$state.i <- state.i
    if ( i == 1){
      df.out <- df.i
    } else {
      df.out <- rbind(df.out, df.i)
    }
  }
  #Eliminate state variable
  df.out$state.i<-NULL
  #Check file for accuracy
  str(df.out)
  #Create base dataframe that all other sections will copy from:
  assign(var.name, df.out, envir = .GlobalEnv)
}

#Direct program to YOUR relevant directory/folder

# Load in Stop by Stop data formatted correctly
loadCSVs('C:/Users/ahnelson/Desktop/WTSP Year/StopbyStopData',"stops")
loadCSVs('C:/Users/ahnelson/Desktop/WTSP Year/RouteData',"routes")

#routes<-read.csv(file="Final_Routes.csv",header = TRUE,sep = ",")

str(routes)
summary(routes)
str(stops)
summary(stops)

#unique (unlist (lapply (stops, function (x) which (is.na (x)))))

stops<-(stops[stops$Country== 840,])
routes<-(routes[routes$Country== 840,])

#Create unique RouteID variable
#stops$RouteID<-(stops$Country*100000)+(stops$State*1000)+stops$Route
#routes$RouteID<-(routes$Country*100000)+(routes$State*1000)+routes$Route

stops$Country<-NULL
stops$State<-NULL
stops$Route<-NULL
str(stops)
#stops <- stops[c(53,seq(1,52))]


unique(routes$RouteID)
Years<-c(1999:2013)

setdiff(routes$RouteID,stops[stops$Year==Years[1],]$RouteID)
setdiff(routes[routes$Year==Years[1],]$RouteID,stops[stops$Year==Years[1],]$RouteID)


setwd("C:/Users/ahnelson/Desktop/WTSP Year")
final_routes<-read.csv(file="Final_Routes.csv",header = TRUE,sep = ",")

#Only include routes in final route list
stops<-stops[stops$RouteID %in% final_routes$RouteID,]

#Add 0,0,0,0,0 rows to Bird Data
for(i in 1:length(Years)){
  Year.i<-Years[i]
  Aou.i<-stops$Aou[1]
  routematch<-setdiff(routes[routes$Year==Years[i],]$RouteID,stops[stops$Year==Years[i],]$RouteID)
  
  for(j in 1:length(routematch)){
    stops<-rbind(stops,c(routematch[j],Year.i,Aou.i,(rep(0,times=50))))
  }
}

#Add NA rows based on Final Route to Bird Data
for(i in 1:length(Years)){
  Year.i<-Years[i]
  Aou.i<-stops$Aou[1]
  routematch<-setdiff(final_routes$RouteID,stops[stops$Year==Years[i],]$RouteID)
  
  for(j in 1:length(routematch)){
    stops<-rbind(stops,c(routematch[j],Year.i,Aou.i,(rep(NA,times=50))))
  }
}

#Only include routes in final route list
stops<-stops[stops$RouteID %in% final_routes$RouteID,]

tail(stops)
unique(stops$RouteID)
unique(final_routes$RouteID)

unique(routes$RouteID)

str(stops)
summary(stops)

#Save to stops.i
stops.i<-stops


unique(stops.i$RouteID)
unique(routes$RouteID)
unique(final_routes$RouteID)

# Change observed values to 0 or 1
for(i in 4:ncol(stops.i)){
  for(j in 1:nrow(stops.i)){
    sample.ij<-stops.i[j,i]
    if(is.na(sample.ij)==TRUE){
      stops.i[j,i]<-NA
      
    } else{
      if(sample.ij==0){
        stops.i[j,i]<-0
      } else if(sample.ij>0){
        stops.i[j,i]<-1 
      } else {
        print("Huh?")
      }
    }
  }
}

summary(stops.i)


#Check whether the route was run in the year and in that period
setwd("C:/Users/ahnelson/Desktop/WTSP Year")
routes_year<-read.csv(file="RouteYear.csv",header = TRUE,sep = ",")

str(routes_year)


routes_year_1<-data.frame(RouteID=routes_year$RouteID,Year1999=routes_year$Year1999,Year2000=routes_year$Year2000,
                          Year2001=routes_year$Year2001,Year2002=routes_year$Year2002,Year2003=routes_year$Year2003,
                          Run1=routes_year$Run1)

routes_year_2<-data.frame(RouteID=routes_year$RouteID,Year2004=routes_year$Year2004,Year2005=routes_year$Year2005,
                          Year2006=routes_year$Year2006,Year2007=routes_year$Year2007,Year2008=routes_year$Year2008,
                          Run2=routes_year$Run2)

routes_year_3<-data.frame(RouteID=routes_year$RouteID,Year2009=routes_year$Year2009,Year2010=routes_year$Year2010,
                          Year2011=routes_year$Year2011,Year2012=routes_year$Year2012,Year2013=routes_year$Year2013,
                          Run3=routes_year$Run3)

str(routes_year_1)

#Apply NA's to routes that don't fit the criteria

  # CHECK EFFECTS OF NA'S AS INPUTS INTO THE LOOPS


YearWrite<-function(stops.i,routes_year,Year.i,year_pos){
  stops.year.i<-stops.i[stops.i$Year==Year.i,]
  routes.i<-unique(stops.year.i$RouteID)
  
  for(i in 1:length(routes.i)){
    stops.test<-stops.year.i[stops.year.i$RouteID==(routes.i[i]),]
    routes.test<-routes_year[routes_year$RouteID==(routes.i[i]),]
    
    if(routes.test[1+year_pos]==0 | routes.test[7]==0){
      print("Applying NA's")
      stops.test[4:length(stops.test)]<-NA
    } else {
      print("All Clear")
    }
    
    df.i<-stops.test
    
    if ( i == 1){
      df.out <- df.i
    } else {
      df.out <- rbind(df.out, df.i)
    }
  }
  
  print(as.character(paste(c("Writing", Year.i,"file"), collapse=" ")))
  
  write.csv(df.out, paste(c("WTSP_StopComplete_", Year.i,".csv"), collapse=""), row.names = FALSE)
}

setwd("C:/Users/ahnelson/Desktop/WTSP Year")


YearWrite(stops.i,routes_year_1,1999,1)
YearWrite(stops.i,routes_year_1,2000,2)
YearWrite(stops.i,routes_year_1,2001,3)
YearWrite(stops.i,routes_year_1,2002,4)
YearWrite(stops.i,routes_year_1,2003,5)

YearWrite(stops.i,routes_year_2,2004,1)
YearWrite(stops.i,routes_year_2,2005,2)
YearWrite(stops.i,routes_year_2,2006,3)
YearWrite(stops.i,routes_year_2,2007,4)
YearWrite(stops.i,routes_year_2,2008,5)

YearWrite(stops.i,routes_year_3,2009,1)
YearWrite(stops.i,routes_year_3,2010,2)
YearWrite(stops.i,routes_year_3,2011,3)
YearWrite(stops.i,routes_year_3,2012,4)
YearWrite(stops.i,routes_year_3,2013,5)

#write.csv(stops[stops$Year==Year.i,], paste(c("WTSP_AllStops", Year.i,".csv"), collapse=""), row.names = FALSE)

# setwd("C:/Users/ahnelson/Desktop/WTSP Year")
# write.csv(stops, "WTSP_stopsEDIT_Null.csv", row.names = FALSE)


# for(i in 1:length(unique(stops$Year))){
#   Year.i<-unique(stops$Year)[i]
#   write.csv(stops[stops$Year==Year.i,], paste(c("WTSP_AllStops", Year.i,".csv"), collapse=""), row.names = FALSE)
# }


################################################################################
################################################################################
# Run a Majority calc for each stop based on the period of interest and focal year
# Output format should be the same as complete df, with 0, 1, or NA
# Apply majority and focal-based logic to deciding resulting value


setwd("C:/Users/ahnelson/Desktop/WTSP_StopComplete")

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
loadCSVs('C:/Users/ahnelson/Desktop/WTSP_StopComplete',"stops")

str(stops)
summary(stops)

yearstart<-1999
yearstop<-2003
yearfocal<-2001


################################################################################
#LOAD FUNCTION TO RUN
################################################################################
MajorityFocal<-function(stops,yearstart,yearstop,yearfocal){
  #Create list of years
  years<-seq(yearstart,yearstop)
  #print("Set Years")
  
  #Iterate through Routes
  for(i in 1:length(unique(stops$RouteID))){
    Route.i<-unique(stops$RouteID)[i]
    #Subset by Route to capture all years, then by years of interest
    stops.route.i<-(stops[stops$RouteID== Route.i,])
    stops.route.i<-(stops.route.i[stops.route.i$Year>=yearstart & stops.route.i$Year<=yearstop,])
    
    #Iterate through Stops along the Routes
    for(j in 4:ncol(stops.route.i)){
      na.count<-length((is.na(stops.route.i[,j])[is.na(stops.route.i[,j])==TRUE]))
      if(na.count==5){stop.val.ij<-NA} 
      
      else if (na.count < 5){
        
        #Calculate the Majority value, between 0 and 1, disregarding NA's
        sum.stop<-sum(stops.route.i[,j],na.rm=TRUE)
        not.na<-length((is.na(stops.route.i[,j])[is.na(stops.route.i[,j])==FALSE]))
        maj<-sum.stop/not.na
        
        #Apply logic of Majority based on center value of 0.5
        if(maj < 0.5){ stop.val.ij<-0} else if (maj > 0.5){stop.val.ij<-1}
          
          #If finds a maj value of 0.5 (a tie) apply the focal to the inner 3
         else if (maj == 0.5){
          sum.stop<-sum(stops.route.i[2:4,j],na.rm=TRUE)
          not.na<-length((is.na(stops.route.i[2:4,j])[is.na(stops.route.i[2:4,j])==FALSE]))
          new.maj<-sum.stop/not.na
          if(new.maj < 0.5){
            stop.val.ij<-0
          } else if (new.maj > 0.5){
            stop.val.ij<-1
            
            #If still no clear majority, use focal value
          }  else if (new.maj == 0.5){
            stop.val.ij<-stops.route.i[3,j]
          }
        
          #Check for NA's as the returned value:
          if(is.na(stop.val.ij)==TRUE){
            rand<-runif(1,0,1)
            if(rand<0.5){
              stop.val.ij<-0
            } else if (rand>=0.5){
              stop.val.ij<-1
            } else {
              print("PROBLEMS!!!")
            }
            }
          
          } else {
          print("No Majority? Error...")
        }
      }
      #Create a vector for that RouteID value of all of the resulting stop values
      if(j==4){
        stop.row.route.i<-c(Route.i,yearfocal,stops$Aou[1],stop.val.ij)
      } else{
        stop.row.route.i<-c(stop.row.route.i,stop.val.ij)
      }
    }
    
    #Create a data.frame for all RouteID value of all of the resulting stop values
    if(i==1){
      stops.maj.output<-stop.row.route.i 
    }else{
      stops.maj.output<-rbind(stops.maj.output,stop.row.route.i)
    } 
  }
  
  nam_vec<-seq(1,nrow(stops.maj.output))
  rownames(stops.maj.output) <- nam_vec
  
  #Assign to global variable
  stops.maj.output<-as.data.frame(stops.maj.output)
  names(stops.maj.output)<-names(stops)
  
  assign(paste(c("stops.output.", yearfocal), collapse=""), stops.maj.output, envir = .GlobalEnv)
}

################################################################################


MajorityFocal(stops,1999,2003,2001)
summary(stops.output.2001)
write.csv(stops.output.2001, "WTSP.StopMajority.2001.csv", row.names = FALSE)

MajorityFocal(stops,2004,2008,2006)
write.csv(stops.output.2006, "WTSP.StopMajority.2006.csv", row.names = FALSE)

MajorityFocal(stops,2009,2013,2011)
write.csv(stops.output.2011, "WTSP.StopMajority.2011.csv", row.names = FALSE)




#####################################################################################
#####################################################################################

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

##############################
# # # # # # # # # # # # # # # 
#   THE EXPERIMENTAL ZONE   #
# # # # # # # # # # # # # # # 
##############################

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



################################################################################
################################################################################



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
p <- p + geom_histogram(binwidth=1,color="black", fill="white")
p <- p + facet_wrap(~ Routes.ij,ncol=10)  #, scale="free_y")
p <- p + theme_bw() + xlab("Count")
theme(axis.text.x  = element_text(angle=45,hjust = 1,vjust = 1)) + theme(panel.grid.major = element_line(colour = "black", size = 1.5))
p <- p + ggtitle("Route Counts in the US (Country Code 840)") + theme(plot.title = element_text(lineheight=.8, face="bold"))
print(p)


###############################################################
###############################################################
str(routes)
summary(routes)
routes<-routes[routes$Country!=124,]
routes<-routes[routes$Year>1998,]

unique(routes$Year)

routes.i<-data.frame(Year=routes$Year,RouteID=routes$RouteID)

tab.ii<-as.data.frame(table(routes.i))

length(unique(tab.ii$RouteID))

#Create logical steps of statements to identify valid routes and report list of routes
Route.V<-data.frame(RouteID=unique(tab.ii$RouteID),check.1=(rep(999,length(unique(tab.ii$RouteID)))),check.2=(rep(999,length(unique(tab.ii$RouteID)))),check.3=(rep(999,length(unique(tab.ii$RouteID)))))

###############################

RouteCheck<-function(tab.ii,Route.V,k){
  
  year.list<-c(2001,2006,2011)
  
  check.year.i<-c(year.list[k]-1,year.list[k],year.list[k]+1)
  check.year.twice.i<-c(year.list[k]-2,year.list[k]-1,year.list[k],year.list[k]+1,year.list[k]+2)
  
  #For each route:
  for(i in 1:length(Route.V$RouteID)){
    Route.i<-Route.V[i,1]
    
    tab.ii.check.ij<-tab.ii[tab.ii$RouteID==Route.i & tab.ii$Year%in%check.year.i,]
    tab.ii.check.twice.ij<-tab.ii[tab.ii$RouteID==Route.i & tab.ii$Year%in%check.year.twice.i,]
    
    if(sum(tab.ii.check.ij$Freq)>=1 & sum(tab.ii.check.twice.ij$Freq)>=2){
      Route.V[i,k+1]<-1
    } else {
      Route.V[i,k+1]<-0
    }
  }
  
  assign("Route.V",Route.V, envir = .GlobalEnv)
}
######################

RouteCheck(tab.ii,Route.V,1)
RouteCheck(tab.ii,Route.V,2)
RouteCheck(tab.ii,Route.V,3)

head(Route.V)

#Check that there are at least 2 hits in the 3 periods:
for(i in 1:length(Route.V$RouteID)){
  Route.i<-as.character(Route.V$RouteID[i])
  Route.i<-as.integer(Route.i)
  twice.check<-sum(Route.V[i,2:4])
  
  if(twice.check>=2){
    #print("Check!")
    
    if(i==1){
      Final.Routes<-(Route.i)
    } else {
      Final.Routes<-c(Final.Routes,Route.i)
    }
  } else {
    #print("Nope!")
  }
}

Final.Routes<-data.frame(RouteID=Final.Routes)

length(Final.Routes[,1])

write.csv(Final.Routes, file = "Final_Routes.csv")



################################################################################
################################################################################
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
sum(is.na(stops[,6:55])==TRUE)
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
