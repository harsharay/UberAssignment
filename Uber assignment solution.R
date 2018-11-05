uber <- read.csv("Uber Request Data.csv", header= TRUE) #reading from thr csv file
View(uber)
str(uber)

#Converting request data in time stamp to standard format
uber$Request.timestamp <- gsub("/", "-",uber$Request.timestamp)
uber$Request.timestamp <- paste(uber$Request.timestamp,":01",sep="")
uber$Request.timestamp <- as.POSIXlt(uber$Request.timestamp, format="%d-%m-%Y %H:%M:%S")

View(uber)

#Converting drop time stamp into standard date format
uber$Drop.timestamp <- gsub("/", "-",uber$Drop.timestamp)
uber$Drop.timestamp <- paste(uber$Drop.timestamp,":01",sep="")
uber$Drop.timestamp <- as.POSIXlt(uber$Drop.timestamp, format="%d-%m-%Y %H:%M:%S")

View(uber)

#Adding date and time as seperate columns
uber$RequestDate <- as.Date(uber$Request.timestamp)
uber$RequestTime <- format(uber$Request.timestamp , "%H:%M:%S") #just for displaying the time,not converting date to posixct format as we will be working on hours only. Hence extracting hours only from this
uber$RequestHours <- format(uber$Request.timestamp, "%H")
uber$DropDate <- as.Date(uber$Drop.timestamp)
uber$DropTime <- format(uber$Drop.timestamp , "%H:%M:%S") #just for displaying the time,not converting date to posixct format as we will be working on hours only. Hence extracting hours only from this
uber$DropHours <- format(uber$Drop.timestamp, "%H")

#Adding time of the day based on the hours
uber$RequestHours <- as.numeric(uber$RequestHours)
uber$DropHours <- as.numeric(uber$DropHours)

library(dplyr) #for adding a column using case_when based on multiple ifelse conditions
library(tibble) #for adding a column after a particular coumn in the table

uber <- add_column(.data = uber,RequestTimeofDay=case_when(
  between(uber$RequestHours,04,07) ~"Earlymorning",
  between(uber$RequestHours,08,12) ~"Morning",
  between(uber$RequestHours,13,16) ~"Afternoon",
  between(uber$RequestHours,17,19) ~"Evening",
  between(uber$RequestHours,20,23) ~"Night",
  between(uber$RequestHours,24,25) ~"Midnight",
  between(uber$RequestHours,00,03) ~"Midnight"
 ),.after = "RequestDate")

#Adding day of week depending on the Request date
uber <- add_column(.data = uber,RequestDayofWeek=weekdays(uber$RequestDate),.after = "RequestDate")


#Converting Request time and request day into dactors
uber$RequestTimeofDay <- as.factor(uber$RequestTimeofDay)
uber$RequestDayofWeek <- as.factor(uber$RequestDayofWeek)

library(ggplot2) #for plotting the below graphs

#Creating labelling object which will be used for geom_text
labelling <- geom_text(stat="count", aes(label=..count..),position=position_stack(0.5))

#plot to show number of cabs at different times of day
ggplot(uber, aes(x=RequestTimeofDay, fill=RequestTimeofDay))+geom_bar()+labelling

#plot for showing number of cabs being request during the day at different hours
ggplot(uber, aes(x=RequestHours))+geom_bar()+labelling

#plot to show cabs from different pickup points from different intervals of day(e.g morning , afternoon etc)
ggplot(uber, aes(x=RequestTimeofDay, fill=Pickup.point))+geom_bar()+labelling

#plot for showing cabs from different pickup points
ggplot(uber, aes(x=Pickup.point, fill=Pickup.point))+geom_bar()+labelling

#plot to show the trip status of uber cabs
ggplot(uber, aes(x=Status, fill=Status))+geom_bar()+labelling

#plot to show pickup point vs status of the trips
ggplot(uber, aes(x=Pickup.point, fill=Status))+geom_bar(position="dodge")+geom_text(aes(label=..count..), stat="count", position=position_dodge(width=1), vjust=-0.5)

#plot to show cab bookings from different pickup points at different intervals of day(hours)  
ggplot(uber, aes(x=RequestHours, fill=Pickup.point))+geom_histogram(bins = 20, binwidth = 0.5, position="dodge")+geom_text(aes(label=..count..), stat="count", position = position_dodge(width = 0.5), vjust=-0.5)
ggplot(uber, aes(x=RequestHours, fill=Pickup.point))+geom_histogram(bins = 20, binwidth = 0.5, position="dodge")+geom_line(stat="count")

#plot for showing no of trips requested per hour vs status of each trip
ggplot(uber, aes(x=RequestHours, fill=Status))+geom_bar()+labelling
ggplot(uber, aes(x=RequestHours, fill=Status))+geom_histogram(bins = 20, binwidth = 0.5, position="dodge")+geom_line(stat="count")

#Adding columns  where the status of cab is Airport and trip completes, no cabs available and completed 
uber$AirportTripComplete <- with(uber, ifelse(Status=="Trip Completed" & Pickup.point=="Airport", 1, 0))
uber$AirportTripNoCarsAvailable <- with(uber, ifelse(Status=="No Cars Available" & Pickup.point=="Airport", 1, 0))
uber$AirportCancelled <- with(uber, ifelse(Status=="Cancelled" & Pickup.point=="Airport", 1, 0))

#Adding columns where pickup point is City and with trip completed, no cabs available and completed
uber$CityTripComplete <- with(uber, ifelse(Status=="Trip Completed" & Pickup.point=="City", 1, 0))
uber$CityTripNoCarsAvailable <- with(uber, ifelse(Status=="No Cars Available" & Pickup.point=="City", 1, 0))
uber$CityCancelled <- with(uber, ifelse(Status=="Cancelled" & Pickup.point=="City", 1, 0))

#Creating a new aggreagate value for no. of trips completed per hour and plotting the graph
CountAirportTripComplete <- aggregate(uber$AirportTripComplete, list(uber$RequestHours), FUN=sum)
View(CountAirportTripComplete)
colnames(CountAirportTripComplete) <- c("Hours", "No.ofTripscompleted")
AirportTripcompletedGraph <- ggplot(CountAirportTripComplete, aes(x=Hours, y=No.ofTripscompleted, fill=No.ofTripscompleted))+geom_bar(stat="identity")
AirportTripcompletedGraph+geom_text(aes(label=No.ofTripscompleted), vjust=-0.5)

#Creating aggregate value of cabs getting cancelled per hour from Airport and plotting the graph 
CountAirportTripCancelled <- aggregate(uber$AirportCancelled, list(uber$RequestHours), FUN= sum)
View(CountAirportTripCancelled)
colnames(CountAirportTripCancelled) <- c("Hours", "No.ofTripscancelled")
AirportTripcancelledGraph <- ggplot(CountAirportTripCancelled, aes(x=Hours, y=No.ofTripscancelled, fill=No.ofTripscancelled))+geom_bar(stat="identity")
AirportTripcancelledGraph+geom_text(aes(label=No.ofTripscancelled), vjust=-0.5)

#Creating aggreagate value of cabs showing no cabs available and plotting a graph
CountAirportTripNoCabsAvaialable <- aggregate(uber$AirportTripNoCarsAvailable, list(uber$RequestHours), FUN= sum)
View(CountAirportTripNoCabsAvaialable)
colnames(CountAirportTripNoCabsAvaialable) <- c("Hours", "NumofNoCabsAvialable")
AirportTripnocabsavailableGraph <- ggplot(CountAirportTripNoCabsAvaialable, aes(x=Hours, y=NumofNoCabsAvialable , fill= NumofNoCabsAvialable ))+geom_bar(stat="identity")
AirportTripnocabsavailableGraph+geom_text(aes(label=NumofNoCabsAvialable), vjust=-0.5)

library(gridExtra) #for plotting multiple graphs in a grid
a1 <- AirportTripcompletedGraph+geom_text(aes(label=No.ofTripscompleted), vjust=-0.2)
a2 <- AirportTripcancelledGraph+geom_text(aes(label=No.ofTripscancelled), vjust=-0.2)
a3 <- AirportTripnocabsavailableGraph+geom_text(aes(label=NumofNoCabsAvialable), vjust=-0.2)
grid.arrange(a1,a2,a3, nrow=3, ncol=1)


#plot to show trips getting completed from different pickup points
ggplot(uber, aes(x=(Status=="Trip Completed"), fill=Pickup.point))+geom_bar()+labelling

#plot to show how many total requests are happening from different pickup points
ggplot(uber, aes(x=Pickup.point, fill=factor(Pickup.point)))+geom_bar()+labelling

#plot to show requested hours with different pickup points
ggplot(uber, aes(x=RequestHours, fill=factor(Pickup.point)))+geom_bar()+labelling

#plot to show different status of cab during different days of week
ggplot(uber, aes(x=RequestDayofWeek, fill=Status))+geom_bar()+labelling



#Creating aggregate values for trip completed for no. of hours and plotting graph
CountCityTripCompleted <- aggregate(uber$CityTripComplete, list(uber$RequestHours), FUN=sum)
View(CountCityTripCompleted)
colnames(CountCityTripCompleted) <- c("Hours", "CountCityTripCompleted")
CountCityTripCompletedGraph <- ggplot(CountCityTripCompleted, aes(x=Hours, y=CountCityTripCompleted, fill=CountCityTripCompleted))+geom_bar(stat="identity")
CountCityTripCompletedGraph+geom_text(aes(label=CountCityTripCompleted), vjust=-0.5)   

#Creating aggregate values for trip cancelled for no. of hours and plotting graph
CountCityTripCancelled <- aggregate(uber$CityCancelled, list(uber$RequestHours), FUN=sum)
View(CountCityTripCancelled)
colnames(CountCityTripCancelled) <- c("Hours", "CountCityTripCancelled")
CountCityTripCancelledGraph <- ggplot(CountCityTripCancelled, aes(x=Hours, y=CountCityTripCancelled, fill=CountCityTripCancelled))+geom_bar(stat="identity")
CountCityTripCancelledGraph+geom_text(aes(label=CountCityTripCancelled), vjust=-0.5)   


#Creating aggregate values for no cabs available for no. of hours and plotting graph
CountCityTripNoCabsaAvailable <- aggregate(uber$CityTripNoCarsAvailable, list(uber$RequestHours), FUN=sum)
View(CountCityTripNoCabsaAvailable)
colnames(CountCityTripNoCabsaAvailable) <- c("Hours", "CountCitynocabsAvailable")
CountCityTripNoCabsaAvailableGraph <- ggplot(CountCityTripNoCabsaAvailable, aes(x=Hours, y=CountCitynocabsAvailable, fill=CountCitynocabsAvailable))+geom_bar(stat="identity")
CountCityTripNoCabsaAvailableGraph+geom_text(aes(label=CountCitynocabsAvailable), vjust=-0.5)   

c1 <- CountCityTripCompletedGraph+geom_text(aes(label=CountCityTripCompleted), vjust=-0.2)   
c2 <- CountCityTripCancelledGraph+geom_text(aes(label=CountCityTripCancelled), vjust=-0.2)   
c3 <- CountCityTripNoCabsaAvailableGraph+geom_text(aes(label=CountCitynocabsAvailable), vjust=-0.2)   

grid.arrange(c1,c2,c3,nrow=3,ncol=1)


#Supply and Demand
#Supply, for a particular pickup point can be considered as the total number of cabs bookings and demand can be considered as total number of completed cab trips
Demand <- aggregate(Request.id ~ Pickup.point, uber,length)
colnames(Demand) <- c("Pickup.point", "Totalbookings")
Demand
View(Demand)

Supply <- aggregate(Request.id ~ Pickup.point+(Status=="Trip Completed"), uber, length)
colnames(Supply) <- c("Pickup.point","TripStatus","CompletedTrips")
View(Supply) 
Supply <- Supply[-which(Supply$TripStatus=="FALSE"),]
Supply$TripStatus[which(Supply$TripStatus=="TRUE")] <- "Trip Completed"
#Supply is only the completed trips

TotalGap <- Demand$Totalbookings - Supply$CompletedTrips
TotalDemandSupply <- data.frame(Demand,Supply)
View(TotalDemandSupply)
TotalDemandSupply <- TotalDemandSupply[,-which(colnames(TotalDemandSupply)=="Pickup.point.1")]
TotalDemandSupply$PercentofGap <- round((TotalDemandSupply$CompletedTrips/TotalDemandSupply$Totalbookings)*100,2)
View(TotalDemandSupply)
TotalDemandSupply$SupplyDemandGap <- TotalDemandSupply$Totalbookings - TotalDemandSupply$CompletedTrips


Demandplot <- ggplot(Demand, aes(x=Pickup.point, y=Totalbookings, fill=Pickup.point))+geom_bar(stat="identity")+geom_text(aes(label=Totalbookings), position=position_stack(0.5))
Supplyplot <- ggplot(Supply, aes(x=Pickup.point, y=CompletedTrips, fill=Pickup.point))+geom_bar(stat="identity")+geom_text(aes(label=CompletedTrips), position=position_stack(0.5))
grid.arrange(Demandplot,Supplyplot, nrow=2)

#Total Supply and Deman Graph
TotalDemandSupplyGraph <- ggplot(TotalDemandSupply, aes(x=Pickup.point, y=SupplyDemandGap, fill=Pickup.point))+geom_bar(stat="identity", position="dodge")+geom_text(aes(label=SupplyDemandGap), position=position_dodge(width = 1), vjust=-0.5)
TotalDemandSupplyGraph

#Hourly Demand
HourlyDemand <- aggregate(Request.id ~ Pickup.point+RequestHours, uber, length)
View(HourlyDemand)
colnames(HourlyDemand) <- c("Pickup.point","RequestHours","Totalbookings")

#Hourly Supply
HourlySupply <- aggregate(Request.id ~ Pickup.point+RequestHours+Status, uber, length)
View(HourlySupply)
colnames(HourlySupply) <- c("Pickup.point", "RequestHours", "Status", "CompletedTrips")
HourlySupply <-HourlySupply[-which(HourlySupply$Status=="No Cars Available"),]
HourlySupply <-HourlySupply[-which(HourlySupply$Status=="Cancelled"),]

HourlyStats <- data.frame(HourlyDemand,HourlySupply)
View(HourlyStats)
HourlyStats <- HourlyStats[,-which(colnames(HourlyStats)=="Pickup.point.1")]
HourlyStats <- HourlyStats[,-which(colnames(HourlyStats)=="RequestHours.1")]
HourlyStats$SupplyDemandGap <- HourlyStats$Totalbookings - HourlyStats$CompletedTrips
HourlyStats$PercentofGap <- round((HourlyStats$SupplyDemandGap/HourlyStats$Totalbookings)*100,2)
View(HourlyStats)

HourlyDemandGraph <- ggplot(HourlyDemand, aes(x=RequestHours,  y=Totalbookings, fill=Pickup.point ))+geom_bar(stat="identity", position="dodge")+geom_text(aes(label=Totalbookings), position=position_dodge(width=1), vjust=-0.5)
HourlySupplyGraph <- ggplot(HourlySupply, aes(x=RequestHours, y=CompletedTrips, fill=Pickup.point))+geom_bar(stat="identity", position="dodge")+geom_text(aes(label=CompletedTrips), position=position_dodge(width=1), vjust=-0.5)
grid.arrange(HourlyDemandGraph,HourlySupplyGraph,nrow=2)

SupplyDemandGapGraph <- ggplot(HourlyStats, aes(x=RequestHours, y= SupplyDemandGap, fill=Pickup.point))+geom_bar(stat="identity", position="dodge")+geom_text(aes(label=SupplyDemandGap), position=position_dodge(width = 1), vjust=-0.5)
SupplyDemandGapGraph

#Calculating the time durations for trips completed from different pickup points
TripCompleted <- subset(x = uber, Status=="Trip Completed")
View(TripCompleted)
summary(TripCompleted$Status)
str(TripCompleted)
TripCompleted$TripDuration <- round(TripCompleted$Drop.timestamp - TripCompleted$Request.timestamp,2)

AirportTripDuration <- subset(TripCompleted,TripCompleted$Pickup.point=="Airport")
View(AirportTripDuration)
AirportTripDuration$TripDuration <- as.numeric(AirportTripDuration$TripDuration)
AirportTripDuration$PeakHours <- ifelse(AirportTripDuration$RequestHours>=17 & AirportTripDuration$RequestHours<=22, "yes", "no")
View(AirportTripDuration)
round(mean(AirportTripDuration$TripDuration[which(AirportTripDuration$PeakHours=="yes")]),2) #52.63 mins
round(mean(AirportTripDuration$TripDuration[which(AirportTripDuration$PeakHours=="no")]),2) #52.03 mins


CityTripDuration <- subset(TripCompleted,TripCompleted$Pickup.point=="City")
View(CityTripDuration)
CityTripDuration$TripDuration <- as.numeric(CityTripDuration$TripDuration)
CityTripDuration$PeakHours <- ifelse(CityTripDuration$RequestHours>=4 & CityTripDuration$RequestHours<=10, "yes", "no")
View(CityTripDuration)
round(mean(CityTripDuration$TripDuration[which(CityTripDuration$PeakHours=="yes")]),2) #53.43 mins
round(mean(CityTripDuration$TripDuration[which(CityTripDuration$PeakHours=="no")]),2) #52.03 mins
