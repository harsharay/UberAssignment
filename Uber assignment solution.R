uber <- read.csv("Uber Request Data.csv", header= TRUE)
View(uber)
str(uber)
uber1 <- uber

View(uber1)
str(uber1)

uber1$Request.timestamp <- gsub("/", "-",uber1$Request.timestamp)

uber1$Request.timestamp <- paste(uber1$Request.timestamp,":01",sep="")

uber1$Request.timestamp <- as.POSIXlt(uber1$Request.timestamp, format="%d-%m-%Y %H:%M:%S")

# 11-07-2016 11:51:00 request time stamp eg
View(uber1)

#Converting drop time stamp into standard date format
uber1$Drop.timestamp <- gsub("/", "-",uber1$Drop.timestamp)
uber1$Drop.timestamp <- paste(uber1$Drop.timestamp,":01",sep="")
uber1$Drop.timestamp <- as.POSIXlt(uber1$Drop.timestamp, format="%d-%m-%Y %H:%M:%S")

#Adding date and time as seperate columns
uber1$RequestDate <- as.Date(uber1$Request.timestamp)
uber1$RequestTime <- format(uber1$Request.timestamp , "%H:%M:%S")
uber1$RequestHours <- format(uber1$Request.timestamp, "%H")
uber1$DropDate <- as.Date(uber1$Drop.timestamp)
uber1$DropTime <- format(uber1$Drop.timestamp , "%H:%M:%S")
#uber1$RequestTime <- as.POSIXct(uber1$RequestTime, format="%H:%M:%S")
#uber1$DropTime <- as.POSIXct(uber1$DropTime, format="%H:%M:%S")
uber1$DropHours <- format(uber1$Drop.timestamp, "%H")

#Adding time of the day based on the hours
uber1$RequestHours <- as.numeric(uber1$RequestHours)
uber1$DropHours <- as.numeric(uber1$DropHours)



library(dplyr) #for adding a column using case_when based on multiple ifelse conditions
library(tibble) #for adding a column after a particular coumn in the table

uber1 <- add_column(.data = uber1,RequestTimeofDay=case_when(
  between(uber1$RequestHours,04,07) ~"Earlymorning",
  between(uber1$RequestHours,08,12) ~"Morning",
  between(uber1$RequestHours,13,16) ~"Afternoon",
  between(uber1$RequestHours,17,19) ~"Evening",
  between(uber1$RequestHours,20,23) ~"Night",
  between(uber1$RequestHours,24,25) ~"Midnight",
  between(uber1$RequestHours,00,03) ~"Midnight"
 ),.after = "RequestDate")

#Adding day of week depending on the Request date
uber1 <- add_column(.data = uber1,RequestDayofWeek=weekdays(uber1$RequestDate),.after = "RequestDate")


#Converting Request time and request day into dactors
uber1$RequestTimeofDay <- as.factor(uber1$RequestTimeofDay)
uber1$RequestDayofWeek <- as.factor(uber1$RequestDayofWeek)

library(ggplot2) #for plotting the below graphs

labelling <- geom_text(stat="count", aes(label=..count..),position=position_stack(0.5))

#plot to show cabs at different times of day
ggplot(uber1, aes(x=RequestTimeofDay, fill=RequestTimeofDay))+geom_bar()+labelling

#plot for showing cabs from different pickup points
ggplot(uber1, aes(x=Pickup.point, fill=Pickup.point))+geom_bar()+labelling

#plot to show cabs from different pickup points from different intervals of day(e.g morning , afternoon etc)
ggplot(uber1, aes(x=RequestTimeofDay, fill=Pickup.point))+geom_bar()+labelling

#plot to show cabs from different pickup points on different times of days vs Status
#ggplot(uber1, aes(fill=RequestTimeofDay, y=Pickup.point, x= Status))+geom_bar(stat="identity", position = "dodge")

#plot to show cab bookings from different pickup points at different intervals of day(hours)
ggplot(uber1, aes(x=RequestHours, fill=Pickup.point))+geom_histogram(bins = 20, binwidth = 0.5)+labelling
ggplot(uber1, aes(x=RequestHours, fill=Pickup.point))+geom_histogram(bins = 20, binwidth = 0.5, position="dodge")+geom_line(stat="count")


#plot to show the trip status of uber cabs
ggplot(uber1, aes(x=Status, fill=Status))+geom_bar()+labelling

#plot to show hourly trips requested 
ggplot(uber1, aes(x=RequestHours))+geom_bar()+labelling

#plot to show trips getting completed from different pickup points
ggplot(uber1, aes(x=(Status=="Trip Completed"), fill=Pickup.point))+geom_bar()+labelling

#plot to show trips getting competed from different pickup points on different hours of day
#ggplot(uber1, aes(x=(Status=="Trip Completed"), fill=Pickup.point, y=RequestHours))+geom_histogram(stat="identity")

#plot for showing no of trips requested per hour vs status of each trip
ggplot(uber1, aes(x=RequestHours, fill=factor(Status)))+geom_bar()+labelling
ggplot(uber1, aes(x=RequestHours, fill=Status))+geom_histogram(bins = 20, binwidth = 0.5, position="dodge")+geom_line(stat="count")

#plot to show how many total requests are happening from different pickup points
ggplot(uber1, aes(x=Pickup.point, fill=factor(Pickup.point)))+geom_bar()+labelling

#plot to show requested hours with different pickup points
ggplot(uber1, aes(x=RequestHours, fill=factor(Pickup.point)))+geom_bar()+labelling

#plot to show different status of cab during different days of week
ggplot(uber1, aes(x=RequestDayofWeek, fill=Status))+geom_bar()+labelling

#plot to show pickup point vs status of the trips
ggplot(uber1, aes(x=Pickup.point, fill=Status))+geom_bar(position="dodge")+geom_text(aes(label=..count..), stat="count", position=position_dodge(width=1), vjust=-0.5)


#Adding columns  where the status of cab is Airport and trip completes, no cabs available and completed 
uber1$AirportTripComplete <- with(uber1, ifelse(Status=="Trip Completed" & Pickup.point=="Airport", 1, 0))
uber1$AirportTripNoCarsAvailable <- with(uber1, ifelse(Status=="No Cars Available" & Pickup.point=="Airport", 1, 0))
uber1$AirportCancelled <- with(uber1, ifelse(Status=="Cancelled" & Pickup.point=="Airport", 1, 0))

#Adding columns where pickup point is City and with trip completed, no cabs available and completed
uber1$CityTripComplete <- with(uber1, ifelse(Status=="Trip Completed" & Pickup.point=="City", 1, 0))
uber1$CityTripNoCarsAvailable <- with(uber1, ifelse(Status=="No Cars Available" & Pickup.point=="City", 1, 0))
uber1$CityCancelled <- with(uber1, ifelse(Status=="Cancelled" & Pickup.point=="City", 1, 0))

#Creating a new aggreagate value for no. of trips completed per hour and plotting the graph
CountAirportTripComplete <- aggregate(uber1$AirportTripComplete, list(uber1$RequestHours), FUN=sum)
View(CountAirportTripComplete)
colnames(CountAirportTripComplete) <- c("Hours", "No.ofTripscompleted")
AirportTripcompletedGraph <- ggplot(CountAirportTripComplete, aes(x=Hours, y=No.ofTripscompleted, fill=No.ofTripscompleted))+geom_bar(stat="identity")
AirportTripcompletedGraph+geom_text(aes(label=No.ofTripscompleted), vjust=-0.5)

#Creating aggregate value of cabs getting cancelled per hour from Airport and plotting the graph 
CountAirportTripCancelled <- aggregate(uber1$AirportCancelled, list(uber1$RequestHours), FUN= sum)
View(CountAirportTripCancelled)
colnames(CountAirportTripCancelled) <- c("Hours", "No.ofTripscancelled")
AirportTripcancelledGraph <- ggplot(CountAirportTripCancelled, aes(x=Hours, y=No.ofTripscancelled, fill=No.ofTripscancelled))+geom_bar(stat="identity")
AirportTripcancelledGraph+geom_text(aes(label=No.ofTripscancelled), vjust=-0.5)

#Creating aggreagate value of cabs showing no cabs available and plotting a graph
CountAirportTripNoCabsAvaialable <- aggregate(uber1$AirportTripNoCarsAvailable, list(uber1$RequestHours), FUN= sum)
View(CountAirportTripNoCabsAvaialable)
colnames(CountAirportTripNoCabsAvaialable) <- c("Hours", "NumofNoCabsAvialable")
AirportTripnocabsavailableGraph <- ggplot(CountAirportTripNoCabsAvaialable, aes(x=Hours, y=NumofNoCabsAvialable , fill= NumofNoCabsAvialable ))+geom_bar(stat="identity")
AirportTripnocabsavailable+geom_text(aes(label=NumofNoCabsAvialable), vjust=-0.5)

a1 <- AirportTripcompletedGraph+geom_text(aes(label=No.ofTripscompleted), vjust=-0.2)
a2 <- AirportTripcancelledGraph+geom_text(aes(label=No.ofTripscancelled), vjust=-0.2)
a3 <- AirportTripnocabsavailableGraph+geom_text(aes(label=NumofNoCabsAvialable), vjust=-0.2)
grid.arrange(a1,a2,a3, nrow=3, ncol=1)

#Creating aggregate values for trip completed for no. of hours and plotting graph
CountCityTripCompleted <- aggregate(uber1$CityTripComplete, list(uber1$RequestHours), FUN=sum)
View(CountCityTripCompleted)
colnames(CountCityTripCompleted) <- c("Hours", "CountCityTripCompleted")
CountCityTripCompletedGraph <- ggplot(CountCityTripCompleted, aes(x=Hours, y=CountCityTripCompleted, fill=CountCityTripCompleted))+geom_bar(stat="identity")
CountCityTripCompletedGraph+geom_text(aes(label=CountCityTripCompleted), vjust=-0.5)   

#Creating aggregate values for trip cancelled for no. of hours and plotting graph
CountCityTripCancelled <- aggregate(uber1$CityCancelled, list(uber1$RequestHours), FUN=sum)
View(CountCityTripCancelled)
colnames(CountCityTripCancelled) <- c("Hours", "CountCityTripCancelled")
CountCityTripCancelledGraph <- ggplot(CountCityTripCancelled, aes(x=Hours, y=CountCityTripCancelled, fill=CountCityTripCancelled))+geom_bar(stat="identity")
CountCityTripCancelledGraph+geom_text(aes(label=CountCityTripCancelled), vjust=-0.5)   


#Creating aggregate values for no cabs available for no. of hours and plotting graph
CountCityTripNoCabsaAvailable <- aggregate(uber1$CityTripNoCarsAvailable, list(uber1$RequestHours), FUN=sum)
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
Demand <- aggregate(Request.id ~ Pickup.point, uber1,length)
colnames(Demand) <- c("Pickup.point", "Totalbookings")
Demand

Supply <- aggregate(Request.id ~ Pickup.point+(Status=="Trip Completed"), uber1, length)
colnames(Supply) <- c("Pickup.point","TripStatus","CompletedTrips")
View(Supply) 
Supply <- Supply[-which(Supply$TripStatus=="FALSE"),]
Supply$TripStatus[which(Supply$TripStatus=="TRUE")] <- "Trip Completed"
#Supply is only the completed trips

Demandplot <- ggplot(Demand, aes(x=Pickup.point, y=Totalbookings, fill=Pickup.point))+geom_bar(stat="identity")+geom_text(aes(label=Totalbookings), position=position_stack(0.5))+theme_economist()
Supplyplot <- ggplot(Supply, aes(x=Pickup.point, y=CompletedTrips, fill=Pickup.point))+geom_bar(stat="identity")+geom_text(aes(label=CompletedTrips), position=position_stack(0.5))+theme_economist()
grid.arrange(Demandplot,Supplyplot, nrow=2)

#Hourly Demand
HourlyDemand <- aggregate(Request.id ~ Pickup.point+RequestHours, uber1, length)
View(HourlyDemand)
colnames(HourlyDemand) <- c("Pickup.point","RequestHours","Totalbookings")

#Hourly Supply
HourlySupply <- aggregate(Request.id ~ Pickup.point+RequestHours+Status, uber1, length)
View(HourlySupply)
colnames(HourlySupply) <- c("Pickup.point", "RequestHours", "Status", "CompletedTrips")
HourlySupply <-HourlySupply[-which(HourlySupply$Status=="No Cars Available"),]
HourlySupply <-HourlySupply[-which(HourlySupply$Status=="Cancelled"),]

HourlyDemandGraph <- ggplot(HourlyDemand, aes(fill=Pickup.point, y=Totalbookings, x=RequestHours))+geom_bar(stat="identity", position="dodge")+geom_line(stat="identity")
HourlySupplyGraph <- ggplot(HourlySupply, aes(x=RequestHours, y=CompletedTrips, fill=Pickup.point))+geom_bar(stat="identity", position="dodge")+geom_line(stat="identity")

grid.arrange(HourlyDemandGraph,HourlySupplyGraph,nrow=2)
