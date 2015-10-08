library(ggplot2) #ggplot, geom_bar
library(plyr) #count
library(ggthemes) #themes
library(grid)
library(chron)
library(plyr)
library(MASS)
library(RgoogleMaps)
library(RColorBrewer)
source('colorRampPaletteAlpha.R')

#Run the R script in the same place as the data or set to relevant directory
setwd("/Users/patrick/Dropbox/Spring 2015/")

#Read data, make it a data frame, reorder the weekdays
#crime <- read.csv("testcrime.csv")
crime <- read.csv("crime_full_loc.csv")
crime <- data.frame(crime)


#Change to Date format and get months, reorder months
crime$Month <- months(as.Date(crime$Occurred, "%m/%d/%Y"))
crime$Month <-factor(crime$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
crime$Weekdays <- weekdays(as.Date(crime$Occurred, "%m/%d/%Y"), abbreviate = TRUE)

#Shorten weekdays to make graphs easier to read
crime$Weekdays <-factor(crime$Weekdays, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
#crime$Weekdays <-factor(crime$Weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


#Generate IncidentGroups from Incident.Type
crime$IncidentGroups[grepl("ADMINISTRATIVE",crime$Incident.Type ) | grepl("ALARM RESPONSE", crime$Incident.Type) | grepl("DISORDERLY CONDUCT", crime$Incident.Type)| grepl("DISTURBANCE", crime$Incident.Type)| grepl("EH&S", crime$Incident.Type)| grepl("FIRE", crime$Incident.Type)| grepl("HARASSMENT", crime$Incident.Type)| grepl("HEALTH & SAFETY", crime$Incident.Type)| grepl("OBSCENE ACTIVITY", crime$Incident.Type)| grepl("OFFICER STATUS", crime$Incident.Type)| grepl("SERVICE", crime$Incident.Type)| grepl("TRAFFIC", crime$Incident.Type)| grepl("TRESPASS", crime$Incident.Type)| grepl("VANDALISM", crime$Incident.Type)| grepl("VEHICLE CODE", crime$Incident.Type)] <- "ADMIN/SERVICE/COMPLAINT"
crime$IncidentGroups[grepl("BURGLARY", crime$Incident.Type) | grepl("BURGLARY-MOTOR VEHICLE", crime$Incident.Type) | grepl("BURGLARY-OTHER", crime$Incident.Type) | grepl("EXTORTION", crime$Incident.Type) | grepl("FRAUD", crime$Incident.Type) | grepl("IDENTITY THEFT", crime$Incident.Type) | grepl("PROPERTY", crime$Incident.Type) | grepl("ROBBERY", crime$Incident.Type) | grepl("Theft Petty-Plain", crime$Incident.Type) | grepl("THEFT-GRAND", crime$Incident.Type) | grepl("THEFT-GRAND AUTO", crime$Incident.Type) | grepl("THEFT-GRAND PERSON", crime$Incident.Type) | grepl("THEFT-MOTOR VEHICLE", crime$Incident.Type) | grepl("THEFT-PETTY", crime$Incident.Type) | grepl("THEFT-TRICK", crime$Incident.Type)] <- "THEFT"
crime$IncidentGroups[grepl("CHILD NEGLECT", crime$Incident.Type)] <- "NON-VIOLENT"
crime$IncidentGroups[grepl("NON-FORCIBLE SEX OFFENSE", crime$Incident.Type)] <- "SEXUAL ASSAULT"
crime$IncidentGroups[grepl("ALCOHO", crime$Incident.Type)| grepl("ALCOHOL", crime$Incident.Type) | grepl("NARCOTICS", crime$Incident.Type)] <- "SUBSTANCE ABUSE"
crime$IncidentGroups[grepl("ASSAULT", crime$Incident.Type) | grepl("BATTERY", crime$Incident.Type) | grepl("CRIMINAL THREATS", crime$Incident.Type) | grepl("DOMESTIC VIOLENCE", crime$Incident.Type) | grepl("FORCIBLE SEX OFFENSE", crime$Incident.Type) | grepl("INCIDENT", crime$Incident.Type) | grepl("LA MUNICIPAL CODE", crime$Incident.Type) | grepl("SUICIDE", crime$Incident.Type) | grepl("WARRANT", crime$Incident.Type) | grepl("WEAPONS", crime$Incident.Type) ] <- "VIOLENT ACT"


#Add a line break to make it easier to read
crime$IncidentGroups <- gsub("ADMIN/SERVICE/COMPLAINT", "ADMIN/SERVICE/\nCOMPLAINT", crime$IncidentGroups)


#Generate TimeGroups by converting time occurred into 24 hour format
crime$Hours <- strptime(crime$Time.Occurred, "%I:%M %p")
crime$Hours <- format(crime$Hours, "%H")
crime$TimeGroups[grepl(0, crime$Hours) | grepl(1, crime$Hours) | grepl(2, crime$Hours)] <- "12 AM - 3 AM"
crime$TimeGroups[grepl(3, crime$Hours) | grepl(4, crime$Hours)  | grepl(5, crime$Hours)] <- "3 AM - 6 AM"
crime$TimeGroups[grepl(6, crime$Hours) | grepl(7, crime$Hours) | grepl(8, crime$Hours)] <- "6 AM - 9 AM"
crime$TimeGroups[grepl(9, crime$Hours) | grepl(10, crime$Hours)  | grepl(11, crime$Hours)] <- "9 AM - 12 PM"
crime$TimeGroups[grepl(12, crime$Hours) | grepl(13, crime$Hours) | grepl(14, crime$Hours)] <- "12 PM - 3 PM"
crime$TimeGroups[grepl(15, crime$Hours) | grepl(16, crime$Hours)  | grepl(17, crime$Hours)] <- "3 PM - 6 PM"
crime$TimeGroups[grepl(18, crime$Hours) | grepl(19, crime$Hours) | grepl(20, crime$Hours)] <- "6 PM - 9 PM"
crime$TimeGroups[grepl(21, crime$Hours) | grepl(22, crime$Hours)  | grepl(23, crime$Hours)] <- "9 PM - 12 AM"

#Reorder time groups
crime$TimeGroups <- factor(crime$TimeGroups, levels = c("3 AM - 6 AM", "6 AM - 9 AM", "9 AM - 12 PM", "12 PM - 3 PM", "3 PM - 6 PM", "6 PM - 9 PM", "9 PM - 12 AM", "12 AM - 3 AM"))


crime$Weeknum [grepl("Mon", crime$Weekdays)] <- 1
crime$Weeknum [grepl("Tue", crime$Weekdays)] <- 2
crime$Weeknum [grepl("Wed", crime$Weekdays)] <- 3
crime$Weeknum [grepl("Thu", crime$Weekdays)] <- 4
crime$Weeknum [grepl("Fri", crime$Weekdays)] <- 5
crime$Weeknum [grepl("Sat", crime$Weekdays)] <- 6
crime$Weeknum [grepl("Sun", crime$Weekdays)] <- 7


#Subset with no XXXs, remove indicent types with errors
crime_nox <- crime[- grep("XXX", crime$AREA_COLOR),]
crime_nox <- crime_nox[- grep("XX", crime_nox$AREA_COLOR),]
crime_nox <- crime_nox[- grep("N/A", crime_nox$IncidentGroups),]

rawdata <- data.frame(as.numeric(crime_nox$Long), as.numeric(crime_nox$Lat))
names(rawdata) <- c("lon", "lat")
data <- as.matrix(rawdata)

# Reproduce William's original map
par(bg='black')
plot(data, cex=0.1, col="white", pch=16)

# Create heatmap with kde2d and overplot
k <- kde2d(data[,1], data[,2], n=500)
# Intensity from green to red
cols <- rev(colorRampPalette(brewer.pal(8, 'RdYlGn'))(100))
par(bg='white')
image(k, col=cols, xaxt='n', yaxt='n')
points(data, cex=0.1, pch=16)

# Mapping via RgoogleMaps
# Find map center and get map
center <- rev(sapply(rawdata, mean))
map <- GetMap(center=center, zoom=11)
# Translate original data
coords <- LatLon2XY.centered(map, rawdata$lat, rawdata$lon, 11)
coords <- data.frame(coords)

# Rerun heatmap
k2 <- kde2d(coords$newX, coords$newY, n=500)

# Create exponential transparency vector and add
alpha <- seq.int(0.5, 0.95, length.out=100)
alpha <- exp(alpha^6-1)
cols2 <- addalpha(cols, alpha)

# Plot
PlotOnStaticMap(map)
image(k2, col=cols2, add=T)
points(coords$newX, coords$newY, pch=16, cex=0.3)
