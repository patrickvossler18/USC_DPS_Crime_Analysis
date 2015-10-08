library(ggplot2) #ggplot, geom_bar
library(plyr) #count
library(ggthemes) #theme_wsj()
library(grid)
library(chron)
library(plyr)

#Run the R script in the same place as the data or set to relevant directory
setwd("/Users/patrick/Dropbox/Spring 2015/")

#Read data, make it a data frame, reorder the weekdays
crime <- read.csv("testcrime.csv")
crime <- data.frame(crime)
#crime$Weekdays <-factor(crime$Weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Change to Date format and get months, reorder months
crime$Month <- months(as.Date(crime$Occurred, "%m/%d/%Y"))
crime$Month <-factor(crime$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
crime$Weekdays <- weekdays(as.Date(crime$Occurred, "%m/%d/%Y"), abbreviate = TRUE)
crime$Weekdays <-factor(crime$Weekdays, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#crime$TimeGroups <- factor(crime$TimeGroups, levels = c("3 AM - 6 AM", "6 AM - 9 AM", "9 AM - 12 PM", "12 PM - 3 PM", "3 PM - 6 PM", "6 PM - 9 PM", "9 PM - 12 AM", "12 AM - 3 AM"))

crime$IncidentGroups[grepl("ADMINISTRATIVE",crime$Incident.Type ) | grepl("ALARM RESPONSE", crime$Incident.Type) | grepl("DISORDERLY CONDUCT", crime$Incident.Type)| grepl("DISTURBANCE", crime$Incident.Type)| grepl("EH&S", crime$Incident.Type)| grepl("FIRE", crime$Incident.Type)| grepl("HARASSMENT", crime$Incident.Type)| grepl("HEALTH & SAFETY", crime$Incident.Type)| grepl("OBSCENE ACTIVITY", crime$Incident.Type)| grepl("OFFICER STATUS", crime$Incident.Type)| grepl("SERVICE", crime$Incident.Type)| grepl("TRAFFIC", crime$Incident.Type)| grepl("TRESPASS", crime$Incident.Type)| grepl("VANDALISM", crime$Incident.Type)| grepl("VEHICLE CODE", crime$Incident.Type)] <- "ADMIN/SERVICE/COMPLAINT"
crime$IncidentGroups[grepl("BURGLARY", crime$Incident.Type) | grepl("BURGLARY-MOTOR VEHICLE", crime$Incident.Type) | grepl("BURGLARY-OTHER", crime$Incident.Type) | grepl("EXTORTION", crime$Incident.Type) | grepl("FRAUD", crime$Incident.Type) | grepl("IDENTITY THEFT", crime$Incident.Type) | grepl("PROPERTY", crime$Incident.Type) | grepl("ROBBERY", crime$Incident.Type) | grepl("Theft Petty-Plain", crime$Incident.Type) | grepl("THEFT-GRAND", crime$Incident.Type) | grepl("THEFT-GRAND AUTO", crime$Incident.Type) | grepl("THEFT-GRAND PERSON", crime$Incident.Type) | grepl("THEFT-MOTOR VEHICLE", crime$Incident.Type) | grepl("THEFT-PETTY", crime$Incident.Type) | grepl("THEFT-TRICK", crime$Incident.Type)] <- "THEFT"
crime$IncidentGroups[grepl("CHILD NEGLECT", crime$Incident.Type)] <- "NON-VIOLENT"
crime$IncidentGroups[grepl("NON-FORCIBLE SEX OFFENSE", crime$Incident.Type)] <- "SEXUAL ASSAULT"
crime$IncidentGroups[grepl("ALCOHO", crime$Incident.Type)| grepl("ALCOHOL", crime$Incident.Type) | grepl("NARCOTICS", crime$Incident.Type)] <- "SUBSTANCE ABUSE"
crime$IncidentGroups[grepl("ASSAULT", crime$Incident.Type) | grepl("BATTERY", crime$Incident.Type) | grepl("CRIMINAL THREATS", crime$Incident.Type) | grepl("DOMESTIC VIOLENCE", crime$Incident.Type) | grepl("FORCIBLE SEX OFFENSE", crime$Incident.Type) | grepl("INCIDENT", crime$Incident.Type) | grepl("LA MUNICIPAL CODE", crime$Incident.Type) | grepl("SUICIDE", crime$Incident.Type) | grepl("WARRANT", crime$Incident.Type) | grepl("WEAPONS", crime$Incident.Type) ] <- "VIOLENT ACT"
crime$IncidentGroups <- gsub("ADMIN/SERVICE/COMPLAINT", "ADMIN/SERVICE/\nCOMPLAINT", crime$IncidentGroups)

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
crime$TimeGroups <- factor(crime$TimeGroups, levels = c("3 AM - 6 AM", "6 AM - 9 AM", "9 AM - 12 PM", "12 PM - 3 PM", "3 PM - 6 PM", "6 PM - 9 PM", "9 PM - 12 AM", "12 AM - 3 AM"))
#Subset with no XXXs
#crime_nox <- crime[- grep("XXX", crime$AREA_COLOR),]
#crime_nox <- crime[- grep("XX", crime$AREA_COLOR),]
#crime_all <- count(crime_nox, vars=c("Incident.Type","Weekdays", "Month"))
#crime_all_sum <- ddply(crime_all,.(Weekdays,Incident.Type),summarize,sums=sum(freq))
#crime_nox <- subset(crime, crime$AREA_COLOR != XXX, select = c("IncidentGroups","Weekdays", "Month", "freq"))

#Subset Theft only
crime_theft <- count(crime_nox, vars=c("IncidentGroups", "Weekdays", "Month"))
crime_theft <- subset(crime_theft, crime_theft$IncidentGroups == "THEFT", select = c("IncidentGroups","Weekdays", "Month", "freq"))
theft_sum <- ddply(crime_theft,.(Weekdays),summarize,sums=sum(freq))
#Subset Greek
#crime_greek <- count(crime_nox, vars=c("IncidentGroups","Weekdays", "Month"))
#crime_greek <- subset(crime_greek, crime_greek$AREA_COLOR == "K", select = c("Incident.Type","Weekdays", "Month", "freq", "AREA_COLOR") )
#crime_greek_sum <- ddply(crime_greek,.(Weekdays, IncidentGroups),summarize,sums=sum(freq))
#Frequency by Time Groupings
crime_time <- count(crime_nox, vars=c("IncidentGroups", "TimeGroups"))

#Frequency by Time and Location
#crime_timelocation <- count(crime_nox, vars=c("IncidentGroups", "TimeGroups", "AREA_COLOR"))

#Frequency and Day of the Week, Theft Only
##########################

#Graph bar plot x= Weekday y= freq of theft
final_theft <- ggplot(crime_theft)+geom_bar(aes(x=Weekdays, y=freq, label = freq), stat="identity", fill = "#01516c")

#Economist theme
final_theft + theme_economist(stata = TRUE) + scale_fill_economist( stata = TRUE ) + labs( title = "Thefts at USC by Weekday \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 1, vjust = 0.5), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(size = 14,hjust = 0.5))+
  geom_text(data= theft_sum, aes(x=Weekdays, y=sums,label = theft_sum$sums), vjust= 0, size =4)


#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday_theft_jameson.png", width = 10, height = 10)

########################## 
#Frequency and Day of the Week, All Crime
########################## 


#Graph bar plot x= weekday y= freq of all crime
final_allcrime <- ggplot(crime_all)+geom_bar(aes(x=Weekdays, y=freq), stat="identity", fill = "#01516c")+facet_wrap(~IncidentGroups, nrow = 2)

#Economist theme
final_allcrime + theme_economist(stata = TRUE) + scale_fill_economist( stata = TRUE ) + labs( title = "Crimes at USC by Weekday \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 1, vjust = 0.5), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(size = 14,hjust = 0.5))
#+geom_text(data= crime_all_sum, aes(x=Weekdays, y=sums,label = crime_all_sum$sums), vjust= 0, size =4)

#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday_allcrime_jameson.png", width = 10, height = 10)

##########################
#Frequency and Time Groupings by Crime Groupings
##########################

#Graph bar plot x= Time Groupings y= Frequency by Crime groups
final_time <- ggplot(crime_time)+geom_bar(aes(x=TIME_GROUP, y=freq), stat="identity", fill = "#01516c")+facet_wrap(~IncidentGroups, ncol = 3)

#Economist theme
final_time + theme_economist(stata = TRUE) + scale_fill_economist( stata = TRUE ) + labs( title = "Crimes at USC by Time \n", x= "Time Groups", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10, angle = -90, hjust = 1, vjust = 0.5), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(size = 14,hjust = 0.5))

#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday_timegroups_full_jameson.png", width = 10, height = 10)
