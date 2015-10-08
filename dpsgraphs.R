library(ggplot2) #ggplot, geom_bar
library(plyr) #count
library(ggthemes) #theme_wsj()
library(grid)

#Run the R script in the same place as the data or set to relevant directory
setwd("/Users/patrick/Dropbox/Spring 2015/")

#Read data, make it a data frame, reorder the weekdays
crime <- read.csv("CleanCrimes.csv")
crime <- data.frame(crime)
crime$Weekdays <-factor(crime$Weekdays, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Use plyr's count function to create a frequency variable
graph <- count(crime, vars=c("GROUP1", "Weekdays"))

#Graph bar plot with facet wrap. 8 categories so two rows with four columns
final <- ggplot(graph)+geom_bar(aes(x=Weekdays, y=freq), stat="identity")+facet_wrap(~GROUP1, ncol = 4)

#Change theme to WSJ, add x and y labels, add y scale ticks every 5, rotate x text -90 to make them easier to read
final + theme_wsj()+ scale_color_wsj("colors6", "") + labs( title = "Crime at USC from December - February \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10,angle=-90,hjust = 0), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), panel.margin = unit(1.5, "lines"), plot.title = element_text(hjust = 0.5))

#Save as .png
ggsave(file = "weekday&crime_wsj.png", width = 10, height = 10)

#Economist theme
final + theme_economist(stata = TRUE) + scale_fill_economist( stata = TRUE) + labs( title = "Crime at USC from December - February \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10,angle=-90,hjust = 0), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(size = 12,hjust = 0.5))

#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday&crime_economist.png", width = 10, height = 10)


#Tableau Theme
final + theme_igray() + scale_fill_tableau() + labs( title = "Crime at USC from December - February \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10,angle=-90,hjust = 0), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(hjust = 0.5))

#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday&crime_tableau.png", width = 10, height = 10)

#FiveThirtyEight
final + theme_fivethirtyeight() + scale_color_fivethirtyeight() + labs( title = "Crime at USC from December - February \n", x= "Weekday", y = "Frequency") +
  scale_y_continuous(breaks =seq(0,100,5)) +
  theme(axis.text.x=element_text(size = 10,angle=-90,hjust = 0), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12, vjust = 1.5), panel.margin = unit(1.5, "lines"), plot.title = element_text(hjust = 0.5))

#Save as .png.
#Height and width measured in cm? not pixels
ggsave(file = "weekday&crime_fivethirtyeight.png", width = 10, height = 10)
  