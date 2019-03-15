#Kathleen Hicks
#For ESNERR
#Precipitation data from UC Integrated Pest Control Management
# http://ipm.ucanr.edu/WEATHER/wxactstnames.html
#Upwelling data from Brent Hughes on June 26, 2018
#Last updated 11/19/2018

rm(list=ls()); graphics.off()

#setwd('C:/my_directory/Hicks_ESNERR_R_Handoff') #Set your own working directory
setwd("~/Box/ElkhornSlough") #Molly's directory

#Import the precipitation
precip = read.csv("Castroville_precip_1989_2018.csv", stringsAsFactors = FALSE)

#Indicate that the date column should be read as dates
precip$Date <- as.Date(precip$Date,"%m/%d/%Y") #because the source csv date format is m/d/yyyy
head(precip)


#Import the upwelling data
upwelling = read.csv("Monterey_Upwelling_Indices.csv", stringsAsFactors = FALSE)
upwelling$Date <- as.Date(upwelling$Date,"%m/%d/%Y") #because the source csv date format is m/d/yyyy
head(upwelling)


png(file="PrecipTrends.png",width=700,height=400, res = 150)
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(x = precip$Date, y= precip$Precip, type = "l", 
     xlim = c(min(precip$Date), max(precip$Date)), ann = FALSE)
mtext(side = 2, text = "Precipitation (cm)", line = 2) #puts y-axis label on as text
abline(v = as.Date(c("1995-01-01", "2005-01-01", "2015-01-01")), col = "darkgray") #puts vertical lines every 5 years
dev.off()



png(file="UpwellingTrends.png",width=700,height=400, res = 150)
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(x = upwelling$Date, y = upwelling$Upwelling, type = "l", 
     xlim = c(min(precip$Date), max(precip$Date)), ann = FALSE)
mtext(side = 2, text = "Upwelling (cms/100m)", line = 2) #puts y-axis label on as text
abline(v = as.Date(c("1995-01-01", "2005-01-01", "2015-01-01")), col = "darkgray") #puts vertical lines every 5 years
dev.off()
