#Kathleen Hicks
#For ESNERR
#Plots smoothed trends for all sites in a region
#Uses "ts" and "decompose" from TSA package
#Last updated 11/19/18

rm(list=ls()); graphics.off()

#setwd('C:/my_directory/Hicks_ESNERR_R_Handoff') #Change to your own working directory
dat = read.csv("MasterQueryOct2018.csv", stringsAsFactors = FALSE) #Change as necessary for most updated csv

#Change the date format
#This tells R that the csv has date as dd-mmm-yy
#will show the date as yyyy-mm-dd
dat$Date <- as.Date(dat$Date,"%d-%b-%y")

#Make a new column that has just the year from the date
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")
head(dat)

#Make a list of the parameters that will be graphed
params = c("Ammonia.as.N.mg.L", "Nitrate.as.N..mg.L.", "MCCL.Phosphate..mg.L.", "DO.SAT..")

#Import package TSA first if needed
#This is what I used for smoothing trends
pacman::p_load(TSA)

#Group the sites according to location or other similarities
sites.all = c(4, 5, 8, 13, 18) 


#Make a list of the regions
groups = list(sites.all)


#################################   AMMONIA   #############################################
for (i in groups) {
  color = 1
  sites.x = groups[groups = i] #picks one region from the list
  #Images are saved with names beginning with the number of the first site in that region
  #png(file= paste0(i,"Ammonia", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  #Next line pulls up empty axes
  #Change ylim to accomodate different parameters
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 2), ann = FALSE) 
  mtext(side = 2, text = "Ammonia as N (mg/L)", line = 2) #puts y-axis labels on as text
  abline(h = 0.1, lty = 3) #Puts a dashed horizontal line on the graph at the threshold for ammonia
  abline(v = c(1995, 2005, 2015), col = "darkgray") #Puts vertical lines on the graph every 5 years
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1)) #makes legend with up to 6 colors
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ] #Takes data from one site at a time
    pseries = as.numeric(sub$Ammonia.as.N.mg.L) #Cleaning ammonia values
    pseries <- na.omit(pseries) #Cleaning ammonia values
    #Next line makes a "time series" of all of the ammonia values, 
    #starting in the earliest year and ending in the most recent
    #Frequency = 12 makes each month a season
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12) 
    #Next line separates the trend into seasonal, random, and trend components
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2) #Graphs just the trend lines on the axes
    color = color + 1 #will make each successive site a different color
  }
  
  #dev.off()
}

################################# AMMONIA (SMALLER AXIS) #############################################
#Does the same thing as AMMONIA but with 1/2 size y-axis for sites with lower ammonia

for (i in groups) {
  color = 1
  sites.x = groups[groups = i]
  png(file= paste0(i,"LowAmmonia", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 1), ann = FALSE)
  mtext(side = 2, text = "Ammonia as N (mg/L)", line = 2) 
  abline(h = 0.1, lty = 3)
  abline(v = c(1995, 2005, 2015), col = "darkgray")
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1))
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ]
    #   print(j)
    pseries = as.numeric(sub$Ammonia.as.N.mg.L)
    pseries <- na.omit(pseries)
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2)
    color = color + 1
  }
  
  dev.off()
}

#################################      NITRATE     #############################################
for (i in groups) {
  color = 1
  sites.x = groups[groups = i]
  #png(file= paste0(i,"Nitrate", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 10), ann = FALSE)
  mtext(side = 2, text = "Nitrate as N (mg/L)", line = 2) 
  abline(h = 1, lty = 3, col = "black")
  abline(v = c(1995, 2005, 2015), col = "darkgray")
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1))
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ]
    #   print(j)
    pseries = as.numeric(sub$Nitrate.as.N..mg.L.)
    pseries <- na.omit(pseries)
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2)
    color = color + 1
  }
  #dev.off()
}


################################# NITRATE (SMALLER AXIS) #############################################
for (i in groups) {
  color = 1
  sites.x = groups[groups = i]
  png(file= paste0(i,"LowNitrate", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 3.5), ann = FALSE)
  mtext(side = 2, text = "Nitrate as N (mg/L)", line = 2) 
  abline(h = 1, lty = 3, col = "black")
  abline(v = c(1995, 2005, 2015), col = "darkgray")
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1))
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ]
    #   print(j)
    pseries = as.numeric(sub$Nitrate.as.N..mg.L.)
    pseries <- na.omit(pseries)
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2)
    color = color + 1
  }
  dev.off()
}


################################# PHOSPHATE #############################################
for (i in groups) {
  color = 1
  sites.x = groups[groups = i]
  png(file= paste0(i,"Phosphate", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 2.5), ann = FALSE)
  mtext(side = 2, text = "Orthophosphate as P (mg/L)", line = 2) 
  abline(h = 0.13, lty = 3)
  abline(v = c(1995, 2005, 2015), col = "darkgray")
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1))
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ]
    #   print(j)
    pseries = as.numeric(sub$MCCL.Phosphate..mg.L.) 
    pseries <- na.omit(pseries)
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2)
    color = color + 1
  }
  dev.off()
}



################################# DO (% SATURATION) #############################################
for (i in groups) {
  color = 1
  sites.x = groups[groups = i]
  png(file= paste0(i,"DO", ".png") , width=700, height=400, res = 150)
  par(mfrow = c(1,1))
  par(mai=c(0.5, 0.6, 0.3, 0.3))
  plot(0, type = "n", xlim = c(1989, 2018), ylim = c(-75, 100), ann = FALSE)
  mtext(side = 2, text = "DO % Saturation", line = 2) 
  abline(v = c(1995, 2005, 2015), col = "gray")
  legend("topright", ncol = 3, bty = "n", lty = 1, lwd = 2, leg = print(i), col = seq(1, 6, 1))
  for (j in i) {
    sub = dat[dat$CardNumberCode == j, ]
    #   print(j)
    pseries = as.numeric(sub$DO.SAT..) - 100 #For DO, values are scaled as how far from 100% saturation
    pseries <- na.omit(pseries)
    series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
                 end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
    tscomponents <-  decompose(series)
    points(tscomponents$trend, type = "l", col = color, lwd = 2)
    color = color + 1
  }
  dev.off()
}


####################### EXPANDED AXES FOR REGIONS WITH HIGH VALUES###########################
#Upper Slough Nitrate
color = 1
png(file="1HighNitrate.png",width=700,height=400, res = 150)
par(mfrow = c(1,1))
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 20), ann = FALSE)
mtext(side = 2, text = "Nitrate as N (mg/L)", line = 2) 
abline(h = 1, lty = 3, col = "black")
abline(v = c(1995, 2005, 2015), col = "darkgray")
legend("topright", bty = "n", ncol = 3, lty = 1, lwd = 2, 
       leg = c(sites.upper), col = seq(1, 6, 1))
for (j in sites.upper) {
  sub = dat[dat$CardNumberCode == j, ]
  pseries = as.numeric(sub$Nitrate.as.N..mg.L.)
  pseries <- na.omit(pseries)
  series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
               end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
  tscomponents <-  decompose(series)
  points(tscomponents$trend, type = "l", lwd = 2, col = color)
  color = color + 1
}
dev.off()


#Middle Slough Ammonia
color = 1
png(file="7HighAmmonia.png",width=700,height=400, res = 150)
par(mfrow = c(1,1))
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 5), 
     ann = FALSE)
mtext(side = 2, text = "Ammonia as N (mg/L)", line = 2) 
abline(h = 0.1, lty = 3)
abline(v = c(1995, 2005, 2015), col = "darkgray")
legend("topright", bty = "n", lty = 1, ncol = 3, lwd = 2, leg = c(sites.middle), col = seq(1, 6, 1))
for (j in sites.middle) {
  sub = dat[dat$CardNumberCode == j, ]
  pseries = na.omit(as.numeric(sub$Ammonia.as.N.mg.L))
  series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
               end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
  tscomponents <-  decompose(series)
  points(tscomponents$trend, type = "l", lwd = 2, col = color)
  color = color + 1
}
dev.off()

#South Estuary Nitrate
color = 1
png(file="20HighNitrate.png",width=700,height=400, res = 150)
par(mfrow = c(1,1))
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 55), ann = FALSE)
mtext(side = 2, text = "Nitrate as N (mg/L)", line = 2) 
abline(h = 1, lty = 3, col = "black")
abline(v = c(1995, 2005, 2015), col = "darkgray")
legend("topleft", bty = "n", ncol = 3, lty = 1, lwd = 2, leg = c(sites.southEstuary), col = seq(1, 6, 1))
for (j in sites.southEstuary) {
  sub = dat[dat$CardNumberCode == j, ]
  pseries = as.numeric(sub$Nitrate.as.N..mg.L.)
  pseries <- na.omit(pseries)
  series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
               end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
  tscomponents <-  decompose(series)
  points(tscomponents$trend, type = "l", lwd = 2, col = color)
  color = color + 1
}
dev.off()






