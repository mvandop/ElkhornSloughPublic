#Kathleen Hicks
#For ESNERR
#Creates a 5x5 grid of plots of raw data
#Uses report card numbering to identify sites
#Last updated 11/19/18

rm(list=ls()); graphics.off()

setwd("~/Box/ElkhornSlough") #Change to your own working directory!
dat = read.csv("MasterQueryOct2018.csv", stringsAsFactors = FALSE) #Update as needed with new csv

#Change the date format
#This tells R that the csv has date as dd-mmm-yy
#will show the date as yyyy-mm-dd
dat$Date <- as.Date(dat$Date,"%d-%b-%y")

#Make a new column that has just the year from the date
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")
head(dat)

#Make a list of sites
sites = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15.2, 16, 17, 18, 19, 20, 21, 22, 23, 24)


#Make a list of the parameters that will be graphed
params = c("Ammonia.as.N.mg.L", "Nitrate.as.N..mg.L.", "MCCL.Phosphate..mg.L.", "DO.mg.L", "DO.SAT..")


#For each of i parameters, make graphs for all of the sites 
for(i in params) {
  #Next line saves a png in your working directory folder
  #Commented out to view graphs in R first
  png(file= paste0("rawDat", i, ".png") , width=1400, height=800, res = 150)
  par(mfrow=c(5, 5)); #Make a 5x5 grid of graphs
  par(mai=c(0.3, 0.3, 0.1, 0)) #Set margins
  for (j in sites) {
    sub = dat[dat$CardNumberCode == j, ] #Take just the data from that site
    y = sub[ , i] #make the y-values of the graph equal to just values for parameter i
    plot(x=sub$Date, y=y, type = "l", xlims = c(1989, 2018), #For more years change xlims
         xlab = NULL, ylab = NULL) 
    legend("topleft", legend = c( " "), title= j, bty = "n")
  }
  dev.off() 
}



