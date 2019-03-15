#Kathleen Hicks
#For ESNERR
#Using updated query of nutrient data

rm(list=ls()); graphics.off()

setwd("~/Box/ElkhornSlough")
dat = read.csv("MasterQueryOct2018.csv", 
               stringsAsFactors = FALSE)

#Change the date format
dat$Date <- as.Date(dat$Date,"%d-%b-%y")
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")
head(dat)

#Make a list of sites
#Exclude sites that have lots of gaps or that don't have 
#enough of a history
#sites = unique(dat$ESNERR_StCode)
#sites <- sites[sites != "TS2" & sites != "MCS2" & sites != "ROK"
               #& sites != "CAT" & sites != "SRL"]

#Make a list of the parameters that will be graphed
##params = c("Ammonia.as.N.mg.L", #"Free.Ammonia..mg.L.",
           #"Nitrate.as.N..mg.L.", #"Umol.Nitrate",
           #"MCCL.Phosphate..mg.L.",
           #"DO.mg.L", "DO.SAT..", "Sal..PSU.")


library(TSA)


png(file= paste0("BlohmPorterNitrateRaw", ".png"), width=1400, height=800, res = 150)
par(mai=c(1, 1, 0.3, 0.3))
sub = dat[dat$CardNumberCode == 2, ]
plot(x=sub$Date, y=sub$Nitrate.as.N..mg.L., type = "l", main = "Blohm Porter Raw Data", 
     xlims = c(1989, 2018), xlab = "Date", ylab = "Nitrate as N (mg/L)")
dev.off()



png(file= paste0("BlohmPorterNitrateSmoothed", ".png"), width=1400, height=800, res = 150)
par(mai=c(1, 1, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 20), main = "Blohm Porter Smoothed Data", 
     xlab = "Date", ylab = "Nitrate as N (mg/L)")
#abline(h = 0.1, lty = 3)
#abline(v = c(1995, 2005, 2015), col = "darkgray")
sub = dat[dat$CardNumberCode == 2, ]
pseries = as.numeric(sub$Nitrate.as.N..mg.L)
pseries <- na.omit(pseries)
series <- ts(pseries, start = c(as.numeric(min(sub$Year)), 1), 
             end = c(as.numeric(max(sub$Year)), 1), frequency = 12)
tscomponents <-  decompose(series)
points(tscomponents$trend, type = "l")
dev.off()


