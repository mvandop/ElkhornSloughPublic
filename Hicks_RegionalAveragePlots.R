#Kathleen Hicks
#For ESNERR
#Using updated query of nutrient data

rm(list=ls()); graphics.off()

setwd("~/Box/ElkhornSlough")
dat = read.csv("MasterQueryOct2018.csv", stringsAsFactors = FALSE)

#Change the date format
dat$Date <- as.Date(dat$Date,"%d-%b-%y")
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")
head(dat)



pacman::p_load(TSA)


#Group the sites according to location or other similarities
#Using a different set of numbers (column NumberCode)

groups = list(sites.upper, sites.Azevedo, sites.middle, sites.lower,
              sites.moroCojo, sites.southEstuary, sites.Bennett)


#For a certain region
#average the values of a certain day

upper = dat[dat$NumberCode == 13|dat$NumberCode == 14|dat$NumberCode == 15, ]
azevedo = dat[dat$NumberCode == 10|dat$NumberCode == 11|dat$NumberCode == 12, ]
middle = dat[dat$NumberCode == 6|dat$NumberCode == 7|dat$NumberCode == 8
             |dat$NumberCode == 9|dat$NumberCode == 30, ]
lower=dat[dat$NumberCode == 1|dat$NumberCode == 31, ]
morocojo=dat[dat$NumberCode == 2|dat$NumberCode == 3|dat$NumberCode == 20, ]
southEstuary = dat[dat$NumberCode == 4|dat$NumberCode == 5|
                      dat$NumberCode == 21|dat$NumberCode == 23|dat$NumberCode == 24, ]
bennett=dat[dat$NumberCode == 16|dat$NumberCode == 17|dat$NumberCode == 18|dat$NumberCode == 19, ]


########################################  NITRATE  ################################################
upper.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, upper, mean )
upper.averages.series = na.omit(upper.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.upper <- ts(upper.averages.series, start = 1988, 
                    end = 2018, frequency = 12)
tscomponents.upper <- decompose(series.upper)

azevedo.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, azevedo, mean )
azevedo.averages.series = na.omit(azevedo.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.azevedo <- ts(azevedo.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.azevedo <- decompose(series.azevedo)


middle.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, middle, mean )
middle.averages.series = na.omit(middle.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.middle <- ts(middle.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.middle <- decompose(series.middle)

lower.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, lower, mean )
lower.averages.series = na.omit(lower.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.lower <- ts(lower.averages.series, start = 1988, 
                    end = 2018, frequency = 12)
tscomponents.lower <- decompose(series.lower)

morocoj.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, morocojo, mean )
morocoj.averages.series = na.omit(morocoj.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.morocoj <- ts(morocoj.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.morocojo <- decompose(series.morocoj)



southEstuary.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, southEstuary, mean )
southEstuary.averages.series = na.omit(southEstuary.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.southEstuary <- ts(southEstuary.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.southEstuary <- decompose(series.southEstuary)

bennett.averages <- aggregate( as.numeric(Nitrate.as.N..mg.L.) ~ Date, bennett, mean )
bennett.averages.series = na.omit(bennett.averages$`as.numeric(Nitrate.as.N..mg.L.)`)
series.bennett <- ts(bennett.averages.series, start = 1988, 
                          end = 2018, frequency = 12)
tscomponents.bennett <- decompose(series.bennett)

png(file= paste0("OverallRegionalNitrateTrends", ".png"), width=1400, height=800, res = 150)
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 45), ann = FALSE)
mtext(side = 2, text = "Nitrate as N (mg/L)", line = 2) 
legend("topright", bty = "n", lty = 1, lwd = 2, ncol = 2,
       leg = c("Upper", "Azevedo", "Middle", "Lower", "Moro Cojo", "South Estuary", "Bennett"), col = seq(1, 7, 1))
abline(h = 1, lty = 3, col = "black")
abline(v = c(1995, 2005, 2015), col = "darkgray")
points(tscomponents.upper$trend, type = "l", lwd = 2)
points(tscomponents.azevedo$trend, type = "l", lwd = 2, col = 2)
points(tscomponents.middle$trend, type = "l", lwd = 2, col = 3)
points(tscomponents.lower$trend, type = "l", lwd = 2, col = 4)
points(tscomponents.morocojo$trend, type = "l", lwd = 2, col = 5)
points(tscomponents.southEstuary$trend, type = "l", lwd = 2, col = 6)
points(tscomponents.bennett$trend, type = "l", lwd = 2, col = 7)
dev.off()

######################################## AMMONIA ################################################
upper.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, upper, mean )
upper.averages.series = na.omit(upper.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.upper <- ts(upper.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.upper <- decompose(series.upper)

azevedo.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, azevedo, mean )
azevedo.averages.series = na.omit(azevedo.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.azevedo <- ts(azevedo.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.azevedo <- decompose(series.azevedo)


middle.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, middle, mean )
middle.averages.series = na.omit(middle.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.middle <- ts(middle.averages.series, start = 1988, 
                    end = 2018, frequency = 12)
tscomponents.middle <- decompose(series.middle)

lower.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, lower, mean )
lower.averages.series = na.omit(lower.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.lower <- ts(lower.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.lower <- decompose(series.lower)

morocoj.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, morocojo, mean )
morocoj.averages.series = na.omit(morocoj.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.morocoj <- ts(morocoj.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.morocojo <- decompose(series.morocoj)



southEstuary.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, southEstuary, mean )
southEstuary.averages.series = na.omit(southEstuary.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.southEstuary <- ts(southEstuary.averages.series, start = 1988, 
                          end = 2018, frequency = 12)
tscomponents.southEstuary <- decompose(series.southEstuary)

bennett.averages <- aggregate( as.numeric(Ammonia.as.N.mg.L) ~ Date, bennett, mean )
bennett.averages.series = na.omit(bennett.averages$`as.numeric(Ammonia.as.N.mg.L)`)
series.bennett <- ts(bennett.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.bennett <- decompose(series.bennett)

png(file= paste0("OverallRegionalAmmoniaTrends", ".png"), width=1400, height=800, res = 150)
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 1.5), ann = FALSE)
mtext(side = 2, text = "Ammonia as N (mg/L)", line = 2) 
legend("topright", bty = "n", lty = 1, lwd = 2, ncol = 2,
       leg = c("Upper", "Azevedo", "Middle", "Lower", "Moro Cojo", "South Estuary", "Bennett"), col = seq(1, 7, 1))
abline(h = 0.1, lty = 3, col = "black")
abline(v = c(1995, 2005, 2015), col = "darkgray")
points(tscomponents.upper$trend, type = "l", lwd = 2)
points(tscomponents.azevedo$trend, type = "l", lwd = 2, col = 2)
points(tscomponents.middle$trend, type = "l", lwd = 2, col = 3)
points(tscomponents.lower$trend, type = "l", lwd = 2, col = 4)
points(tscomponents.morocojo$trend, type = "l", lwd = 2, col = 5)
points(tscomponents.southEstuary$trend, type = "l", lwd = 2, col = 6)
points(tscomponents.bennett$trend, type = "l", lwd = 2, col = 7)
dev.off()


######################################## PHOSPHATE ################################################
upper.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, upper, mean )
upper.averages.series = na.omit(upper.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.upper <- ts(upper.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.upper <- decompose(series.upper)

azevedo.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, azevedo, mean )
azevedo.averages.series = na.omit(azevedo.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.azevedo <- ts(azevedo.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.azevedo <- decompose(series.azevedo)

middle.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, middle, mean )
middle.averages.series = na.omit(middle.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.middle <- ts(middle.averages.series, start = 1988, 
                    end = 2018, frequency = 12)
tscomponents.middle <- decompose(series.middle)

lower.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, lower, mean )
lower.averages.series = na.omit(lower.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.lower <- ts(lower.averages.series, start = 1988, 
                   end = 2018, frequency = 12)
tscomponents.lower <- decompose(series.lower)

morocoj.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, morocojo, mean )
morocoj.averages.series = na.omit(morocoj.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.morocoj <- ts(morocoj.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.morocojo <- decompose(series.morocoj)

southEstuary.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, southEstuary, mean )
southEstuary.averages.series = na.omit(southEstuary.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.southEstuary <- ts(southEstuary.averages.series, start = 1988, 
                          end = 2018, frequency = 12)
tscomponents.southEstuary <- decompose(series.southEstuary)

bennett.averages <- aggregate( as.numeric(MCCL.Phosphate..mg.L.) ~ Date, bennett, mean )
bennett.averages.series = na.omit(bennett.averages$`as.numeric(MCCL.Phosphate..mg.L.)`)
series.bennett <- ts(bennett.averages.series, start = 1988, 
                     end = 2018, frequency = 12)
tscomponents.bennett <- decompose(series.bennett)


png(file= paste0("OverallRegionalPhosphateTrends", ".png"), width=1400, height=800, res = 150)
par(mai=c(0.5, 0.6, 0.3, 0.3))
plot(0, type = "n", xlim = c(1989, 2018), ylim = c(0, 2), ann = FALSE)
mtext(side = 2, text = "Orthophosphate as P (mg/L)", line = 2) 
legend("topright", bty = "n", lty = 1, lwd = 2, ncol = 2,
       leg = c("Upper", "Azevedo", "Middle", "Lower", "Moro Cojo", "South Estuary", "Bennett"), col = seq(1, 7, 1))
abline(h = 0.13, lty = 3, col = "black")
abline(v = c(1995, 2005, 2015), col = "darkgray")
points(tscomponents.upper$trend, type = "l", lwd = 2)
points(tscomponents.azevedo$trend, type = "l", lwd = 2, col = 2)
points(tscomponents.middle$trend, type = "l", lwd = 2, col = 3)
points(tscomponents.lower$trend, type = "l", lwd = 2, col = 4)
points(tscomponents.morocojo$trend, type = "l", lwd = 2, col = 5)
points(tscomponents.southEstuary$trend, type = "l", lwd = 2, col = 6)
points(tscomponents.bennett$trend, type = "l", lwd = 2, col = 7)
dev.off()
