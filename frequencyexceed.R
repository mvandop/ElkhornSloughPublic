#Molly Van Dop
#For ESNERR
pacman::p_load(lubridate, tidyverse, ggplot2)

rm(list=ls()); graphics.off()

setwd("~/Box/ElkhornSlough") #Change to your own working directory
dat = read.csv("MasterQueryOct2018.csv", stringsAsFactors = FALSE) #Change as necessary for most updated csv

#My ggplot theme
theme_m <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())
#Change the date format
#This tells R that the csv has date as dd-mmm-yy
#will show the date as yyyy-mm-dd
dat$Date <- as.Date(dat$Date,"%d-%b-%y")
#Make a new column that has just the year from the date
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")

dat <- dat %>% mutate(month = month(Date))
dat <- dat %>%  mutate(dryS = ifelse(month %in% c(5,6,7,8,9,10),1,0))
dat <- dat %>% select(NumberCode, Date, Ammonia.as.N.mg.L, Nitrate.as.N..mg.L., 
                      MCCL.Phosphate..mg.L., Year, month, dryS)

#Subset data for sites we are focused on
datsites <- dat %>% filter(NumberCode %in% c(4,5,8,13,18))

#Change from character to numeric 
datsites$Ammonia.as.N.mg.L <- as.numeric(datsites$Ammonia.as.N.mg.L)
datsites$Nitrate.as.N..mg.L. <- as.numeric(datsites$Nitrate.as.N..mg.L.)
datsites$MCCL.Phosphate..mg.L. <- as.numeric(datsites$MCCL.Phosphate..mg.L.)

#Want to see what the baseline levels of the different nutrients are in the sites
sumDatSites <- datsites %>% group_by(NumberCode) %>% summarise(meanNi = mean(Nitrate.as.N..mg.L., na.rm = T), 
                                                               meanAmm = mean(Ammonia.as.N.mg.L), 
                                                               meanP = mean(MCCL.Phosphate..mg.L., na.rm = T))

# Colored Histogram with Different Number of Bins

ggplot(datsites %>% filter(Ammonia.as.N.mg.L <=5) , aes(Ammonia.as.N.mg.L, color = NumberCode)) + 
  geom_density() + theme_m +
  ggtitle("Density of Ammonia as N (mg/L): Selected Sites") + 
  xlab("Ammonia as N (mg/L)") + 
  ylab("Density")

ggplot(datsites %>% filter(Nitrate.as.N..mg.L. <=8)  , aes(Nitrate.as.N..mg.L., color = NumberCode)) + 
  geom_density() + theme_m + 
  ggtitle("Density of Nitrate as N (mg/L): Selected Sites") + 
  xlab("Nitrate as N (mg/L)") + 
  ylab("Density")

ggplot(datsites, aes(MCCL.Phosphate..mg.L., color = NumberCode)) + 
  geom_density() + theme_m + 
  ggtitle("Density of Orthophosphate as P (mg/L): Selected Sites") + 
  xlab("Orthophosphate as P (mg/L)") + 
  ylab("Density")


exceedDatSitesN <- datsites %>% group_by(NumberCode) %>% filter(Nitrate.as.N..mg.L. >= 2*mean(Nitrate.as.N..mg.L., na.rm = T))
exceedDatSitesA <- datsites %>% group_by(NumberCode) %>% filter(Ammonia.as.N.mg.L >= 2*mean(Ammonia.as.N.mg.L, na.rm = T))
exceedDatSitesP <- datsites %>% group_by(NumberCode) %>% filter(MCCL.Phosphate..mg.L. >= 2*mean(MCCL.Phosphate..mg.L., na.rm = T))




ggplot(exceedDatSitesN, aes(month, fill = NumberCode)) + 
  geom_histogram(bins = 12, binwidth = 0.5, col = "black") + theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Count of Nitrate Spikes by Month, Site") + 
  scale_fill_discrete(name = "Site")

ggplot(exceedDatSitesA, aes(month, fill = NumberCode)) + 
  geom_histogram(bins = 12, binwidth = 0.5, col = "black") + theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Count of Ammonia Spikes by Month, Site") + 
  scale_fill_discrete(name = "Site")

ggplot(exceedDatSitesP, aes(month, fill = NumberCode)) + 
  geom_histogram(bins = 12, binwidth = 0.5, col = "black") + theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Count of Phosphate Spikes by Month, Site") + 
  scale_fill_discrete(name = "Site")

#Let's think about this a little differently. 
#Basically, we want to know the average nutrient level by site and by month

sumdatsites2 <- datsites %>% group_by(month, NumberCode) %>% summarise(meanNi = mean(Nitrate.as.N..mg.L., na.rm = T), 
                                                                      meanAmm = mean(Ammonia.as.N.mg.L), 
                                                                      meanP = mean(MCCL.Phosphate..mg.L., na.rm = T))


ggplot(sumdatsites2, aes(x = month, y = meanNi, fill = NumberCode)) + 
  geom_col(col = "black") + 
  theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Average Nitrate Level (mg/L) by Month, Site") + 
  scale_fill_discrete(name = "Site") + 
  ylab("Mean Nitrate Level (mg/L)")

ggplot(sumdatsites2, aes(x = month, y = meanAmm, fill = NumberCode)) + 
  geom_col(col = "black") + 
  theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Average Ammonia as N Level (mg/L) by Month, Site") + 
  scale_fill_discrete(name = "Site") + 
  ylab("Mean Ammonia as N Level (mg/L)")

ggplot(sumdatsites2, aes(x = month, y = meanP, fill = NumberCode)) + 
  geom_col(col = "black") + 
  theme_m +
  scale_x_continuous(breaks=seq(2,12, by = 2)) + 
  ggtitle("Average Phosphate Level (mg/L) by Month, Site") + 
  scale_fill_discrete(name = "Site") + 
  ylab("Mean Phosphate Level (mg/L)")
