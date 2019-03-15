#Kathleen Hicks
#For ESNERR
#Seasonal Kendall Test
#Last updated 11/19/18

rm(list=ls()); graphics.off()

setwd("~/Box/ElkhornSlough") #Change to your own wd
dat = read.csv("MasterQueryOct2018.csv", stringsAsFactors = FALSE) #Update reference as necessary

head(dat)

dat$Date <- as.Date(dat$Date,"%d-%b-%y")
dat$Year <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y")
dat$Month <- format(as.Date(dat$Date, format = "%d-%b-%y"), "%m")


head(dat)

sites = unique(dat$ESNERR_StCode)
sites <- sites[sites != "TS2" & sites != "MCS2" & sites != "ROK"
               & sites != "CAT" & sites != "SRL"]
sites

params = c("Ammonia.as.N.mg.L", "Nitrate.as.N..mg.L.", "MCCL.Phosphate..mg.L.")

pacman::p_load(EnvStats)


# TREND ANALYSIS for all sites for each parameter
#Prints the parameter
#Then prints site name followed by estimates of tau, slope, intercept 
#value of chi-square and z(trend)
#p-value for chi-squared and trend

for (i in params) {
  print(i)
  for(j in sites) {
    sub = dat[dat$ESNERR_StCode == j, ]
    year = sub$Year
    year = as.numeric(year)
    season = sub$Month
    season= as.numeric(season)
    subparam= sub[ , i]
    subparam = as.numeric(subparam)
    sk_test<- kendallSeasonalTrendTest(subparam, season = season, year = year)
    print(j)   
    print(sk_test$estimate)
    print(sk_test$statistic)
    print(sk_test$p.value)

  }
}

