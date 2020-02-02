### Montejo-Kovacevich, G., Martin, S.H., Meier, J.I., Bacquet, C.N., Monllor, M., Jiggins, C.D. and Nadeau, N.J., 2020. ###
### Microclimate buffering and thermal tolerance across elevations in a tropical butterfly. ###
### Journal of Experimental Biology. ######

##### OBTAIN CLIMATE DATA ####
## from world clim ###
#### packages #####
rm(list=ls())
dev.off()
library(raster)
library(sp)
library(ggplot2)
library(dplyr)
library(reshape2)
library(vegan)
library(grid)
library(cowplot)
library(gridExtra)
library(raster)

#### data #####
setwd("microclimate.thermal.tolerance.Heliconius-JEB-2020/")
logger.info <- read.csv("1.data/logger.info.csv")
wild.temp.all <- read.csv("1.data/wild.temp.data.csv")

## summary of localities ###
logger.info$alt.slope <- paste(logger.info$alt_type, logger.info$side_andes, sep="_")
summ.localities <- summarise(group_by(logger.info, alt.slope, side_andes, alt_type),
                             lat= mean(latitude),
                             lon= mean(longitude),
                             alt=mean(point_altitude)); summ.localities
wild.temp.all$Month <- paste("m", substr(wild.temp.all$Date, 6, 7), sep="")

######################################## WORLDCLIM2 ####################


# my localities 
lats <- c(summ.localities$lat)
lons <- c(summ.localities$lon)
coords <- data.frame(x=lons,y=lats)

#### download data ####
# 0.5 minutes of a degree
# mean annual temp, diurnal range, precipitation 
bio.east <- getData("worldclim",var="bio",res=0.5, lon=lons[c(1,3)], lat=lats[c(1,3)] )
bio.west <- getData("worldclim",var="bio",res=0.5, lon=lons[c(2,4)], lat=lats[c(2,4)] )

#### tmin monthly
tmin.east <- getData("worldclim",var="tmin",res=0.5, lon=lons[c(1,3)], lat=lats[c(1,3)] )
tmin.west <- getData("worldclim",var="tmin",res=0.5, lon=lons[c(2,4)], lat=lats[c(2,4)] )

#### tmax monthly
tmax.east <- getData("worldclim",var="tmax",res=0.5, lon=lons[c(1,3)], lat=lats[c(1,3)] )
tmax.west <- getData("worldclim",var="tmax",res=0.5, lon=lons[c(2,4)], lat=lats[c(2,4)] )

#### tmean monthly
tmean.east <- getData("worldclim",var="tmean",res=0.5, lon=lons[c(1,3)], lat=lats[c(1,3)] )
tmean.west <- getData("worldclim",var="tmean",res=0.5, lon=lons[c(2,4)], lat=lats[c(2,4)] )

#### tmean monthly
alt.east <- getData("worldclim",var="alt",res=0.5, lon=lons[c(1,3)], lat=lats[c(1,3)] )
alt.west <- getData("worldclim",var="alt",res=0.5, lon=lons[c(2,4)], lat=lats[c(2,4)] )

# extract worldclim interesting variables
names(bio.west)
bio.west1 <- bio.west[[c(1,2,3,4,7,12)]]
bio.east1 <- bio.east[[c(1,2,3,4,7,12)]]
names(bio.west1) <- c("annual.mean.temp","bio2.diurnal.range","bio3.isothermality","bio4.seasonality","bio7.annual.temp.range", "annual.precipitation")
names(bio.east1) <- c("annual.mean.temp","bio2.diurnal.range","bio3.isothermality","bio4.seasonality","bio7.annual.temp.range", "annual.precipitation")

#### extract data from each dataset, add to summ df ##########
# 1 east
coords <- data.frame(x=lons[c(1,3)], y=lats[c(1,3)])
points <- SpatialPoints(coords, proj4string = bio.east1@crs)
values <- raster::extract(bio.east1, points)
df <- cbind.data.frame(coordinates(points),values)

summ.localities$wc.annual.mean.temp <- df$annual.mean.temp[match(summ.localities$lon, df$x)]
summ.localities$wc.bio2.annual.diurnal.range <- df$bio2.diurnal.range[match(summ.localities$lon, df$x)]
summ.localities$wc.bio3.isothermality <- df$bio3.isothermality[match(summ.localities$lon, df$x)]
summ.localities$wc.bio4.seasonality <- df$bio4.seasonality[match(summ.localities$lon, df$x)]
summ.localities$wc.bio7.annual.temp.range <- df$bio7.annual.temp.range[match(summ.localities$lon, df$x)]
summ.localities$wc.annual.precipitation <- df$annual.precipitation[match(summ.localities$lon, df$x)]

# 2 west annual mean
coords <- data.frame(x=lons[c(2,4)], y=lats[c(2,4)])
points <- SpatialPoints(coords, proj4string = bio.west1@crs)
values <- raster::extract(bio.west1,points)
df <- cbind.data.frame(coordinates(points),values)

summ.localities$wc.annual.mean.temp[c(2,4)] <- df$annual.mean.temp[c(1,2)]
summ.localities$wc.bio2.annual.diurnal.range[c(2,4)] <- df$bio2.diurnal.range[c(1,2)]
summ.localities$wc.bio3.isothermality[c(2,4)] <- df$bio3.isothermality[c(1,2)]
summ.localities$wc.bio4.seasonality[c(2,4)] <- df$bio4.seasonality[c(1,2)]
summ.localities$wc.bio7.annual.temp.range[c(2,4)] <- df$bio7.annual.temp.range[c(1,2)]
summ.localities$wc.annual.precipitation[c(2,4)] <- df$annual.precipitation[c(1,2)]

# 3 min temp monthly east
coords <- data.frame(x=lons[c(1,3)], y=lats[c(1,3)])
points <- SpatialPoints(coords, proj4string = tmin.east@crs)
values <- raster::extract(tmin.east,points)
df <- cbind.data.frame(coordinates(points),values)
tmin.df.e <- gather(df, key="month", value="value", 3:14)
tmin.df.e$data.type <- c("tmin")
tmin.df.e$month <-if_else(nchar(tmin.df.e$month)==9, paste("m",substr(tmin.df.e$month, 5,6), sep=""), 
                        paste("m0", substr(tmin.df.e$month, 5,5), sep = ""));

# 4 min temp monthly west
coords <- data.frame(x=lons[c(2,4)], y=lats[c(2,4)])
points <- SpatialPoints(coords, proj4string = tmin.west@crs)
values <- raster::extract(tmin.west,points)

df <- cbind.data.frame(coordinates(points),values)
tmin.df.w <- gather(df, key="month", value="value", 3:14)
tmin.df.w$data.type <- c("tmin")
tmin.df.w$month <-if_else(nchar(tmin.df.w$month)==9, paste("m",substr(tmin.df.w$month, 5,6), sep=""), 
                          paste("m0", substr(tmin.df.w$month, 5,5), sep = ""));

# 5 max temp monthly east
coords <- data.frame(x=lons[c(1,3)], y=lats[c(1,3)])
points <- SpatialPoints(coords, proj4string = tmax.east@crs)
values <- raster::extract(tmax.east,points)
df <- cbind.data.frame(coordinates(points),values)
tmax.df.e <- gather(df, key="month", value="value", 3:14)
tmax.df.e$data.type <- c("tmax")
tmax.df.e$month <-if_else(nchar(tmax.df.e$month)==9, paste("m",substr(tmax.df.e$month, 5,6), sep=""), 
                          paste("m0", substr(tmax.df.e$month, 5,5), sep = ""));

# 6 max temp monthly west
coords <- data.frame(x=lons[c(2,4)], y=lats[c(2,4)])
points <- SpatialPoints(coords, proj4string = tmax.west@crs)
values <- raster::extract(tmax.west,points)
df <- cbind.data.frame(coordinates(points),values)
tmax.df.w <- gather(df, key="month", value="value", 3:14)
tmax.df.w$data.type <- c("tmax")
tmax.df.w$month <-if_else(nchar(tmax.df.w$month)==9, paste("m",substr(tmax.df.w$month, 5,6), sep=""), 
                           paste("m0", substr(tmax.df.w$month, 5,5), sep = ""));

# 7 mean temp monthly east
coords <- data.frame(x=lons[c(1,3)], y=lats[c(1,3)])
points <- SpatialPoints(coords, proj4string = tmean.east@crs)
values <- raster::extract(tmean.east,points)
df <- cbind.data.frame(coordinates(points),values)
tmean.df.e <- gather(df, key="month", value="value", 3:14)
tmean.df.e$data.type <- c("tmean")
tmean.df.e$month <-if_else(nchar(tmean.df.e$month)==10, paste("m",substr(tmean.df.e$month, 6,7), sep=""), 
                           paste("m0", substr(tmean.df.e$month, 6,6), sep = ""));

# 8 mean temp monthly west
coords <- data.frame(x=lons[c(2,4)], y=lats[c(2,4)])
points <- SpatialPoints(coords, proj4string = tmean.west@crs)
values <- raster::extract(tmean.west,points)
df <- cbind.data.frame(coordinates(points),values)
tmean.df.w <- gather(df, key="month", value="value", 3:14)
tmean.df.w$data.type <- c("tmean")
tmean.df.w $month <-if_else(nchar(tmean.df.w $month)==10, paste("m",substr(tmean.df.w $month, 6,7), sep=""), 
                           paste("m0", substr(tmean.df.w $month, 6,6), sep = ""));

## combine all monthly dfs
monthly.wc.data <- rbind(tmin.df.e, tmin.df.w,
                         tmax.df.e, tmax.df.w,
                         tmean.df.e, tmean.df.w); monthly.wc.data
names(summ.localities)
monthly.wc.data$alt.slope <- summ.localities$alt.slope[match(monthly.wc.data$x, summ.localities$lon)]
monthly.wc.data$alt <- summ.localities$alt[match(monthly.wc.data$x, summ.localities$lon)]

write.csv(monthly.wc.data, "1.data/monthly.wc.data.csv", row.names = FALSE)
write.csv(summ.localities, "1.data/summ.localities.csv", row.names = FALSE)



######################################## MICROCLIMATE DATASETS FOR FIGURES ####################
#### 1. Fig. 1 A1/B1 yearly - mean across dataloggers at a given time, date, area ####
logger_all_temp_mean_date.time <- dplyr::summarise(group_by(wild.temp.all,Time, Date, alt_height_slope, alt_type, side_andes, height),
                                                   value=mean(value),
                                                   sd=sd(value))

logger_all_temp_mean_date.time$alt.type_height <- paste(logger_all_temp_mean_date.time$alt_type, logger_all_temp_mean_date.time$height, sep="_")
logger_all_temp_mean_date.time$sd <- as.numeric(as.character(logger_all_temp_mean_date.time$sd))
logger_all_temp_mean_date.time$alt.type_height <- factor(logger_all_temp_mean_date.time$alt.type_height, levels= c("low_c", "low_u","high_c", "high_u"))
logger_all_temp_mean_date.time<-logger_all_temp_mean_date.time[order(factor(logger_all_temp_mean_date.time$alt.type_height, levels =  c("low_c", "low_u","high_c", "high_u"))),]
logger_all_temp_mean_date.time$date.time <- paste(logger_all_temp_mean_date.time$Date, logger_all_temp_mean_date.time$Time) 
logger_all_temp_mean_date.time$date.time <- as.POSIXct(logger_all_temp_mean_date.time$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

write.csv(logger_all_temp_mean_date.time, "1.data/fig1.1.logger.hourly.means.csv", row.names = FALSE)

#### 2. Fig. 1.2 - first datalogger max,min,mean, then DAILY mean ####
# first logger daily max,min,mean - then average per day

daily_temp_mean <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id, Month),
                                    value=mean(value),
                                    sd=mean(sd(value)),
                                    type=paste("temp.mean"))
daily_temp_min <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id, Month),
                                   value=min(value),
                                   sd=min(sd(value)),
                                   type=paste("temp.min"))
daily_temp_max <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id, Month),
                                   value=max(value),
                                   sd=max(sd(value)),
                                   type=paste("temp.max"))

# mean of max/min/mean across dataloggers per day
mean.daily_temp_max <- dplyr::summarise(group_by(daily_temp_max, Date, alt_height_slope, alt_type, side_andes, height),
                                   value=mean(value),
                                   sd=max(sd(value)),
                                   type=paste("temp.max"))
mean.daily_temp_min <- dplyr::summarise(group_by(daily_temp_min, Date, alt_height_slope, alt_type, side_andes, height),
                                        value=mean(value),
                                        sd=max(sd(value)),
                                        type=paste("temp.min"))
mean.daily_temp_mean <- dplyr::summarise(group_by(daily_temp_mean, Date, alt_height_slope, alt_type, side_andes, height),
                                        value=mean(value),
                                        sd=max(sd(value)),
                                        type=paste("temp.max"))

write.csv(mean.daily_temp_mean, "1.data/fig1.2.daily.mean.alllogger.daily.csv", row.names = FALSE)
write.csv(mean.daily_temp_max, "1.data/fig1.2.daily.max.alllogger.daily.csv", row.names = FALSE)
write.csv(mean.daily_temp_min, "1.data/fig1.2.daily.min.alllogger.daily.csv", row.names = FALSE)


#### 3. Fig. 2.A - mean annual temperature across loggers at a given time of the day 

# first means across dataloggers at X hour (64loggers*24)
summ.temp.hours.side.alt.height <- dplyr::summarise(group_by(wild.temp.all,Time, side_andes,alt_type, datalogger_id, height,alt_height_slope, alt_height_type),
                                                            value.mean = mean(value),
                                                            sd= sd(value))
# then average these across the year
summ.temp.hours.side.alt.height <- dplyr::summarise(group_by(summ.temp.hours.side.alt.height,Time, side_andes,alt_type,  height,alt_height_slope, alt_height_type),
                                                    value = mean(value.mean),
                                                    sd= sd(value.mean))

summ.temp.hours.side.alt.height$alt.type_height <- paste(summ.temp.hours.side.alt.height$alt_type, summ.temp.hours.side.alt.height$height, sep="_")
summ.temp.hours.side.alt.height$sd <- as.numeric(as.character(summ.temp.hours.side.alt.height$sd))

write.csv(summ.temp.hours.side.alt.height, "1.data/fig2.A.summ.temp.hours.side.alt.height.csv", row.names = FALSE)


#### 3. Table S1- BIO 1 - annual mean temperature ####
# use same for fig 1.2 ()
head(daily_temp_mean )

yearly.mean.datalogger <- dplyr::summarise(group_by(daily_temp_mean, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                      value=mean(value),
                                      type=paste("temp.mean"))

yearly.mean <- dplyr::summarise(group_by(yearly.mean.datalogger, alt_height_slope, alt_type, side_andes, height),
                                           mean.temp=mean(value),
                                           se=sd(value)/sqrt(n()),
                                           type=paste("temp.mean"))

yearly.mean$mean.se <- paste(round(yearly.mean$mean.temp,2), round(yearly.mean$se,2), sep=" ± ")
yearly.mean.spread <- spread(yearly.mean[c(2,3,4, 5)], key = height, value = mean.temp)

yearly.mean.spread <- spread(yearly.mean[c(2,3,4, 8)], key = height, value = mean.se)
write.csv(yearly.mean.spread, "1.data/tableS1.yearly.mean.spread.csv")



#### 4. Fig. 2B - BIO 2 - annual mean diurnal range ####
# first daily max,mean,min per logger (as fig 1.2)
# then temp range per logger in a day (max-min)
daily_temp_max.min <- rbind(daily_temp_max, daily_temp_min) 

# spread and calculate logger daily range
daily_temp_max.min.spread <- spread(daily_temp_max.min[c(1,3:6,7,9,10)], key=type, value=value)
daily_temp_max.min.spread$temp.range <- daily_temp_max.min.spread$temp.max - daily_temp_max.min.spread$temp.min
test<- subset(daily_temp_max.min.spread,is.na(daily_temp_max.min.spread$temp.range))

# average yearly diurnal range per logger
yearly.daily_temp_max.min.spread <- dplyr::summarise(group_by(daily_temp_max.min.spread, alt_type, side_andes, height, datalogger_id),
          temp.range.mean= mean(temp.range),
          temp.range.mean.se=sd(temp.range)/sqrt(n())); yearly.daily_temp_max.min.spread

yearly.daily_temp_max.min.spread.area <-  dplyr::summarise(group_by(yearly.daily_temp_max.min.spread, alt_type, side_andes, height),
                                              temp.range.area.mean= mean(temp.range.mean),
                                              temp.range.area.mean.sd=sd(temp.range.mean))
yearly.daily_temp_max.min.spread.area$alt.type_height <- paste(yearly.daily_temp_max.min.spread.area$alt_type, yearly.daily_temp_max.min.spread.area$height, sep=".")

write.csv(yearly.daily_temp_max.min.spread.area, "1.data/fig2.B.yearly.daily_temp_max.min.spread.area.csv", row.names = FALSE)


#### 5. Table 1 - site temperature offset understory daily max, mean, min #### 
# grab data calculated for fig1.2
daily_temp_mean$point <- logger.info$point[match(daily_temp_mean$datalogger_id, logger.info$code_mine)]
daily_temp_max$point <- logger.info$point[match(daily_temp_max$datalogger_id, logger.info$code_mine)]
daily_temp_min$point <- logger.info$point[match(daily_temp_min$datalogger_id, logger.info$code_mine)]
names(daily_temp_mean)

# make wide format and remove rows
daily.mean.spread <- spread(daily_temp_mean[c(1,3:5,7,9,10)], key=height, value=value); daily.mean.spread <- subset(daily.mean.spread, u!=""&c!="")
daily.max.spread <- spread(daily_temp_max[c(1,3:5,7,9,10)], key=height, value=value); daily.max.spread <- subset(daily.max.spread, u!=""&c!="")
daily.min.spread <- spread(daily_temp_min[c(1,3:5,7,9,10)], key=height, value=value); daily.min.spread <- subset(daily.min.spread, u!=""&c!="")

# calculate daily offset of understory
daily.mean.spread$offset.understory <- daily.mean.spread$u -daily.mean.spread$c
daily.mean.spread$offset.type <- c("mean.offset")
daily.max.spread$offset.understory <- daily.max.spread$u -daily.max.spread$c
daily.max.spread$offset.type <- c("max.offset")
daily.min.spread$offset.understory <- daily.min.spread$u -daily.min.spread$c
daily.min.spread$offset.type <- c("min.offset")

daily.mean.max.min.offset.spread <- rbind(daily.mean.spread, daily.max.spread, daily.min.spread)
write.csv(daily.mean.max.min.offset.spread , "1.data/fig2.B.daily.mean.max.min.offset.spread.csv", row.names = FALSE)


# mean offsets per area, suitable for table
names(daily.mean.spread)
daily.mean.spread.area <- summarise(group_by(daily.mean.spread, alt_type, side_andes, type),
          offset.understory.value=mean(offset.understory),
          n=n(),
          offset.understory.se=sd(offset.understory)/sqrt(n),
          offset.type=c("mean.offset"))

daily.max.spread.area <-summarise(group_by(daily.max.spread, alt_type, side_andes, type),
          offset.understory.value=mean(offset.understory),
          n=n(),
          offset.understory.se=sd(offset.understory)/sqrt(n),
          offset.type=c("max.offset"))

daily.min.spread.area <-summarise(group_by(daily.min.spread, alt_type, side_andes, type),
          offset.understory.value=mean(offset.understory),
          n=n(),
          offset.understory.se=sd(offset.understory)/sqrt(n),
          offset.type=c("min.offset"))

daily.mean.max.min.offset.spread.area <- rbind(daily.mean.spread.area, daily.max.spread.area, daily.min.spread.area)
daily.mean.max.min.offset.spread.area$value.table <- paste(round(daily.mean.max.min.offset.spread.area$offset.understory.value,2), round(daily.mean.max.min.offset.spread.area$offset.understory.se,3), sep="±")
daily.mean.max.min.offset.spread.area
daily.mean.max.min.offset.spread.area.table <- spread(daily.mean.max.min.offset.spread.area[c(1,2,7,8)], key=offset.type, value=value.table)
write.csv(daily.mean.max.min.offset.spread.area.table , "1.data/fig2.B.daily.mean.max.min.offset.spread.area.table.csv", row.names = FALSE)


# mean offsets per point, suitable for table histogram
names(daily.mean.spread)
daily.mean.spread.point <- summarise(group_by(daily.mean.spread, alt_type, side_andes, type, point),
                                    offset.understory.value=mean(offset.understory),
                                    n=n(),
                                    offset.understory.se=sd(offset.understory)/sqrt(n),
                                    offset.type=c("mean.offset"))

daily.max.spread.point <-summarise(group_by(daily.max.spread, alt_type, side_andes, type, point),
                                  offset.understory.value=mean(offset.understory),
                                  n=n(),
                                  offset.understory.se=sd(offset.understory)/sqrt(n),
                                  offset.type=c("max.offset"))

daily.min.spread.point <-summarise(group_by(daily.min.spread, alt_type, side_andes, type, point),
                                  offset.understory.value=mean(offset.understory),
                                  n=n(),
                                  offset.understory.se=sd(offset.understory)/sqrt(n),
                                  offset.type=c("min.offset"))

daily.mean.max.min.offset.spread.point <- rbind(daily.mean.spread.point, daily.max.spread.point, daily.min.spread.point)
write.csv(daily.mean.spread.point , "1.data/fig2.B.daily.mean.offset.point.csv", row.names = FALSE)
write.csv(daily.max.spread.point , "1.data/fig2.B.daily.max.offset.point.csv", row.names = FALSE)
write.csv(daily.min.spread.point, "1.data/fig2.B.daily.min.offset.point.csv", row.names = FALSE)
write.csv(daily.mean.max.min.offset.spread.point , "1.data/fig2.B.daily.mean.max.min.offset.spread.point.csv", row.names = FALSE)


#### 6. Fig. 2 C #### 
# first daily max,mean,min per logger (as fig 1.2)
head(daily_temp_max)
daily.max.mean.min.logger <- rbind(daily_temp_max, daily_temp_mean, daily_temp_min)
daily.max.mean.min.logger$alt.slope <- paste(daily.max.mean.min.logger$alt_type, daily.max.mean.min.logger$side_andes, sep="_")
daily.max.mean.min.logger$alt.type_height <- paste(daily.max.mean.min.logger$alt_type, daily.max.mean.min.logger$height, sep="_")
daily.max.mean.min.logger

# then mean of daily max/min/mean across the year per logger (3value types *64)
daily.annual.means.per.logger <- dplyr::summarise(group_by(daily.max.mean.min.logger, type ,datalogger_id, alt_height_slope, alt_type, side_andes, height),
                                           mean.value=mean(value))


daily.annual.means.per.logger$alt.slope <- paste(daily.annual.means.per.logger$alt_type, daily.annual.means.per.logger$side_andes, sep="_")
daily.annual.means.per.logger$alt.type_height <- paste(daily.annual.means.per.logger$alt_type, daily.annual.means.per.logger$height, sep="_")
write.csv(daily.annual.means.per.logger, "1.data/fig2.C.daily.annual.means.per.logger.csv", row.names = FALSE)


