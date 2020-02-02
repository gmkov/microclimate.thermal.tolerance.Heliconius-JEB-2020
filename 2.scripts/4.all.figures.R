### Montejo-Kovacevich, G., Martin, S.H., Meier, J.I., Bacquet, C.N., Monllor, M., Jiggins, C.D. and Nadeau, N.J., 2020. ###
### Microclimate buffering and thermal tolerance across elevations in a tropical butterfly. ###
### Journal of Experimental Biology. ######
##################### ALL FIGURES PREP ######################
##### packages ######

rm(list=ls())
dev.off()
library(dplyr)
library(tidyr)
library(reshape2)
library(vegan)
library(grid)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(lemon)
library(multcompView)
library(pwr)
library(egg)

##### data ######
setwd("microclimate.thermal.tolerance.Heliconius-JEB-2020/")
logger.info <- read.csv("1.data/logger.info.csv")
wild.temp.all <- read.csv("1.data/wild.temp.data.csv")
wild.humi.all <- read.csv("1.data/wild.humi.data.csv")
logger_all_temp_mean_date.time <- read.csv("1.data/fig1.1.logger.hourly.means.csv")
month.wc <-read.csv("1.data/fig1.1.monthly.raw.temps.wc.csv")
daily_temp_mean<-read.csv("1.data/fig1.2.daily.mean.alllogger.daily.csv")
daily_temp_max<-read.csv("1.data/fig1.2.daily.max.alllogger.daily.csv")
daily_temp_min<-read.csv("1.data/fig1.2.daily.min.alllogger.daily.csv")

summ.temp.hours.side.alt.height <- read.csv("1.data/fig2.A.summ.temp.hours.side.alt.height.csv")
yearly.daily_temp_max.min.spread.area <- read.csv("1.data/fig2.B.yearly.daily_temp_max.min.spread.area.csv")
daily.annual.means.per.logger <- read.csv( "1.data/fig2.C.daily.annual.means.per.logger.wc.comb.csv")
summ.localities <- read.csv("1.data/summ.localities.csv")

coll <- read.csv("1.data/cg.coll.reach.adult.csv")
ther <- read.csv("1.data/ther.ALL.csv")

##### prep #####
# create datetime variable for yearly plots
wild.temp.all$date.time <- paste(wild.temp.all$Date, wild.temp.all$Time)
wild.humi.all$date.time <- paste(wild.humi.all$Date, wild.humi.all$Time)

# summary stats logger height. altitude
mean(logger.info$logger.height[logger.info$canopy_understory=="c"&!(is.na(logger.info$logger.height))])
mean(logger.info$logger.height[logger.info$canopy_understory=="u"&!(is.na(logger.info$logger.height))])
mean(logger.info$point_altitude[logger.info$alt_type=="high"&!(is.na(logger.info$point_altitude))])
mean(logger.info$point_altitude[logger.info$alt_type=="low"&!(is.na(logger.info$point_altitude))])

month$alt.type_height <- paste(month$alt_type, month$height, sep="_")
month.daily$alt.type_height <- paste(month.daily$alt_type, month.daily$height, sep="_")
month.logger$alt.type_height <- paste(month.logger$alt_type, month.logger$height, sep="_")

# plotting variables
cols1 <- alpha(c("#009E73","#009E73","#D55E00","#D55E00"), 0.9)
month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ylab <- "Temperature (°C)"

##### functions #####
# modified so that the order of tukey groupings matches order of
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters4(alt.aov, TUKEY)$alt_height_slope$Letters)
  names(Tukey.labels) <- 'Letters'
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

# change facet wrap titles
alt_names <- list('high'="Highlands", 'low'="Lowlands")
alt_labeller <- function(variable,value){
  return(alt_names[value])}


########################################## FIGURE 1 #####################
############### 1. yearly raw temp  ########### 
### a1 west temp low #####
#data prep
x.axis.order <- c("low_c_west","low_u_west")
a1.dat <- subset(logger_all_temp_mean_date.time, side_andes=="west"&alt_type=="low")
a1.dat$date.time <- as.POSIXct(a1.dat$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

a1.dat.wild <- subset(wild.temp.all, side_andes=="west"&alt_type=="low")
a1.dat.wild$date.time <- as.POSIXct(a1.dat.wild$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

a1.wc.dat <- subset(month.wc, side_andes=="west"&alt_type=="low")
a1.wc.dat$date.time <- paste(a1.wc.dat$date.wc.plot, "12:00:00", sep = " ")
a1.wc.dat$date.time <- as.POSIXct(a1.wc.dat$date.time, "%d/%m/%Y %H:%M:%S", tz="Europe/London")


a1 <- ggplot(data=a1.dat, aes(x=date.time, y=value, color=alt_height_slope)) +   # the variables of interest
  geom_line(inherit.aes = FALSE,aes(x=date.time, y=value), data=a1.dat.wild,size=.7, color="lightgrey")+
  geom_line(size=.7) +  
  xlab("") + 
  ylab("Temperature (°C)") + 
  #geom_text(size=5,color="black",aes(x=as.POSIXct("2017-02-07 00:00:00", "%Y-%m-%d %H:%M:%S"),y=42, label="A1"))+
  coord_cartesian(ylim=c(10,42))+
  scale_color_manual(limits=x.axis.order,
                     values=alpha(c( "#D55E00","#0072B2"), 0.9))+
  theme_classic()+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmean"), colour="black",size=.8,alpha=.5,,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmean"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmax"), colour="black",size=.8,alpha=.5,,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmax"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmin"), colour="black",size=.8,alpha=.5,,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a1.wc.dat, type=="tmin"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); a1


### a2 west temp high #####

#data prep
x.axis.order <- c("high_c_west","high_u_west")
a2.dat <- subset(logger_all_temp_mean_date.time, side_andes=="west"|alt_type=="high")
a2.dat$date.time <- as.POSIXct(a2.dat$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")
a2.dat.wild <- subset(wild.temp.all, side_andes=="west"&alt_type=="high")
a2.dat.wild$date.time <- as.POSIXct(a2.dat.wild$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

a2.wc.dat <- subset(month.wc, side_andes=="west"&alt_type=="high")
a2.wc.dat$date.time <- paste(a2.wc.dat$date.wc.plot, "12:00:00", sep = " ")
a2.wc.dat$date.time <- as.POSIXct(a2.wc.dat$date.time, "%d/%m/%Y %H:%M:%S", tz="Europe/London")

a2 <- ggplot(data=a2.dat, aes(x=date.time, y=value, color=alt_height_slope)) +   # the variables of interest
  geom_line(inherit.aes = FALSE,aes(x=date.time, y=value), data=a2.dat.wild,size=.7, color="lightgrey")+
  geom_line(size=.7) +  
  #geom_text(size=5,color="black",aes(x=as.POSIXct("2017-02-07 00:00:00", "%Y-%m-%d %H:%M:%S"),y=42, label="A2"))+
  xlab("") +  ylab("") + 
  coord_cartesian(ylim=c(10,42))+
  scale_color_manual(limits=x.axis.order, values=alpha(c("#E69F00","#56B4E9"), 0.9))+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmean"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmean"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmax"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmax"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmin"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(a2.wc.dat, type=="tmin"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); a2

### b1 raw east temp low #####
x.axis.order <- c("low_c_east","low_u_east")
b1.dat <- subset(logger_all_temp_mean_date.time, (side_andes=="east"|alt_type=="low"))
b1.dat$date.time <- as.POSIXct(b1.dat$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

b1.dat.wild <- subset(wild.temp.all, side_andes=="east"&alt_type=="low")
b1.dat.wild$date.time <- as.POSIXct(b1.dat.wild$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

b1.wc.dat <- subset(month.wc, side_andes=="east"&alt_type=="low")
b1.wc.dat$date.time <- paste(b1.wc.dat$date.wc.plot, "12:00:00", sep = " ")
b1.wc.dat$date.time <- as.POSIXct(b1.wc.dat$date.time, "%d/%m/%Y %H:%M:%S", tz="Europe/London")

b1 <- ggplot(data=b1.dat, aes(x=date.time, y=value, color=alt_height_slope)) +   # the variables of interest
  geom_line(inherit.aes = FALSE,aes(x=date.time, y=value), data=b1.dat.wild,size=.7, color="lightgrey")+
  geom_line(size=.7) +  
  xlab("") + 
  ylab("Temperature (°C)") + 
  #geom_text(size=5,color="black",aes(x=as.POSIXct("2017-02-07 00:00:00", "%Y-%m-%d %H:%M:%S"),y=42, label="B1"))+
  coord_cartesian(ylim=c(10,42))+
  scale_color_manual(limits=x.axis.order,values=alpha(c( "#D55E00","#0072B2"), 0.9))+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmean"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmean"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmax"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmax"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmin"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b1.wc.dat, type=="tmin"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); b1

### b2 raw east temp high #####
#data prep
x.axis.order <- c("high_c_east","high_u_east")
b2.dat <- subset(logger_all_temp_mean_date.time, side_andes=="east"|alt_type=="high")
b2.dat$date.time <- as.POSIXct(b2.dat$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")
b2.dat.wild <- subset(wild.temp.all, side_andes=="east"&alt_type=="high")
b2.dat.wild$date.time <- as.POSIXct(b2.dat.wild$date.time, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

b2.wc.dat <- subset(month.wc, side_andes=="east"&alt_type=="high")
b2.wc.dat$date.time <- paste(b2.wc.dat$date.wc.plot, "12:00:00", sep = " ")
b2.wc.dat$date.time <- as.POSIXct(b2.wc.dat$date.time, "%d/%m/%Y %H:%M:%S", tz="Europe/London")

b2 <- ggplot(data=b2.dat, aes(x=date.time, y=value, color=alt_height_slope)) +   # the variables of interest
  geom_line(inherit.aes = FALSE,aes(x=date.time, y=value), data=b2.dat.wild,size=.7, color="lightgrey")+
  geom_line(size=.7) +  
  #geom_text(size=5,color="black",aes(x=as.POSIXct("2017-02-07 00:00:00", "%Y-%m-%d %H:%M:%S"),y=42, label="B2"))+
  xlab("") + 
  ylab("") + 
  coord_cartesian(ylim=c(10,42))+
  scale_color_manual(limits=x.axis.order,values=alpha(c("#E69F00","#56B4E9"), 0.9))+
  theme_classic()+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmean"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmean"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmax"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmax"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  geom_point(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmin"), colour="black",size=.8,alpha=.5)+
  geom_line(inherit.aes = FALSE, aes(x=date.time, y=mean.value),data=subset(b2.wc.dat, type=="tmin"), colour="black", linetype="dashed",alpha=.5,size=.5)+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); b2



##############  2 BOXPLOT MEANS #########
##### MAX a3, b3 ####

### a3 max west temp ####
a3.dat <- subset(daily_temp_max, side_andes=="west")
names(a3.dat)
a3.dat$alt_height_slope <- factor(a3.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))
a3.dat <- a3.dat[order(factor(a3.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))),]
  
#prep
x.axis.order <- c("low_c_west","low_u_west","high_c_west","high_u_west")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

# use this tukey function, as they are all significant anyways
# but real tukey tests are in 6.clim.an- based on LMM
alt.aov <-aov(value ~ alt_height_slope, data=a3.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(a3.dat, alt_height_slope),
                  mean=max(value))
final<-merge(labels,yvalue) 

#plot
a3 <- ggplot(a3.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-1,hjust=0) +
  ylab(c("Temperature (°C)"))+
  scale_x_discrete(labels=c("", "", "", ""))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); a3

### b3 max east temp ####
b3.dat <- subset(daily_temp_max, side_andes=="east")
names(b3.dat)
b3.dat$alt_height_slope <- factor(b3.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))
b3.dat <- b3.dat[order(factor(b3.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))),]

#prep
x.axis.order <- c("low_c_east","low_u_east","high_c_east","high_u_east")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(value ~ alt_height_slope, data=b3.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(b3.dat, alt_height_slope),
                  mean=max(value))
final<-merge(labels,yvalue) 

#plot
b3 <- ggplot(b3.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-1,hjust=0) +
  ylab(c("Temperature (°C)"))+
  scale_x_discrete(labels=c("Canopy", "Understory", "Canopy", "Understory"))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=35.5, label="b3"))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text.x = element_text(size=10, angle = 25, hjust = 1)); b3

# b3 <- ggplot_gtable(ggplot_build(b3))
# b3$layout$clip[b3$layout$name == "panel"] <- "off"
# grid.draw(b3)

##### MEAN a4, b4 ####

### a4 mean west temp ####
a4.dat <- subset(daily_temp_mean, side_andes=="west")
names(a4.dat)
a4.dat$alt_height_slope <- factor(a4.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))
a4.dat <- a4.dat[order(factor(a4.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))),]

#prep
x.axis.order <- c("low_c_west","low_u_west","high_c_west","high_u_west")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(value ~ alt_height_slope, data=a4.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(a4.dat, alt_height_slope),
                  mean=mean(value))
final<-merge(labels,yvalue) 

#plot
a4 <- ggplot(a4.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-2,hjust=0) +
  ylab(c(""))+
  scale_x_discrete(labels=c("", "", "", ""))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=35.5, label="a4"))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); a4

# a4 <- ggplot_gtable(ggplot_build(a4))
# a4$layout$clip[a4$layout$name == "panel"] <- "off"
# grid.draw(a4)

### b4 mean east temp ####
b4.dat <- subset(daily_temp_mean, side_andes=="east")
names(b4.dat)
b4.dat$alt_height_slope <- factor(b4.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))
b4.dat <- b4.dat[order(factor(b4.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))),]

#prep
x.axis.order <- c("low_c_east","low_u_east","high_c_east","high_u_east")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(value ~ alt_height_slope, data=b4.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(b4.dat, alt_height_slope),
                  mean=mean(value))
final<-merge(labels,yvalue) 

#plot
b4 <- ggplot(b4.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-3,hjust=0) +
  ylab(c(""))+
  scale_x_discrete(labels=c("Canopy", "Understory", "Canopy", "Understory"))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=35.5, label="b4"))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 25, hjust = 1)); b4

# b4 <- ggplot_gtable(ggplot_build(b4))
# b4$layout$clip[b4$layout$name == "panel"] <- "off"
# grid.draw(b4)


##### MIN a5, b5, c5, d5, ####

### a5 min west temp ####
a5.dat <- subset(daily_temp_min, side_andes=="west")
names(a5.dat)
a5.dat$alt_height_slope <- factor(a5.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))
a5.dat <- a5.dat[order(factor(a5.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))),]

#prep
x.axis.order <- c("low_c_west","low_u_west","high_c_west","high_u_west")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(value ~ alt_height_slope, data=a5.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(a5.dat, alt_height_slope),
                  mean=mean(value))
final<-merge(labels,yvalue) 

#plot
a5 <- ggplot(a5.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-2,hjust=0) +
  ylab(c(""))+
  scale_x_discrete(labels=c("", "", "", ""))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=35.5, label="a5"))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10)); a5

# a5 <- ggplot_gtable(ggplot_build(a5))
# a5$layout$clip[a5$layout$name == "panel"] <- "off"
# grid.draw(a5)

### b5 min east temp ####
b5.dat <- subset(daily_temp_min, side_andes=="east")
names(b5.dat)
b5.dat$alt_height_slope <- factor(b5.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))
b5.dat <- b5.dat[order(factor(b5.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))),]

#prep
x.axis.order <- c("low_c_east","low_u_east","high_c_east","high_u_east")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(value ~ alt_height_slope, data=b5.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(b5.dat, alt_height_slope),
                  mean=mean(value))
final<-merge(labels,yvalue) 

#plot
b5 <- ggplot(b5.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 36))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-2,hjust=0) +
  ylab(c(""))+
  scale_x_discrete(labels=c("Canopy", "Understory", "Canopy", "Understory"))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=35.5, label="b5"))+
  scale_color_manual(limits=x.axis.order,
                     values=c( "#D55E00","#0072B2", "#E69F00","#56B4E9"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#D55E00","#0072B2", "#E69F00","#56B4E9"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 25, hjust = 1)); b5

# b5 <- ggplot_gtable(ggplot_build(b5))
# b5$layout$clip[b5$layout$name == "panel"] <- "off"
# grid.draw(b5)

### A6 min west hum ####
c5.dat <- subset(daily_hum_min, side_andes=="west")
names(c5.dat)

#prep
x.axis.order <- c("low_c_west","low_u_west","high_c_west","high_u_west")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

#plot
c5 <- ggplot(c5.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(40, 115))+
  scale_x_discrete(limits=x.axis.order,
                   labels=c("", "", "", ""))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=105, label="C5"))+
  scale_color_manual(limits=x.axis.order,
                     values=c("cadetblue4", "darkorchid4", "violet", "cadetblue1"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c("cadetblue4", "darkorchid4", "violet", "cadetblue1"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10))
# annotation_custom(text_low,xmin=1,xmax=2,ymin=25,ymax=25) + 
#  annotation_custom(text_high,xmin=3,xmax=4,ymin=25,ymax=25)
c5
c5 <- ggplot_gtable(ggplot_build(c5))
c5$layout$clip[c5$layout$name == "panel"] <- "off"
grid.draw(c5)


### B6 min east hum ####
d5.dat <- subset(daily_hum_min, side_andes=="east")
names(d5.dat)

#prep
x.axis.order <- c("low_c_east","low_u_east","high_c_east","high_u_east")
text_high <- textGrob("High A", gp=gpar(fontsize=12, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=12, fontface="bold"))

#plot
d5 <- ggplot(d5.dat, aes(y=value, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(40, 115))+
  scale_x_discrete(limits=x.axis.order,
                   labels=labels=c("Canopy", "Understory", "Canopy", "Understory"))+
  #geom_text(size=4.5, color="black", aes(x=(1), y=24.5, label="b"), fontface="plain")+ #adding the superscrpts
  #geom_text(size=4.5, color="black", aes(x=(2), y=24.5, label="ab"), fontface="plain")+
  #geom_text(size=4.5, color="black", aes(x=(3), y=24.5, label="a"), fontface="plain")+
  #geom_text(size=5,color="black",aes(x=0.9,y=105, label="D5"))+
  scale_color_manual(limits=x.axis.order,
                     values=c("cadetblue4", "darkorchid4", "violet", "cadetblue1"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c("cadetblue4", "darkorchid4", "violet", "cadetblue1"), 0.6))+
  theme_classic()+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10))+
  annotation_custom(text_low,xmin=1,xmax=2,ymin=25,ymax=25) + 
  annotation_custom(text_high,xmin=3,xmax=4,ymin=25,ymax=25)
d5
d5 <- ggplot_gtable(ggplot_build(d5))
d5$layout$clip[d5$layout$name == "panel"] <- "off"
grid.draw(d5)




##### combine plots  ######
a<- plot_grid(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,rel_widths=c(2,2,1,1,1,2,2,1,1,1),
              labels = c("A1 Lowlands","A2 Highlands","A3 Daily Max.","A4 Daily Mean","A5 Daily Min.",
                                                       "B1","B2","B3","B4", "B5"), label_size = 12, 
              ncol = 5, nrow = 2,hjust = 0, label_x = 0.1, label_y=1.12, align = "hv")+
  theme(plot.margin = unit(c(1, 0, 0, 0), "cm")); a

ggsave2("../figures/fig1.temp.year.mean.min.max.wc.png",a, width = 13, height = 6, dpi = 300 )
           

########################################## FIGURE 2 #####################
###### 1. fig2A daily hours wild ######  

cols1 <- alpha(c("#E69F00","#56B4E9","#D55E00","#0072B2"), 0.9)
a <- ggplot(summ.temp.hours.side.alt.height, aes(x=Time, y=mean, shape=height))+
  #geom_smooth(method = "auto" )+
  geom_errorbar(aes(ymin =value-sd,ymax = value+sd,y=value,colour=alt.type_height),width=0.3, size=0.4,alpha=.6)+ 
  geom_point(size=3,aes(y=value, colour=alt.type_height , group=alt.type_height)) +
  geom_line(aes(y=value,colour=alt.type_height,x=Time, group=alt.type_height))+
  ylab(ylab)+ xlab("Time of the day")+
  theme_classic()+
  scale_colour_manual(name= "Altitude" , values = cols1,labels=c("Highlands\ncanopy", "Highlands\nunderstory","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_shape_manual(name="Forest layer", labels=c("Canopy", "Understory"), values = c(17,16))+
  facet_wrap(~side_andes, labeller=labeller(side_andes = capitalize), strip.position=NULL, ncol = 1)+
  facet_rep_wrap(~side_andes, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  theme(axis.text.x = element_text(angle = 35,size=10,hjust =1,color=c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text.y = element_text(size=10), legend.position = "none",
        strip.text = element_blank(),
        axis.title=element_text(size=14,face="bold", colour="black")); a

###### 2. fig. 2B BIO2 diurnal range ######

high.more35<-subset(wild.temp.all,value>=35&alt_type=="high")
low.more35<- subset(wild.temp.all,value>=35&alt_type=="low")

b <- ggplot(yearly.daily_temp_max.min.spread.area, aes(x=alt_type, y=temp.range.area.mean, shape=height, colour=alt.type_height))+
  geom_line(inherit.aes = FALSE, aes(x=alt_type, y=temp.range.area.mean, shape=height, group=height),linetype = "dashed", colour="grey") +
  geom_point(size=3) +facet_wrap(~side_andes)+
  geom_point(inherit.aes = FALSE, data=summ.localities, aes(x=alt_type, y=(wc.bio2.annual.diurnal.range/10)), 
             colour="black", shape=8, size=2, position = position_nudge(x = 0, y = 0))+ 
  geom_errorbar(aes(ymin =temp.range.area.mean-temp.range.area.mean.sd,ymax = temp.range.area.mean+temp.range.area.mean.sd,y=temp.range.area.mean.sd),width=0.03, size=0.3,alpha=.6)+ 
  ylab("Diurnal temperature range (°C)")+ xlab("")+
  #scale_colour_manual(name= "Altitude" , values = cols1, labels=c("Highlands","Highlands","Lowlands", "Lowlands"))+
  scale_colour_manual(name= "" , values = cols1,labels=c("Highlands\ncanopy\n", "Highlands\nunderstory\n","Lowlands\ncanopy\n","Lowlands\nunderstory\n"))+
  scale_shape_manual(name="Forest layer", labels=c("Canopy", "Understory"), values = c(17,16,17,16))+
  theme_classic()+
  ylim(2.5,11)+
  scale_x_discrete(labels=c("Highlands", "Lowlands"))+
  facet_wrap(~side_andes, ncol = 1, scales = "free")+
  #facet_wrap(~side_andes, labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  #facet_rep_wrap(~side_andes, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  theme(axis.text.x = element_text(angle = 0,size=12,hjust =.5),
        axis.line.x = element_line(color="black", size = 0.5),
        #strip.text = element_text(size=12, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text.y = element_text(size=10),axis.title=element_text(size=12,face="bold", colour="black")); b

###### 3. fig. 2C WC2 gets it wrong #####
cols.low <- alpha(c("grey", "#E69F00","#56B4E9"))
cols.high <- alpha(c("grey","#D55E00", "#0072B2"))

mu <- plyr::ddply(subset(daily.annual.means.per.logger, type=="temp.max"), c("height", "side_andes", "alt_type","alt.type_height"), summarise, grp.mean=mean(mean.value))
c <- ggplot(subset(daily.annual.means.per.logger, type=="temp.max"), aes(x=mean.value, colour=alt.type_height, fill=alt.type_height)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=alt.type_height),
             linetype="dashed", alpha=.6, size=1)+
  geom_density(adjust = 1.5, alpha=.6)+
  xlab("Maximum temperature (°C)")+
  ylab("Probability density")+
  facet_rep_wrap(~side_andes+alt_type, scales = "fixed")+
  scale_fill_manual(values = c("grey", "#E69F00","#56B4E9","grey", "#D55E00","#0072B2"), 
                    labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_colour_manual(values = c("grey", "#E69F00","#56B4E9","grey", "#D55E00","#0072B2"),
                      labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  #scale_colour_manual(name= "" , values = ,labels=c("Highlands\ncanopy\n", "Highlands\nunderstory\n","Lowlands\ncanopy\n","Lowlands\nunderstory\n"))+
  scale_y_continuous(expand = c(0, 0)) +
  xlim(17.8,34)+
  #scale_x_continuous(expand = c(0, 0)) +
  #facet_rep_wrap(~side_andes+alt_type, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0,size=12,hjust =.5),
        axis.line.x = element_line(color="black", size = 0.5),
        #strip.text = element_text(size=12, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),legend.title = element_blank(),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text.y = element_text(size=10),axis.title=element_text(size=12,face="bold", colour="black")); c

###### combine ######
null <- ggplot()+theme_nothing()
plot_grid(null, a, null, b,c, nrow = 1, rel_widths = c(.05,.9,.05,.75,1.4), rel_heights = c(1,1,1,1,1), 
          axis = 'b',align = "hv", labels=c("","A","","B", "C"))
ggsave("../figures/fig2.png", width = 14, height = 6, dpi = 300)


########################################## FIGURE 3 #####################
##### prep humi data #######
# create datetime variable for yearly plots
wild.temp.all$date.time <- paste(wild.temp.all$Date, wild.temp.all$Time)
wild.humi.all$date.time <- paste(wild.humi.all$Date, wild.humi.all$Time)

# add variables
wild.temp.all$altitude <- logger.info$point_altitude[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$point <- logger.info$point[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$point <- logger.info$point[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$side.alt <- paste(wild.temp.all$side_andes,wild.temp.all$alt_type, sep = ".")

# add variables
wild.humi.all$altitude <- logger.info$point_altitude[match(wild.humi.all$datalogger_id, logger.info$code_mine)]
wild.humi.all$point <- logger.info$point[match(wild.humi.all$datalogger_id, logger.info$code_mine)]
wild.humi.all$point <- logger.info$point[match(wild.humi.all$datalogger_id, logger.info$code_mine)]
wild.humi.all$side.alt <- paste(wild.humi.all$side_andes,wild.humi.all$alt_type, sep = ".")

# summary stats logger height. altitude
mean(logger.info$logger.height[logger.info$canopy_understory=="c"&!(is.na(logger.info$logger.height))])
mean(logger.info$logger.height[logger.info$canopy_understory=="u"&!(is.na(logger.info$logger.height))])
mean(logger.info$point_altitude[logger.info$alt_type=="high"&!(is.na(logger.info$point_altitude))])
mean(logger.info$point_altitude[logger.info$alt_type=="low"&!(is.na(logger.info$point_altitude))])

display.brewer.pal(n = 8, name ="cbp1")

cbp1 <- c("#999999", "#E69F00", "#D55E00", "#009E73",
          "#F0E442", "#0072B2"," #D55E00", "#CC79A7")

##### daily means ####
names(wild.temp.all)
daily_temp_mean <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id,point,side.alt),
                                    value=mean(value),
                                    sd=mean(sd(value)),
                                    type=paste("temp.mean"))
daily_temp_min <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id,point,side.alt),
                                   value=min(value),
                                   sd=min(sd(value)),
                                   type=paste("temp.min"))
daily_temp_max <- dplyr::summarise(group_by(wild.temp.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id,point,side.alt),
                                   value=max(value),
                                   sd=max(sd(value)),
                                   type=paste("temp.max"))

daily_temp_mean$alt.type_height <- paste(daily_temp_mean$alt_type,daily_temp_mean$height, sep="_")
daily_temp_max$alt.type_height <- paste(daily_temp_max$alt_type,daily_temp_max$height, sep="_")
daily_temp_min$alt.type_height <- paste(daily_temp_min$alt_type,daily_temp_min$height, sep="_")


daily_temp_mean$Date <- as.POSIXct(daily_temp_mean$Date, "%Y-%m-%d", tz="Europe/London")
daily_temp_max$Date <- as.POSIXct(daily_temp_max$Date, "%Y-%m-%d", tz="Europe/London")
daily_temp_min$Date <- as.POSIXct(daily_temp_min$Date, "%Y-%m-%d", tz="Europe/London")

daily <- rbind(daily_temp_max,daily_temp_mean,daily_temp_min)

##### table1 daily #####
head(daily)
daily.summ <- summarise(group_by(daily, alt_type, side_andes, type, height),
                        tmean=mean(value),
                        tmean.se= sd(value) / sqrt(length(value))); daily.summ

neworder1 <- c("high","low"); neworder2 <- c("west","east"); neworder3 <- c("temp.max","temp.mean","temp.min")
daily.summ  <- plyr::arrange(transform(daily.summ, alt_type=factor(alt_type,levels=neworder1)),
                             alt_type,side_andes,height,type);daily.summ 
daily.summ$tmean <- round(daily.summ$tmean, 2)
daily.summ$tmean.se <- round(daily.summ$tmean.se, 2)
daily.summ$value.c <- paste(daily.summ$tmean, daily.summ$tmean.se, sep="±")
daily.summ$alt.height <- paste(daily.summ$alt_type, daily.summ$height, sep=".")
daily.summ$type.height <- paste(daily.summ$type, daily.summ$height, sep=".")

names(daily.summ)

#daily.summ <- daily.summ[,-c(1,4,5,6)];daily.summ
daily.summ <- daily.summ[,-c(4,3,5,6,8)];daily.summ
daily.summ.spread <- spread(daily.summ, type.height, value.c); daily.summ.spread
write.csv(daily.summ.spread,"data/daily.summ.table.csv", row.names = FALSE)

# humidity
names(wild.humi.all)

daily_hum_min <- dplyr::summarise(group_by(wild.humi.all, Date, alt_height_slope, alt_type, side_andes, height, alt.type_height),
                                  value=min(value),
                                  sd=min(sd(value)),
                                  type=paste("hum.min"))

##### estimate VPD and annual mean / daily max ###############
# we need RH and T in the same place
head(wild.humi.all)
wild.humi.all$value.type
# so add T to RH dataset (and not the other way around, remember we have many more Tonly loggers)
wild.humi.all$temp <- wild.temp.all$value[match(paste(wild.humi.all$date.time,wild.humi.all$datalogger_id), paste(wild.temp.all$date.time,wild.temp.all$datalogger_id))]

names(wild.humi.all)[4] <- "humi"
e <- exp(1) ; e
wild.humi.all$vpd <- ((100-wild.humi.all$humi)/100)*
  (6.112*e^((17.67*wild.humi.all$temp)/(wild.humi.all$temp+243.5)))

##### get yearly mean and daily max mean #####
# max, mean min per datalogger per day
daily_humi_mean <- dplyr::summarise(group_by(wild.humi.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                    vpd.mean=mean(vpd),
                                    temp.mean=mean(temp),
                                    humi.mean=mean(humi),
                                    type=paste("humi.mean"))

daily_humi_min <- dplyr::summarise(group_by(wild.humi.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                   vpd=min(vpd),
                                   temp.min=min(temp),
                                   humi.min=min(humi),
                                   type=paste("humi.min"))

daily_humi_max <- dplyr::summarise(group_by(wild.humi.all, Date, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                   vpd=max(vpd),
                                   temp.max=max(temp),
                                   humi.max=max(humi),
                                   type=paste("humi.max"))

# now do year vpdmax, mean, min, per logger

yearly_daily_humi_mean <- dplyr::summarise(group_by(daily_humi_mean, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                           vpd.mean.year=mean(vpd.mean),
                                           temp.mean.year=mean(temp.mean),
                                           humi.mean.year=mean(humi.mean),
                                           type=paste("humi.mean"))

yearly_daily_humi_max <- dplyr::summarise(group_by(daily_humi_max, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                          vpd.max.year=mean(vpd),
                                          temp.max.year=mean(temp.max),
                                          humi.max.year=mean(humi.max),
                                          type=paste("humi.max"))

#mostly 0 for VPD, not very relevant, but RH interesting
yearly_daily_humi_min <- dplyr::summarise(group_by(daily_humi_min, alt_height_slope, alt_type, side_andes, height, datalogger_id),
                                          vpd.min.year=mean(vpd),
                                          temp.min.year=mean(temp.min),
                                          humi.min.year=mean(humi.min),
                                          type=paste("humi.min"))

##### Fig. 3 daily max/mean VPD ###############
names(yearly_daily_humi_mean)
cols1 <- alpha(c("#E69F00","#56B4E9","#D55E00","#0072B2"), 0.9)
yearly_daily_humi_mean$alt_height <- paste(yearly_daily_humi_mean$alt_type,yearly_daily_humi_mean$height, sep="_")
yearly_daily_humi_max$alt_height <- paste(yearly_daily_humi_max$alt_type,yearly_daily_humi_max$height, sep="_")

vpd.mean.p <- ggplot(aes(y=vpd.mean.year, x=alt_type, fill=alt_height), data=yearly_daily_humi_mean)+
  #geom_point(aes(fill = alt_height), size = 1, shape = 21, position = position_jitterdodge()) +
  geom_boxplot(aes(fill=alt_height))+
  stat_cor(show.legend = FALSE)+
  stat_compare_means(aes(label = sprintf("p = %5.3f", as.numeric(..p.format..))),method = "t.test")+
  #geom_rug(alpha=.05)+
  ylim(0,9.5)+
  xlab("Altitude")+ ylab(expression(bold("VPD"["mean"])))+
  #scale_colour_manual(name="Altitude", values = cols1)+
  scale_fill_manual(name="", values = cols1)+
  #facet_wrap(~side_andes, labeller=labeller(side_andes = capitalize), strip.position=NULL, ncol = 2)+
  # facet_rep_wrap(~side_andes, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=14),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="italic"),
         legend.position = "bottom"); vpd.mean.p

vpd.max.p <- ggplot(aes(y=vpd.max.year, x=alt_type, fill=alt_height), data=yearly_daily_humi_max)+
  #geom_point(aes(fill = alt_height), size = 1, shape = 21, position = position_jitterdodge()) +
  geom_boxplot(aes(fill=alt_height))+
  stat_cor(show.legend = FALSE)+
  # stat_compare_means(label="p.signif", method = "t.test")+
  stat_compare_means(aes(label = sprintf("p = %5.3f", as.numeric(..p.format..))) ,method = "t.test")+
  ylim(0,9.5)+
  xlab("Altitude")+ ylab(expression(bold("VPD"["max"])))+
  #scale_colour_manual(name="Altitude", values = cols1)+
  scale_fill_manual(name="", values = cols1)+
  #facet_wrap(~side_andes, labeller=labeller(side_andes = capitalize), strip.position=NULL, ncol = 2)+
  # facet_rep_wrap(~side_andes, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=14),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="italic"),
         legend.position = "bottom"); vpd.max.p

plot_grid(vpd.max.p, vpd.mean.p)
ggsave("../figures/fig3.VPD.pdf", width = 7, height = 4, dpi = 300)

########################################## FIGURE 4 #####################
#### data prep ####
ther$altitude <- as.numeric(as.character(ther$altitude))
coll$adult.mass <- as.numeric(as.character(coll$adult.mass))
coll$pupa.clean.mass <- as.numeric(as.character(coll$pupa.clean.mass ))

# convert minss to hms
coll$mins.40.to.ko <- as.hms(as.character(coll$time.40.to.ko))
coll$mins.to.40 <- as.hms(as.character(coll$time.to.40))
coll$mins.ko.total <- as.hms(as.character(coll$time.ko.total))

ther$mins.40.to.ko <- as.hms(as.character(ther$time.40.to.ko))
ther$mins.to.40 <- as.hms(as.character(ther$time.to.40))
ther$mins.ko.total <- as.hms(as.character(ther$time.ko.total))
ther$temp.at.ko <- as.numeric(sub(",", ".", ther$temp.at.ko))

coll$mins.40.to.ko <- as.double.difftime(coll$mins.40.to.ko)/60
coll$mins.to.40 <- as.double.difftime(coll$mins.to.40)/60
coll$mins.ko.total <- as.double.difftime(coll$mins.ko.total)/60

ther$mins.40.to.ko <- as.double.difftime(ther$mins.40.to.ko)/60
ther$mins.to.40 <- as.double.difftime(ther$mins.to.40)/60
ther$mins.ko.total <- as.double.difftime(ther$mins.ko.total)/60

# make mid as low
ther$type.alt1 <- if_else(ther$altitude>700, "high", "low")
coll$type.alt1 <- if_else(coll$mother.alt>700, "high", "low")

#### wild subset #####
# subset by number of indivs, remove species with less than 5 indivs,n=14
ther.wild <- subset(ther, type.reared=="wild")
sp.no <- dplyr::summarise(group_by(ther.wild, species, type.alt1),
                          mean.alt=mean(altitude),
                          n=n()); sp.more.5 <- subset(sp.no, n>4)

ther.wild <- subset(ther.wild, paste(species, type.alt1) %in% paste(sp.more.5$species, sp.more.5$type.alt1))

# subset by exp success
ther.wild.tt <- subset(ther.wild, type.reared=="wild"& exp.success=="yes")

######### era for SIZE
# erato residuals controlling for mins to ko, do models SEPARATELY
era.size <- subset(ther, species=="erato"&type.alt1!="")

######### era for TT
# erato residuals controlling for mins to ko, do models SEPARATELY
era.tt <- subset(ther, species=="erato"&type.alt1!=""&mins.40.to.ko!="")
era.tt <- subset(ther, species=="erato"&type.alt1!=""&!(is.na(mins.40.to.ko))&temp.at.ko<41.1) #402-316/329, 86/73 above 41
era.tt.res <- subset(ther, species=="erato"&type.alt1!=""&!(is.na(mins.40.to.ko))&!(is.na(mins.to.40))&temp.at.ko<41.1) #402-316/329, 86/73 above 41

# erato reared residuals -sig
lm1 <- lm(mins.40.to.ko ~ mins.to.40, data=subset(era.tt.res, type.reared=="reared")); summary(lm1)
era.tt.res$mins.ko.residuals[era.tt.res$type.reared=="reared"] <- residuals(lm1)

# erato wild residuals -sig
lm1 <- lm(mins.40.to.ko ~ mins.to.40, data=subset(era.tt.res, type.reared=="wild")); summary(lm1)
era.tt.res$mins.ko.residuals[era.tt.res$type.reared=="wild"] <- residuals(lm1)

######### coll era for TT
# coll era data
coll.era.tt <- subset(coll, exp.success=="yes"&temp.at.ko<41) #203

# erato sub residuals -sig
str(coll.era.sub)
lm1 <- lm(mins.40.to.ko ~ mins.to.40, data=coll.era.tt); summary(lm1)
coll.era.tt$mins.ko.residuals[!(is.na(coll.era.tt$mins.to.40))] <- residuals(lm1)
mean(ther$altitude[!(is.na(ther$altitude))&ther$type.alt1=="low"]); mean(ther$altitude[!(is.na(ther$altitude))&ther$type.alt1=="high"])

#### 1. Fig. 4A ####################
neworder <- c("erato", "timareta", "melpomene", "sara", "clysonymus", "hierax", "telesiphe", "aoede", "elevatus","wallacei")
ther.wild.tt <- plyr::arrange(transform(ther.wild.tt,species=factor(species,levels=neworder)),species)
sp.ther.cols1 <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#999999", "#999999", "#999999", "#999999", "#999999" ,"#999999")

## at what temperature do high altitude specialists collapse?
## what percentage of high altitude specialists collapse 35-39C?
ther.wild.tt$range <- if_else((ther.wild.tt$species=="erato"|ther.wild.tt$species=="sara"|
                                 ther.wild.tt$species=="melpomene"|ther.wild.tt$species=="timareta"), "wide-range","specialist")

ther.wild.tt$temp.ko.range <- if_else((ther.wild.tt$temp.at.ko<39&ther.wild.tt$temp.at.ko>35), "mid-hot-range","extreme")

mean.temps <- dplyr::summarise(group_by(ther.wild.tt, type.alt1,range,temp.ko.range ),
                               mean.temp.at.ko=median(temp.at.ko),
                               min.temp.at.ko=min(temp.at.ko),
                               max.temp.at.ko=max(temp.at.ko),
                               n=n(),
                               se= sd(temp.at.ko) / sqrt(n))

# mean and SE for plot
mean.KO.times<- dplyr::summarise(group_by(ther.wild.tt, type.alt1),
                                 mean.ko=mean(mins.40.to.ko),
                                 sd.ko=sd(mins.40.to.ko),
                                 mean.temp.at.ko=mean(temp.at.ko),
                                 n=n(),
                                 se= sd(mins.40.to.ko) / sqrt(n))


a.wild.tt <- ggplot(ther.wild.tt , aes(x=species, y=mins.40.to.ko,fill=species)) + 
  geom_boxplot(outlier.shape = NA, coef=0)+
  geom_hline(aes(yintercept = mean.ko), linetype="dashed", data=mean.KO.times, colour="darkgrey")+
  geom_rect(inherit.aes = FALSE,aes(xmin=0.1,xmax=7.9, ymin=(mean.KO.times$mean.ko-mean.KO.times$se), 
                                    ymax=(mean.KO.times$mean.ko+mean.KO.times$se)), data=mean.KO.times, alpha=0.3, fill="red")+
  stat_boxplot(geom ='errorbar', width = 0.2, alpha=.8) +
  geom_boxplot(outlier.shape = NA, coef=0)+
  facet_wrap(~type.alt1, scales = "free_x",labeller =labeller(type.alt1=alt_labeller))+
  stat_n_text(size = 4) + 
  geom_beeswarm(width = 0.1, alpha=.35, size=1.5, cex = .75)+
  scale_fill_manual(values = sp.ther.cols1)+
  scale_colour_manual(values = sp.ther.cols1)+
  scale_x_discrete(labels=c(erato="era", timareta="tim", melpomene="mel",sara="sar",clysonymus="cly", hierax="hie",telesiphe="tel",aoede= "aoe",elevatus="ele",wallacei="wall"))+
  xlab("")+ ylab("Knockout time (minutes)")+  
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=14, face="italic"),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="bold"),
         legend.position = "none"); a.wild.tt

#### 4. Fig. 4B ####################
ther.wild.tt$status <- 2
min(ther.wild.tt$temp.at.ko)
p.wild<- ggsurvplot(
  fit = survfit(Surv(temp.at.ko, status) ~ type.alt1, data = subset(ther.wild.tt, temp.at.ko<49)), 
  xlab = "Temperature (°C)", 
  xlim=c(28.5,39),
  axes.offset=FALSE,
  ylim=c(0,1.03),
  ylab = "Knockout resistance probability", 
  ggtheme =  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    axis.line=element_blank(),panel.background = element_blank(),
                    axis.text.x = element_text(size=14),
                    axis.text.y = element_text(size=12),
                    axis.title.x = element_text(face="bold", size=14),
                    axis.title.y = element_text(face="bold", size=14),
                    panel.border = element_rect( fill = NA, size = 1),
                    strip.text = element_text(size=14, face="italic"),
                    legend.position = "none"),
  conf.int=TRUE, 
  pval=TRUE,pval.coord = c(28.75,0.05), 
  palette=c("black", "black"),
  linetype = c("dotted", "solid"),
  legend.labs = c("Highland","Lowland"),
  legend.title=c("Individual altitude"),
  break.time.by = 1); p.wild

p.wild.test <- ggsurvplot(
  fit = survfit(Surv(as.numeric(mins.40.to.ko), status) ~ type.alt1, data = subset(ther.wild.tt, type.reared=="wild")), 
  xlab = "Time (minutes)", 
  #fun = "pct",
  ylab = "", size=1,
  conf.int=TRUE, pval=TRUE,pval.coord = c(1, .05),
  xlim=c(0.3,60), 
  axes.offset=FALSE,
  ylim=c(0,1.03),
  ggtheme =  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    axis.line=element_blank(),panel.background = element_blank(),
                    axis.text.x = element_text(size=14),
                    axis.text.y = element_text(size=12),
                    axis.title.x = element_text(face="bold", size=14),
                    axis.title.y = element_text(face="bold", size=14),
                    panel.border = element_rect( fill = NA, size = 1),
                    strip.text = element_text(size=14, face="italic")),
  palette=c("black","black"), 
  linetype = c("dotted", "solid"),
  legend.labs = c("Highland","Lowland"),
  legend.title=c("Individual altitude"),
  break.time.by = 5); p.wild.test

#### combine ########### 
bottomrow <- plot_grid(p.wild$plot, p.wild.test$plot, labels = c("B","C"));bottomrow
plot_grid(a.wild.tt, bottomrow  , nrow = 2,ncol=1, rel_widths = c(1,.8), rel_heights = c(1,.9), scale = c(1,0.99), 
          labels=c("A"))


########################################## FIGURE 5 #####################
era.cols <- c("#E69F00","#D55E00")
my_comparisons <- list( c("high", "low"))
neworder <- c("wild","reared")
era.tt <- plyr::arrange(transform(era.tt,type.reared=factor(type.reared,levels=neworder)),type.reared)
# remove mother 20? and mothers from baños butterfly house (1600m.a.s.l)- never included
era.wild.reared.tt.plot <- ggplot(subset(era.tt,mother.id!="20?"&altitude<1500), aes(x=type.alt1, y=mins.40.to.ko, fill=type.alt1)) + 
  stat_boxplot(geom ='errorbar', width = 0.2, alpha=.8) +
  geom_boxplot(outlier.shape = NA, coef=0, alpha=1)+
  facet_wrap(~type.reared, scales = "free_x", labeller =labeller(type.reared=capitalize))+
  #geom_jitter(width = 0.1, alpha=.35, size=1)+
  geom_beeswarm( alpha=.5, size=1.7, cex = 1.7, color="black", fill="black", shape=21)+
  stat_n_text(size = 4) +   
  scale_fill_manual(values = era.cols)+
  #stat_compare_means(method="t.test", label = "p.signif", comparisons = my_comparisons)+
  xlab("Population")+ ylab("Knockout time (minutes)")+  
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=12),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="bold"),
         legend.position = "none"); era.wild.reared.tt.plot
ggsave("figures/fig4.KO.era.raw.png",width = 6,height = 5, dpi = 300)

########################################## FIGURE S1 ##########################################
#### packages ######

rm(list=ls())
dev.off()
library(sp)
library(reshape2)
library(vegan)
library(gridExtra)
library(raster)
library(maps)
library(mapdata)
library(googleway)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
require(mapdata)
library(ggspatial)
library(ggrepel)
library(ggplot2)
library(elevatr)
library(sf)
library(sp)
library(ggmap)
library(viridis)

#### 0. data ######
setwd("/Users/gabrielamontejokovacevich/Dropbox (Cambridge University)/PhD/21_paper2/1.microclimates")
logger.info <- read.csv("data/logger.info.csv")

#### 1. fig1 map ###########

world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw())

### SA insert #####
# ec map coords xlim=c(-81,-76), ylim=c(-2,1.5),
ggplot(data = world) +
  geom_sf(color="darkgrey") + ylab("Latitude") + xlab("Longitude")+
  coord_sf( xlim=c(-88,-65), ylim=c(-10,10), expand = FALSE)+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "white"))+
  scale_color_viridis(name="Altitude\n(m.a.s.l)")+
  scale_y_continuous(breaks=c(-10,0,10))+
  scale_x_continuous(breaks=c(-85, -70,  -60))+
  geom_rect(mapping = aes(xmin=-81, xmax=-76, ymin=-2, ymax=1.5),fill=NA,color="black", alpha=.5)+
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.1, "in"), height = unit(1, "cm"), width = unit(1, "cm"))+
  annotation_scale(location = "bl", width_hint = 0.08, line_width = 1)

ggsave("../figures/SOM/SA.insert.png", width = 6, height = 4, dpi = 300)

### Ecuador map ####
head(logger.info)
loc <- dplyr::summarise(group_by(logger.info, alt_type, side_andes),
                        n=n(),
                        lat=mean(latitude),
                        lon=mean(longitude))
# create df  input SpatialPoints
loc.df <- data_frame(x=loc$lon, y=loc$lat)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# SpatialPoints
loc.sp <- SpatialPoints(loc.df, proj4string = CRS(prj_dd))
## Merging DEMs
elevation_df <- get_elev_raster(loc.sp, prj = prj_dd, z = 8)

## Reprojecting DEM to original projection
elevation_df <- get_elev_raster(loc.sp, prj = prj_dd, z = 6)
jpeg("../figures/SOM/elev.map.points.far.png", width = 15, height = 11.5, units = "cm", res = 300)
plot(elevation_df, col = viridis(20, alpha =.9), xlim=c(-81,-76), ylim=c(-2,1.5),
     xlab=c("Longitude"), ylab=c("Latitude"))
plot(loc.sp, add = TRUE, pch=c(24,24,21,21), col="black", bg=alpha("#E69F00",.9), cex=2)
dev.off()



########################################## FIGURE S2 ##########################################
#### 1. s2a - mean ####
mu <- plyr::ddply(subset(daily.annual.means.per.logger, type=="temp.mean"), c("height", "side_andes", "alt_type","alt.type_height"), summarise, grp.mean=mean(mean.value))
s2.a <- ggplot(subset(daily.annual.means.per.logger, type=="temp.mean"), aes(x=mean.value, colour=alt.type_height, fill=alt.type_height)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=alt.type_height),
             linetype="dashed", alpha=.6, size=1)+
  geom_density(adjust = 1.5, alpha=.6)+
  xlab("Mean temperature (°C)")+
  ylab("Probability density")+
  facet_rep_wrap(~side_andes+alt_type, scales = "fixed")+
  scale_fill_manual(values = c("grey", "#56B4E9","#E69F00","grey", "#0072B2","#D55E00"), 
                    labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_colour_manual(values = c("grey", "#56B4E9","#E69F00","grey", "#0072B2","#D55E00"),
                      labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_y_continuous(expand = c(0, 0)) +
  xlim(18,25)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0,size=12,hjust =.5),
        axis.line.x = element_line(color="black", size = 0.5),
        #strip.text = element_text(size=12, face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),legend.title = element_blank(),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        legend.position = "none",
        axis.text.y = element_text(size=10),axis.title=element_text(size=12,face="bold", colour="black")); s2.a

#### 2. s2b - mean ####
mu <- plyr::ddply(subset(daily.annual.means.per.logger, type=="temp.min"), c("height", "side_andes", "alt_type","alt.type_height"), summarise, grp.mean=mean(mean.value))
s2.b <- ggplot(subset(daily.annual.means.per.logger, type=="temp.min"), aes(x=mean.value, colour=alt.type_height, fill=alt.type_height)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=alt.type_height),
             linetype="dashed", alpha=.6, size=1)+
  geom_density(adjust = 1.5, alpha=.6)+
  xlab("Minimum temperature (°C)")+
  ylab("Probability density")+
  facet_rep_wrap(~side_andes+alt_type, scales = "fixed")+
  scale_fill_manual(values = c("grey", "#56B4E9","#E69F00","grey", "#0072B2","#D55E00"), 
                    labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_colour_manual(values = c("grey", "#56B4E9","#E69F00","grey", "#0072B2","#D55E00"),
                      labels=c("WorldClim2","Highlands\ncanopy", "Highlands\nunderstory","WorldClim2","Lowlands\ncanopy","Lowlands\nunderstory"))+
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0,size=12,hjust =.5),
        axis.line.x = element_line(color="black", size = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),legend.title = element_blank(),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text.y = element_text(size=10),axis.title=element_text(size=12,face="bold", colour="black")); s2.b

########################################## FIGURE S3 ##########################################
s3.west.dat <- subset(daily_humi_min, side_andes=="west")
s3.west.dat$alt_height_slope <- factor(s3.west.dat$alt_height_slope, levels = c("low_c_west", "low_u_west","high_c_west", "high_u_west"))

s3.west.dat$alt_height <- paste(s3.west.dat$alt_type, s3.west.dat$height, sep = "_")

#prep
x.axis.order <- c("low_c_west","low_u_west","high_c_west","high_u_west")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(humi.min ~ alt_height_slope, data=s3.west.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(s3.west.dat, alt_height_slope),
                         mean=mean(humi.min))
final<-merge(labels,yvalue) 

#plot
s3.west <- ggplot(s3.west.dat, aes(y=humi.min, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 120))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-2,hjust=0) +
  scale_color_manual(limits=x.axis.order,
                     values=c( "#0072B2","#D55E00", "#56B4E9","#E69F00"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#0072B2","#D55E00", "#56B4E9","#E69F00"), 0.6))+
  theme_classic()+
  ylab(c(""))+
  scale_x_discrete(labels=c("", "", "", ""))+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 25, hjust = 1)); s3.west
## east
s3.east.dat <- subset(daily_humi_min, side_andes=="east")
s3.east.dat$alt_height_slope <- factor(s3.east.dat$alt_height_slope, levels = c("low_c_east", "low_u_east","high_c_east", "high_u_east"))
s3.east.dat$alt_height <- paste(s3.east.dat$alt_type, s3.east.dat$height, sep = "_")

#prep
x.axis.order <- c("low_c_east","low_u_east","high_c_east","high_u_east")
text_high <- textGrob("High A", gp=gpar(fontsize=10, fontface="bold"))
text_low <- textGrob("Low A", gp=gpar(fontsize=10, fontface="bold"))

alt.aov <-aov(humi.min ~ alt_height_slope, data=s3.east.dat); summary(alt.aov) #correct order
TUKEY <- TukeyHSD(x=alt.aov, 'alt_height_slope' , conf.level=0.95);TUKEY
labels<- generate_label_df(TUKEY, "alt_height_slope")
names(labels)<-c('Letters', 'alt_height_slope')
yvalue<-dplyr::summarise(group_by(s3.east.dat, alt_height_slope),
                         mean=mean(humi.min))
final<-merge(labels,yvalue) 

#plot
s3.east <- ggplot(s3.east.dat, aes(y=humi.min, x=alt_height_slope,color=alt_height_slope)) +
  geom_boxplot(aes(fill=alt_height_slope))+labs(y="")+labs(x="")+
  coord_cartesian(ylim = c(14, 120))+
  geom_text(data = final, aes(x = alt_height_slope, y = mean, label = Letters),vjust=-2,hjust=0) +
  scale_color_manual(limits=x.axis.order,
                     values=c( "#0072B2","#D55E00", "#56B4E9","#E69F00"))+
  scale_fill_manual(limits=x.axis.order,
                    values = alpha(c( "#0072B2","#D55E00", "#56B4E9","#E69F00"), 0.6))+
  theme_classic()+
  ylab(c(""))+
  scale_x_discrete(labels=c("Canopy", "Understory", "Canopy", "Understory"))+
  theme(legend.position="none")+ # Remove legend
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        plot.margin = unit(c(0,0,1,0), "lines"),
        axis.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 25, hjust = 1)); s3.east

## combine
plot_grid(NULL, s3.west, s3.east , nrow = 3,ncol=1, rel_widths = c(1,1,1), rel_heights = c(1,.9,1), scale = c(1,.95,.95), 
          labels=c("","A West", "B East"), label_x = .04, label_y = 1.1)
ggsave("../figures/SOM/figs3.png", width = 3, height = 8, dpi = 300)

########################################## FIGURE S4 ##########################################
# relationship between temperature and vpd
cols1 <- alpha(c("gold4","deepskyblue4","gold4","deepskyblue4"), 0.9)
temp.vpd.p <- ggplot(aes(x=vpd, y=temp, colour=alt_type, fill=alt_type), data=wild.humi.all)+
  geom_point(alpha=.05,size=.1)+
  stat_cor(show.legend = FALSE)+
  geom_smooth()+ ylab("Temperature (°C)")+ xlab("Vapour pressure deficit (VPD)")+
  scale_colour_manual(name="Altitude", values = cols1)+
  scale_fill_manual(name="Altitude", values = cols1)+
  # facet_wrap(~side_andes, labeller=labeller(side_andes = capitalize), strip.position=NULL, ncol = 1)+
  # facet_rep_wrap(~side_andes, repeat.tick.labels = FALSE,labeller=labeller(side_andes = capitalize), strip.position="right", ncol = 1)+
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=14),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="italic"),
         legend.position = "bottom"); temp.vpd.p
ggsave("../figures/SOM/temp.vs.VPD.png")

########################################## FIGURE S5 ##########################################
# to check spread of temperature at KO
ggplot(aes(x=temp.at.ko),data=ther.wild.tt)+
  geom_histogram(bins=20)+facet_wrap(~type.alt1, labeller=alt_labeller, scales = "free_y")+
  geom_vline(xintercept = c(39,41), linetype="dashed", colour="blue")+
  xlab("Temperature at K.O. (°C)")+ylab(c("Count"))+
  theme_classic()+
  theme( axis.line=element_blank(),
         axis.text.x = element_text(size=12),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_rect( fill = NA, size = 1),
         strip.text = element_text(size=14, face="bold"),
         legend.position = "none")
ggsave("figures/SOM/temp.at.KO.png", width = 8, height = 4, dpi = 300)

########################################## FIGURE S6 ##########################################
ther.wild.tt.within <- subset(ther.wild.tt, species=="erato"|species=="melpomene"|species=="sara"|species=="timareta")
sp.ther.cols <- c("#E69F00","#CC79A7","#56B4E9", "#009E73","#999999","#999999","#999999","#999999","#999999","#999999","#999999","#999999")

wild.within.sp.tt <- ggplot(ther.wild.tt.within , aes(x=type.alt1, y=mins.40.to.ko, fill=species)) + 
  geom_boxplot(outlier.shape = NA, coef=0)+
  stat_boxplot(geom ='errorbar', width = 0.2, alpha=.8) +
  geom_boxplot(outlier.shape = NA, coef=0)+
  scale_fill_manual(values = sp.ther.cols)+
  facet_wrap(.~species,labeller =labeller(type.alt1=alt_labeller),ncol = 4)+
  #stat_n_text(size = 4, inherit.aes = FALSE, aes(x=type.alt1, y=mins.40.to.ko, fill=species))+
  stat_compare_means(method="t.test", inherit.aes = FALSE, aes(x=type.alt1, y=mins.40.to.ko), label = "p.signif",
                     label.x.npc = 0.5, label.y.npc = .99)+
  #stat_pvalue_manual(stat.test, label = "p.adj", y.position = 35)+
  geom_beeswarm(width = 0.1, alpha=.35, size=1.5, cex = .75)+
  scale_colour_manual(values = sp.ther.cols)+
  #(labels=c("H. erato", "H. timareta", "H. melpomene", "H. sara"))+
  xlab("Altitude")+ ylab("Knockout time (minutes)")+  
  theme_classic()+
  theme( axis.text.x = element_text(size=14),
         axis.text.y = element_text(size=12),
         axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         panel.border = element_blank(),
         strip.text = element_text(size=14, face="italic"),
         strip.background = element_blank(),
         axis.line = element_line(colour = "black"),
         legend.position = "none"); wild.within.sp.tt
ggsave("figures/SOM/within.sp.wild.TT.png", width = 8, height = 4, dpi = 300)



########################################## SOM

