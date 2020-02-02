### Montejo-Kovacevich, G., Martin, S.H., Meier, J.I., Bacquet, C.N., Monllor, M., Jiggins, C.D. and Nadeau, N.J., 2020. ###
### Microclimate buffering and thermal tolerance across elevations in a tropical butterfly. ###
### Journal of Experimental Biology. ######
############### lmes for tukey tests in figure 1 ###############
##### packages ######

rm(list=ls())
dev.off()
library(emmeans)
library(multcomp)
library(emmeans)
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
library(lme4)
library(lmerTest)
library(egg)
library(broom)
library(MuMIn)
library(piecewiseSEM)
library(stargazer)
library(ggpubr)

############### 0. data ######
setwd("microclimate.thermal.tolerance.Heliconius-JEB-2020/")
logger.info <- read.csv("1.data/logger.info.csv")
wild.temp.all <- read.csv("1.data/wild.temp.data.csv")
wild.humi.all <- read.csv("1.data/wild.humi.data.csv")

month <- read.csv("1.data/1.monthly.raw.temps.wild.wc.comb.csv")
month.daily <- read.csv("1.data/2.monthly.daily.temps.wild.wc.comb.csv")
month.logger <- read.csv("1.data/3.monthly.logger.temps.wild.wc.comb.csv") #(first obtain logger (x7 per area) means, then average through months)
monthly.loggeraveraged.means <- read.csv("1.data/monthly.loggeraveraged.means.csv")

iso.df <- read.csv("1.data/iso.df.csv")
summ.localities <- read.csv("1.data/summ.localities.csv")

# create datetime variable for yearly plots
wild.temp.all$date.time <- paste(wild.temp.all$Date, wild.temp.all$Time)
wild.humi.all$date.time <- paste(wild.humi.all$Date, wild.humi.all$Time)
wild.temp.all$altitude <- logger.info$point_altitude[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$point <- logger.info$point[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$point <- logger.info$point[match(wild.temp.all$datalogger_id, logger.info$code_mine)]
wild.temp.all$side.alt <- paste(wild.temp.all$side_andes,wild.temp.all$alt_type, sep = ".")
month$sd <- as.numeric(as.character(month$sd))
month.daily$sd <- as.numeric(as.character(month.daily$sd))
month.logger$sd <- as.numeric(as.character(month.logger$sd))

# summary stats logger height. altitude
mean(logger.info$logger.height[logger.info$canopy_understory=="c"&!(is.na(logger.info$logger.height))])
mean(logger.info$logger.height[logger.info$canopy_understory=="u"&!(is.na(logger.info$logger.height))])
mean(logger.info$point_altitude[logger.info$alt_type=="high"&!(is.na(logger.info$point_altitude))])
mean(logger.info$point_altitude[logger.info$alt_type=="low"&!(is.na(logger.info$point_altitude))])

display.brewer.pal(n = 8, name ="cbp1")
cbp1 <- c("#999999", "#E69F00", "#D55E00", "#009E73",
          "#F0E442", "#0072B2"," #D55E00", "#CC79A7")

month$alt.type_height <- paste(month$alt_type, month$height, sep="_")
month.daily$alt.type_height <- paste(month.daily$alt_type, month.daily$height, sep="_")
month.logger$alt.type_height <- paste(month.logger$alt_type, month.logger$height, sep="_")

##### daily means- prep data ####
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
write.csv(daily.summ.spread,"1.data/daily.summ.table.csv", row.names = FALSE)

 # humidity
names(wild.humi.all)

daily_hum_min <- dplyr::summarise(group_by(wild.humi.all, Date, alt_height_slope, alt_type, side_andes, height, alt.type_height),
                           value=min(value),
                           sd=min(sd(value)),
                           type=paste("hum.min"))

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

############### 1. Tmax temperature lme ###############
# use tmax from fig 1 (averaged across dataloggers per hour, then daily max/mean/min)
names(daily_temp_max)

# scale date
daily_temp_max$Date.scaled <- scale(daily_temp_max$Date)

# datalogger nested within point within area, responses vary across loggers and across points/areas within areas.
mixed.lmer2 <- lmerTest::lmer(value ~ height  + side_andes+ alt_type+Date.scaled + (1|side.alt/point/datalogger_id), 
                              data = daily_temp_max, REML = FALSE) ;summary(mixed.lmer2)  ; rsquared(mixed.lmer2)
plot(mixed.lmer2)
qqnorm(resid(mixed.lmer2)); qqline(resid(mixed.lmer2))
MuMIn::r.squaredGLMM(mixed.lmer2) # 57%, 65%

# TABLE join tests the interaction contrasts for all effects in the model and compiles them in one Type-III-ANOVA-like table:
joint_tests(mixed.lmer2, by = "side_andes")

# TABLE superscripts to obtain tukey pairwise comparisons
tukey.comp <- emmeans(mixed.lmer2, list(pairwise~alt_type + height+ side_andes), adjust = "tukey")
tukey.comp.pair <- tukey.comp$`pairwise differences of alt.type_height, side_andes`
write.csv(tukey.comp.pair, "1.data/tukey.comp.tmax.csv", row.names = FALSE)



############### 2. Tmean temperature lme ###############
# scale date
daily_temp_mean$Date.scaled <- scale(daily_temp_mean$Date)

# datalogger nested within point within area, responses vary across loggers and across points/areas within areas.
mixed.lmer2 <- lmerTest::lmer(value ~ height  + side_andes+ alt_type+Date.scaled + (1|side.alt/point/datalogger_id), 
                              data = daily_temp_mean, REML = FALSE) ;summary(mixed.lmer2)  ; rsquared(mixed.lmer2)
plot(mixed.lmer2)
qqnorm(resid(mixed.lmer2)); qqline(resid(mixed.lmer2))
MuMIn::r.squaredGLMM(mixed.lmer2) # 84%, 87%

# TABLE join tests the interaction contrasts for all effects in the model and compiles them in one Type-III-ANOVA-like table:
joint_tests(mixed.lmer2, by = "side_andes")

# TABLE superscripts to obtain tukey pairwise comparisons
tukey.comp <- emmeans(mixed.lmer2, list(pairwise~alt_type + height+ side_andes), adjust = "tukey")

############### 3. Tmin temperature lme ###############
# use tmin from fig 1 (averaged across dataloggers per hour, then daily min/min/min)
# scale date
daily_temp_min$Date.scaled <- scale(daily_temp_min$Date)

# datalogger nested within point within area, responses vary across loggers and across points/areas within areas.
mixed.lmer2 <- lmerTest::lmer(value ~ height  + side_andes+ alt_type+Date.scaled + (1|side.alt/point/datalogger_id), 
                              data = daily_temp_min, REML = FALSE) ;summary(mixed.lmer2)  ; rsquared(mixed.lmer2)
plot(mixed.lmer2)
qqnorm(resid(mixed.lmer2)); qqline(resid(mixed.lmer2))
MuMIn::r.squaredGLMM(mixed.lmer2) # 84%, 87%

# TABLE join tests the interaction contrasts for all effects in the model and compiles them in one Type-III-ANOVA-like table:
joint_tests(mixed.lmer2, by = "side_andes")

# TABLE superscripts to obtain tukey pairwise comparisons
tukey.comp <- emmins(mixed.lmer2, list(pairwise~alt_type + height+ side_andes), adjust = "tukey")




