#### METADATA ####

# Project:            RIAVALUE
# Study:              Teabags
# Objective:          Estimate water depth for the Caulerpa prolifera meadow sampling site
# Author:             Carmen B. de los Santos
# Creation:           Faro, 09 January 2020

#### SETTINGS ####

# libraries
library(readxl)     # to read xlsx
library(ggplot2)    # for graphing
library(grid)       # idem
library(gridExtra)  # idem
library(scales)     # idem
library(lubridate)  # for handing times and dates
library(tidyr)      # shape data
library(plyr)       # shape data
library(mgcv)       # for GAM models (generalized additive models)
library(splines)    # for spline regression
library(caret)      # for easy machine learning workflow

# clean
rm(list=ls())
dev.off()

# theme
default <- theme(plot.background=element_blank()) +
  theme(panel.background=element_rect(fill="white",colour="black")) +
  theme(strip.background=element_blank()) +
  theme(strip.text=element_text(size=9,colour="black",angle=0)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.text.x=element_text(colour="black",size=10,angle=0)) +
  theme(axis.text.y=element_text(colour="black",size=10,angle=0)) +
  theme(axis.title.x=element_text(size=10,vjust=0.2)) +
  theme(axis.title.y=element_text(size=10)) +
  theme(plot.margin= unit(c(0.5,0.5,0.5,0.5),"lines")) +
  theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) +
  theme(legend.key.height=unit(0.8,"line")) + 
  theme(legend.text.align=0) +
  theme(plot.title=element_text(size=16,face="bold"))

# set working directory
setwd("~/OneDrive - Universidade do Algarve/Trabajo/STUDIES/wip/teabags/wordir/data/datasets/climate/depth/")

#### MODEL DEFINITION ####
# used train data (logger data from 2017-09-07) to obtain a relationship between observed water depth and tidal height
#### ------------------------------ LOGGER DATA (data.log) ---------------------------------- ####
#### LOGGER 2017-07-25 ####

# logger: Rugged TROLL 100

# initial and final date and time
tini <- as.POSIXct("2017-07-25 12:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2017-07-25 18:50:00",tz="Europe/Lisbon")

# correction height sensor: 18 cm above the bottom -> in meters
hsensor <- 18/100

# load raw file
data <- read_excel("raw/loggers/20170725/20170725_caulerpa.xlsx",col_names=F)

# there are 71 info rows in the raw data, and the following columns:
# column #1: Date and Time (GMT Daylight time, local time)
# column #2: Elapsed time (in seconds)
# column #3: Pressure (bar)
# column #4: Temperature (degree celsius)
# column #5: Depth (m) - not correct

# select valid raws and colums
data <- data[-c(1:71),c(1:4)]

# rename columns
names(data) <- c("datetime_local","elapsed","pressure","temperature")

# data structure
str(data)
data$datetime_local  <- dmy_hm(data$datetime_local,tz="Europe/Lisbon")
data$temperature     <- as.numeric(as.character(data$temperature))
data$pressure        <- as.numeric(as.character(data$pressure))
data$elapsed         <- as.numeric(as.character(data$elapsed))
str(data)
tz(data$datetime_local)

# select time interval
data <- data[data$datetime_local>=tini & data$datetime_local<=tfin,]

# create date time in UTC
data$datetime_utc  <- with_tz(data$datetime_local,tz="UTC")
tz(data$datetime_utc)

# select final columns
data <- data[,c("datetime_local","datetime_utc","temperature","pressure")]

# calculation of water depth from pressure
# see protocol "Determination of water depth with pressure transducer Rugged TROLL 100"
data$depth <- (data$pressure - 1.01325)/0.1004544 + hsensor

# plot depth
pd <- ggplot(data,aes(x=datetime_local,y=depth)) +
  geom_line(colour="blue") + geom_point(colour="blue") +
  scale_x_datetime("Local time",breaks=date_breaks(width="1 hour")) +
  scale_y_continuous("Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pd

# plot temperature
pt <- ggplot(data,aes(x=datetime_local,y=temperature)) +
  geom_line(colour="red") + geom_point(colour="red") +
  scale_x_datetime("Local time",breaks=date_breaks(width="1 hours")) +
  scale_y_continuous("Temperature (C)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pt

# print
pdf("./processed/data_logger_caulerpa_20170725.pdf",onefile=TRUE,paper="a4")
grid.arrange(pd,pt,ncol=1,nrow=2)
dev.off()

# save data
write.csv(data,"processed/data_logger_caulerpa_20170725.csv")

# rename
data1 <- data
data1$dataset <- 1

# clean
rm(tfin,tini,data,hsensor,pd,pt)

#### LOGGER 2017-09-07 ####

# logger: Rugged TROLL 100

# initial and final date and time
tini <- as.POSIXct("2017-09-07 15:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2017-09-11 11:00:00",tz="Europe/Lisbon")

# correction height sensor: 12.5 cm above the bottom -> in meters
hsensor <- 12.5/100

# load raw file
data <- read_excel("raw/loggers/20170907/20170907_caulerpa.xlsx",col_names=F)

# there are 70 info rows in the raw data, and the following columns:
# column #1: Date and Time (GMT Daylight time, local time)
# column #2: Elapsed time (in seconds)
# column #3: Pressure (bar)
# column #4: Temperature (degree celsius)
# column #5: Depth (m) - not correct

# select valid raws and colums
data <- data[-c(1:70),c(1:4)]

# rename columns
names(data) <- c("datetime_local","elapsed","pressure","temperature")

# data structure
str(data)
data$datetime_local  <- dmy_hms(data$datetime_local,tz="Europe/Lisbon")
data$temperature     <- as.numeric(as.character(data$temperature))
data$pressure        <- as.numeric(as.character(data$pressure))
data$elapsed         <- as.numeric(as.character(data$elapsed))
str(data)
tz(data$datetime_local)

# select time interval
data <- data[data$datetime_local>=tini & data$datetime_local<=tfin,]

# create date time in UTC
data$datetime_utc  <- with_tz(data$datetime_local,tz="UTC")
tz(data$datetime_utc)

# select final columns
data <- data[,c("datetime_local","datetime_utc","temperature","pressure")]

# calculation of water depth from pressure
# see protocol "Determination of water depth with pressure transducer Rugged TROLL 100"
data$depth <- (data$pressure - 1.01325)/0.1004544 + hsensor

# plot depth
pd <- ggplot(data,aes(x=datetime_local,y=depth)) +
  geom_line(colour="blue") + geom_point(colour="blue") +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hour")) +
  scale_y_continuous("Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pd

# plot temperature
pt <- ggplot(data,aes(x=datetime_local,y=temperature)) +
  geom_line(colour="red") + geom_point(colour="red") +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Temperature (C)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pt

# print
pdf("./processed/data_logger_caulerpa_20170907.pdf",onefile=TRUE,paper="a4")
grid.arrange(pd,pt,ncol=1,nrow=2)
dev.off()

# save data
write.csv(data,"processed/data_logger_caulerpa_20170907.csv")

# rename
data2 <- data
data2$dataset <- 2

# clean
rm(tfin,tini,data,hsensor,pd,pt)

#### LOGGER 2019-06-17 ####

# logger: HOBO S/N: 10768690

# initial and final date and time
tini <- as.POSIXct("2019-06-17 13:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2019-06-17 14:10:00",tz="Europe/Lisbon")

# correction height sensor: XX cm above the bottom -> in meters
hsensor <- 12.5/100 # this is not correct

# load raw file
data <- read_excel("raw/loggers/20190617/20190617_caulerpa.xlsx",col_names=T)

# there are the following columns:
# column #1: Count
# column #2: Date and time (GMT+01:00, local time)
# column #3: Absolute pressure (kPa)
# column #4: Temperature (degree celsius)

# select valid raws and colums
data <- data[-c(1),c(2:4)]

# rename columns
names(data) <- c("datetime_local","pressure","temperature")

# data structure
str(data)
data$datetime_local <- mdy_hms(data$datetime_local,tz="Europe/Lisbon")
data$temperature    <- as.numeric(as.character(data$temperature))
data$pressure       <- as.numeric(as.character(data$pressure))
str(data)

# convert pressure from kPa to bar (1 bar = 100 kPa)
data$pressure <- data$pressure/100

# select time interval
data <- data[data$datetime_local>=tini & data$datetime_local<=tfin,]

# create date time in UTC
data$datetime_utc  <- with_tz(data$datetime_local,tz="UTC")
tz(data$datetime_utc)

# select final columns
data <- data[,c("datetime_local","datetime_utc","temperature","pressure")]

# calculation of water depth from pressure
# see protocol "Determination of water depth with pressure transducer Rugged TROLL 100"
data$depth <- (data$pressure - 1.01325)/0.1004544 + hsensor

# plot depth
pd <- ggplot(data,aes(x=datetime_local,y=depth)) +
  geom_line(colour="blue") + geom_point(colour="blue") +
  scale_x_datetime("Local time",breaks=date_breaks(width="10 mins")) +
  scale_y_continuous("Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pd

# plot temperature
pt <- ggplot(data,aes(x=datetime_local,y=temperature)) +
  geom_line(colour="red") + geom_point(colour="red") +
  scale_x_datetime("Loca time",breaks=date_breaks(width="10 mins")) +
  scale_y_continuous("Temperature (C)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pt

# print
pdf("./processed/data_logger_caulerpa_20190617.pdf",onefile=TRUE,paper="a4")
grid.arrange(pd,pt,ncol=1,nrow=2)
dev.off()

# save data
write.csv(data,"processed/data_logger_caulerpa_20190617.csv")

# rename
data3 <- data
data3$dataset <- 3

# clean
rm(tfin,tini,data,hsensor,pd,pt)

#### MERGE LOGGER DATASETS ####

# check columns
str(data1)
str(data2)
str(data3)

# merge datasets
data.log <- rbind(data1,data2,data3)

# clean
rm(data1,data2,data3)
dev.off()
#### ------------------------------ TIDE DATA (data.tid) ------------------------------------ ####
#### DATA CHARTS ####

# load data from official charts (obtained by Marcio)
data.tid <- read.csv("raw/tides/tides.csv")

# rename columns
names(data.tid) <- c("datetime_local","datetime_utc","height","event")

# correction tides (Dado que o plano do Zero Hidrografico (ZH) foi fixado em relacao a niveis medios adotados
# ha varias decadas, existe presentemente uma diferenca sistematica de cerca de +10 cm entre as alturas de agua
# observadas e as alturas de mare previstas. Para mais informacoes consultar www.hidrografico.pt
data.tid$height <- data.tid$height + 0.1

# structure
str(data.tid)
data.tid$datetime_local  <- ymd_hms(data.tid$datetime_local,tz="Europe/Lisbon")
data.tid$datetime_utc    <- ymd_hms(data.tid$datetime_utc,tz="UTC")
str(data.tid)
tz(data.tid$datetime_local)
tz(data.tid$datetime_utc)

# plot
pt <- ggplot(data.tid,aes(x=datetime_local,y=height)) +
  geom_line(size=0.1,colour="lightblue") +
  geom_point(aes(colour=event),size=0.3) + 
  scale_x_datetime("Local time",breaks=date_breaks(width="30 days")) +
  scale_y_continuous("Tide height (m, ZH)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pt

# print
pdf("./processed/data_tides_all.pdf",onefile=TRUE,paper="a4")
grid.arrange(pt,ncol=1,nrow=2)
dev.off()

# clean
rm(pt)

#### ------------------------------ TRAIN DATA PREPARATION (data.tra) ----------------------- ####
#### DESCRIPTION ####

# time interval with logger data
# as.POSIXct("2017-09-07 15:00:00",tz="Europe/Lisbon")
# as.POSIXct("2017-09-11 11:00:00",tz="Europe/Lisbon")

# initial and final date and time (including tide heights before/after logger data)
tini <- as.POSIXct("2017-09-07 09:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2017-09-11 15:00:00",tz="Europe/Lisbon")

#### VIEW DATA TIDE vs DATA LOGGER ####

# select water depth from logger data (data1)
data1 <- data.log[data.log$dataset==2,]
str(data1)
data1$value <- data1$depth # rename to merge later
data1  <- data1[,c("datetime_local","datetime_utc","value")]
data1$event  <- "undefined" 
data1$source <- "logger"

# select tide height from tide charts for the time interval with logger data (data2)
data2  <- data.tid[data.tid$datetime_local>=tini & data.tid$datetime_local<=tfin,]
str(data2)
data2$value <-  data2$height  # rename to merge later
data2  <- data2[,c("datetime_local","datetime_utc","value","event")]
data2$source <- "charts"

# merge both datasets
str(data1)
str(data2)
data.tra <- rbind(data1,data2)
str(data.tra)

# plot both datasets
ggplot(data.tra,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Tide height (charts, m, ZH) or  Water depth (logger, m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))

# clean
rm(tini,tfin,data1,data2)

#### INTERPOLATE TIDE HEIGHT FOR THE TIME PERIOD ####

# select data with tide heights
data.tra.hei <- data.tra[data.tra$source=="charts",c("datetime_local","value","event")]

# rename
data.tra.hei$height <- data.tra.hei$value
data.tra.hei$depth  <- NULL
data.tra.hei$event  <- as.character(data.tra.hei$event)

# calculate minutes from first datetime with logger data
data.tra.hei$timemin <- as.numeric(data.tra.hei$datetime_local)/60
data.tra.hei$timemin <- data.tra.hei$timemin-as.numeric(as.POSIXct("2017-09-07 15:00:00",tz="Europe/Lisbon"))/60

# create intervals (time interval with logger data)
data.tra.int <- data.frame(datetime_local=seq(ISOdatetime(2017,9,7,15,0,0,tz="Europe/Lisbon"),
                                    ISOdatetime(2017,9,11,11,0,0,tz="Europe/Lisbon"),
                                    by=60*1)) # time interval in seconds

# calculate minutes from first date
data.tra.int$timemin <- as.numeric(data.tra.int$datetime_local)/60
data.tra.int$timemin <- data.tra.int$timemin - as.numeric(as.POSIXct("2017-09-07 15:00:00",tz="Europe/Lisbon"))/60

# add data from charts
data.tra.int <- merge(data.tra.int,data.tra.hei[,c("datetime_local","height","event")],all.x=T)

# for each time event (t), i.e. row in data.int, identify previous/posterior tide event (time, height)

DATA.TRA.INT <- data.frame(
  "datetime_local"   = POSIXct(length = nrow(data.tra.int)),
  "timemin"          = numeric(length = nrow(data.tra.int)),
  "prev_event"       = character(length = nrow(data.tra.int)),
  "prev_time"        = numeric(length = nrow(data.tra.int)),
  "prev_height"      = numeric(length = nrow(data.tra.int)) ,
  "post_event"       = character(length = nrow(data.tra.int)),
  "post_time"        = numeric(length = nrow(data.tra.int)),
  "post_height"      = numeric(length = nrow(data.tra.int))
)
DATA.TRA.INT$prev_event <- factor(DATA.TRA.INT$prev_event,levels=c("low-tide","high-tide"))
DATA.TRA.INT$post_event <- factor(DATA.TRA.INT$post_event,levels=c("low-tide","high-tide"))

for (i in 1:nrow(data.tra.int)){
  
  # select time event
  data <- data.tra.int[i,]
  
  # get info previous event
  prev_event <- data.tra.hei[data.tra.hei$timemin <= data$timemin,]
  prev_event <- prev_event[which.max(prev_event$timemin),]
  
  # get info posterior event
  post_event <- data.tra.hei[data.tra.hei$timemin >= data$timemin,]
  post_event <- post_event[which.min(post_event$timemin),]
  
  # add info to data
  DATA.TRA.INT[i, ] <- data.frame(data$datetime_local,
                              data$timemin,
                              prev_event$event,
                              prev_event$timemin,
                              prev_event$height,
                              post_event$event,
                              post_event$timemin,
                              post_event$height)
  
}

# replace
data.tra.int <- DATA.TRA.INT

# clean
rm(i,data,DATA.TRA.INT,post_event,prev_event)

# calculate parameters for each point
data.tra.int$par_T <- data.tra.int$post_time-data.tra.int$prev_time   # time (min) between closest event
data.tra.int$par_t <- data.tra.int$timemin - data.tra.int$prev_time   # time (min) from previous event

# calculate estimated height using analitical formula
data.tra.int$height <- with(data.tra.int,ifelse(prev_event==post_event,prev_height,
                                        (prev_height+post_height)/2+(prev_height-post_height)/2*cos((pi*par_t)/par_T)))

# select columns
data.tra.int <- data.tra.int[,c("datetime_local","timemin","height")]

# check time
tz(data.tra.int$datetime)
data.tra.int <- with_tz(data.tra.int,tz="Europe/Lisbon")
tz(data.tra.int$datetime)

#### VIEW DATA INTERPOLATED TIDE vs DATA LOGGER ####

# check data.tra
str(data.tra)
data.tra <- data.tra[,c("datetime_local","source","event","value")]

# add data interpolations
str(data.tra.int)
data.tra.int$value <-  data.tra.int$height # rename to merge later
data.tra.int  <- data.tra.int[,c("datetime_local","value")]
data.tra.int$event  <- "undefined"
data.tra.int$source <- "interpolated"

# merge both datasets
str(data.tra)
str(data.tra.int)
data.tra <- rbind(data.tra,data.tra.int)
str(data.tra)

# plot both datasets
p <- ggplot(data.tra,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Tide height (m, ZH) or Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
p

# print
pdf("./processed/plot_model_training_data.pdf",onefile=TRUE,paper="a4")
grid.arrange(p,ncol=1,nrow=2)
dev.off()

# clean
rm(p,data.tra.hei,data.tra.int)

#### ------------------------------ VALIDATION DATA PREPARATION (data.val) ------------------ ####
#### DESCRIPTION ####

# time interval with logger data
# as.POSIXct("2017-07-25 12:00:00",tz="Europe/Lisbon")
# as.POSIXct("2017-07-25 18:50:00",tz="Europe/Lisbon")

# initial and final date and time (including tide heights before/after logger data)
tini <- as.POSIXct("2017-07-25 10:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2017-07-26 00:00:00",tz="Europe/Lisbon")

#### VIEW DATA TIDE vs DATA LOGGER ####

# select water depth from logger data (data1)
data1 <- data.log[data.log$dataset==1,]
str(data1)
data1$value <- data1$depth # rename to merge later
data1  <- data1[,c("datetime_local","datetime_utc","value")]
data1$event  <- "undefined" 
data1$source <- "logger"

# select tide height from tide charts for the time interval with logger data (data2)
data2  <- data.tid[data.tid$datetime_local>=tini & data.tid$datetime_local<=tfin,]
str(data2)
data2$value <-  data2$height  # rename to merge later
data2  <- data2[,c("datetime_local","datetime_utc","value","event")]
data2$source <- "charts"

# merge both datasets
str(data1)
str(data2)
data.val <- rbind(data1,data2)
str(data.val)

# plot both datasets
ggplot(data.val,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Tide height (charts, m, ZH) or  Water depth (logger, m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))

# clean
rm(tini,tfin,data1,data2)

#### INTERPOLATE TIDE HEIGHT FOR THE TIME PERIOD ####

# select data with tide heights
data.val.hei <- data.val[data.val$source=="charts",c("datetime_local","value","event")]

# rename
data.val.hei$height <- data.val.hei$value
data.val.hei$depth  <- NULL
data.val.hei$event  <- as.character(data.val.hei$event)

# calculate minutes from first datetime with logger data
data.val.hei$timemin <- as.numeric(data.val.hei$datetime_local)/60
data.val.hei$timemin <- data.val.hei$timemin-as.numeric(as.POSIXct("2017-07-25 12:00:00",tz="Europe/Lisbon"))/60

# create intervals (time interval with logger data)
data.val.int <- data.frame(datetime_local=seq(ISOdatetime(2017,7,25,12,00,00,tz="Europe/Lisbon"),
                                              ISOdatetime(2017,7,25,18,50,00,tz="Europe/Lisbon"),
                                              by=60*1)) # time interval in seconds

# calculate minutes from first date
data.val.int$timemin <- as.numeric(data.val.int$datetime_local)/60
data.val.int$timemin <- data.val.int$timemin - as.numeric(as.POSIXct("2017-07-25 12:00:00",tz="Europe/Lisbon"))/60

# add data from charts
data.val.int <- merge(data.val.int,data.val.hei[,c("datetime_local","height","event")],all.x=T)

# for each time event (t), i.e. row in data.int, identify previous/posterior tide event (time, height)

DATA.VAL.INT <- data.frame(
  "datetime_local"   = POSIXct(length = nrow(data.val.int)),
  "timemin"          = numeric(length = nrow(data.val.int)),
  "prev_event"       = character(length = nrow(data.val.int)),
  "prev_time"        = numeric(length = nrow(data.val.int)),
  "prev_height"      = numeric(length = nrow(data.val.int)) ,
  "post_event"       = character(length = nrow(data.val.int)),
  "post_time"        = numeric(length = nrow(data.val.int)),
  "post_height"      = numeric(length = nrow(data.val.int))
)
DATA.VAL.INT$prev_event <- factor(DATA.VAL.INT$prev_event,levels=c("low-tide","high-tide"))
DATA.VAL.INT$post_event <- factor(DATA.VAL.INT$post_event,levels=c("low-tide","high-tide"))

for (i in 1:nrow(data.val.int)){
  
  # select time event
  data <- data.val.int[i,]
  
  # get info previous event
  prev_event <- data.val.hei[data.val.hei$timemin <= data$timemin,]
  prev_event <- prev_event[which.max(prev_event$timemin),]
  
  # get info posterior event
  post_event <- data.val.hei[data.val.hei$timemin >= data$timemin,]
  post_event <- post_event[which.min(post_event$timemin),]
  
  # add info to data
  DATA.VAL.INT[i, ] <- data.frame(data$datetime_local,
                                  data$timemin,
                                  prev_event$event,
                                  prev_event$timemin,
                                  prev_event$height,
                                  post_event$event,
                                  post_event$timemin,
                                  post_event$height)
  
}

# replace
data.val.int <- DATA.VAL.INT

# clean
rm(i,data,DATA.VAL.INT,post_event,prev_event)

# calculate parameters for each point
data.val.int$par_T <- data.val.int$post_time-data.val.int$prev_time   # time (min) between closest event
data.val.int$par_t <- data.val.int$timemin - data.val.int$prev_time   # time (min) from previous event

# calculate estimated height using analitical formula
data.val.int$height <- with(data.val.int,ifelse(prev_event==post_event,prev_height,
                                                (prev_height+post_height)/2+(prev_height-post_height)/2*cos((pi*par_t)/par_T)))

# select columns
data.val.int <- data.val.int[,c("datetime_local","timemin","height")]

# check time
tz(data.val.int$datetime)
data.val.int <- with_tz(data.val.int,tz="Europe/Lisbon")
tz(data.val.int$datetime)

#### VIEW DATA INTERPOLATED TIDE vs DATA LOGGER ####

# check data.tra
str(data.val)
data.val <- data.val[,c("datetime_local","source","event","value")]

# add data interpolations
str(data.val.int)
data.val.int$value <-  data.val.int$height # rename to merge later
data.val.int  <- data.val.int[,c("datetime_local","value")]
data.val.int$event  <- "undefined"
data.val.int$source <- "interpolated"

# merge both datasets
str(data.val)
str(data.val.int)
data.val <- rbind(data.val,data.val.int)
str(data.val)

# plot both datasets
p <- ggplot(data.val,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Tide height (m, ZH) or Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
p

# print
pdf("./processed/plot_model_validation_data.pdf",onefile=TRUE,paper="a4")
grid.arrange(p,ncol=1,nrow=2)
dev.off()

# clean
rm(p,data.val.hei,data.val.int)

#### ------------------------------ MODEL CREATION AND VALIDATION ------------------------------ ####
#### DATA ####

# training data
str(data.tra)
head(data.tra)
data.tra <- data.tra[data.tra$source!="charts",]
data.tra <- spread(data.tra,source,value)
data.tra <- data.tra[is.na(data.tra$logger)==F,]

ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) + default +
  annotate("text",x=2,y=5,label=paste0("n = ",nrow(data.tra)))

# validation data
str(data.val)
head(data.val)
data.val <- data.val[data.val$source!="charts",]
data.val <- spread(data.val,source,value)
data.val <- data.val[is.na(data.val$logger)==F,]

ggplot(data.val,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) + default +
  annotate("text",x=2,y=4,label=paste0("n = ",nrow(data.val)))

#### VISUALISATION ####

# calculate lag
data.tra$depth_lag <- data.tra$logger - data.tra$interpolated

# check distribution of depth lag values
pl1 <- ggplot(data.tra,aes(depth_lag)) +
  geom_histogram(bins=30,fill="white",colour="black") +
  geom_vline(xintercept=mean(data.tra$depth_lag),colour="blue",size=1.5) +
  geom_vline(xintercept=median(data.tra$depth_lag),colour="green",size=1.5) +
  annotate("text",x=1.8,y=50,colour="blue",label=paste0("mean = ",round(mean(data.tra$depth_lag),2)," m"),hjust=0) +
  annotate("text",x=1.8,y=45,colour="green",label=paste0("median = ",round(median(data.tra$depth_lag),2)," m"),hjust=0) +
  annotate("text",x=1.8,y=40,colour="black",label=paste0("min = ",round(min(data.tra$depth_lag),2)," m"),hjust=0) +
  annotate("text",x=1.8,y=35,colour="black",label=paste0("max = ",round(max(data.tra$depth_lag),2)," m"),hjust=0) +
  default
pl1

# check lag value pattern over time
pdata <- gather(data.tra,source,value,interpolated,logger,depth_lag)

pl2 <- ggplot(pdata,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="6 hours")) +
  scale_y_continuous("Tide height (m, ZH), water depth (m), depth lag (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pl2

# print
pdf("./processed/plot_model_training_data_lag.pdf",onefile=TRUE,paper="a4")
grid.arrange(pl1,pl2,ncol=1,nrow=2)
dev.off()

# clean
rm(pl1,pl2,pdata)

#### OPTION 1 (constant) ####

# build the model
constant <- median(data.tra$depth_lag)
model <- function(hei){
  dep = hei + constant
}

# make predictions
predictions <- model(data.val$interpolated)
data.val$mod1 <- predictions

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

# visualise the model
m1 <- ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) +
  default +
  labs(subtitle="mod1 - constant")
m1

# clean
rm(constant,predictions,model)
  
#### OPTION 2 (linear regression) ####

# build the model
model <- lm(logger~interpolated,data=data.tra)

# make predictions
predictions <- model %>% predict(data.val)
data.val$mod2 <- predictions

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

# visualize the model
m2 <- ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) +
  geom_smooth(method="lm") +
  default +
  labs(subtitle="mod2 - linear regression")
m2

# clean
rm(predictions,model)

#### OPTION 3 (polynomial regression) ####

# explore models
model <- lm(logger~poly(interpolated,6,raw=T),data=data.tra)
summary(model)

# from the summary output, it can be seen that polynomial terms are not significant,
# so, just create a second polynomial regression model.

# build the model
model <- lm(logger~I(interpolated^2),data=data.tra)

# make predictions
predictions <- model %>% predict(data.val)
data.val$mod3 <- predictions

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

# visualize the model
m3 <- ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) +
  geom_smooth(method="lm",formula=y~poly(x,2,raw=T)) +
  default +
  labs(subtitle="mod3 - polynomial regression")
m3

# clean
rm(model,predictions)

#### OPTION 4 (spline regression) ####

# build the model
knots <- quantile(data.tra$interpolated,p=c(0.25,0.5,0.75))
model <- lm (logger ~ bs(interpolated,knots=knots),data=data.tra)

# make predictions
predictions <- model %>% predict(data.val)
data.val$mod4 <- predictions

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

# visualize the model
m4 <- ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) +
  geom_smooth(method="lm",formula=y~splines::bs(x,df=3)) +
  default +
  labs(subtitle="mod4 - spline regression")
m4

# clean
rm(knots,model,predictions)

#### OPTION 5 (generalized additive models) ####

# build the model
model <- gam(logger~s(interpolated),data=data.tra)

# make predictions
predictions <- model %>% predict(data.val)
data.val$mod5 <- predictions

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

# visualize the model
m5 <- ggplot(data.tra,aes(x=interpolated,y=logger)) +
  geom_point(shape=21) +
  geom_smooth(method=gam,formula=y~s(x)) +
  default +
  labs(subtitle="mod5 - generalized additive model")
m5

# clean
rm(model,predictions)

#### MODEL SELECTION ####

# visualisation
pdata <- data.val[,c("datetime_local","logger",
                     "mod1","mod2","mod3","mod4","mod5")]
pdata <- gather(pdata,source,value,logger,mod1,mod2,mod3,mod4,mod5)
head(pdata)

mc <- ggplot(pdata,aes(x=datetime_local,y=value,colour=source)) +
  geom_point(size=0.3) + geom_line() +
  scale_x_datetime("Local time",breaks=date_breaks(width="1 hours")) +
  scale_y_continuous("Water depth (m)") +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
mc

# print
pdf("./processed/plot_model_comparison_a.pdf",onefile=TRUE,paper="a4")
grid.arrange(m1,m2,m3,m4,m5,ncol=2,nrow=3)
dev.off()

pdf("./processed/plot_model_comparison_b.pdf",onefile=TRUE,paper="a4")
grid.arrange(mc,ncol=1,nrow=2)
dev.off()

# clean
rm(m1,m2,m3,m4,m5,mc,pdata)

# linear model outperformed the other models
model <- lm(logger~interpolated,data=data.tra)

# make predictions
predictions <- model %>% predict(data.val)

# model performance
RMSE(predictions, data.val$logger)
R2(predictions, data.val$logger)

#### ------------------------------ MODEL APPLICATION ------------------------------ ####
#### MODEL APPLICATION ####
# initial and final datetime to estimate water depths
tini <- as.POSIXct("2017-06-01 00:00:00",tz="Europe/Lisbon")
tfin <- as.POSIXct("2019-07-01 00:00:00",tz="Europe/Lisbon")

# select tide height from tide charts for the time interval
data.est  <- data.tid[data.tid$datetime_local>=tini & data.tid$datetime_local<=tfin,]
data.est$interpolated <- data.est$height # to apply the model
data.est  <- data.est[,c("datetime_local","interpolated")]

# calculate the predicted depth
data.est$prediction <- model %>% predict(data.est)

# create year-month column
data.est$year  <- year(data.est$datetime_local)
data.est$month <- month(data.est$datetime_local)
data.est$year_month <- as.POSIXct(ym(paste0(data.est$year,data.est$month))) 

# obtain monthy values (mean,min,max)
table.est <- ddply(data.est,.(year_month,year,month),summarise,
                   mean_depth = mean(prediction),
                   max_depth  = max(prediction),
                   min_depth  = min(prediction))

# save table
write.csv(table.est,"processed/table_model_application.csv")

# visualise results
table.est <- gather(table.est,parameter,value,mean_depth,max_depth,min_depth)
pe <- ggplot(data.est,aes(x=datetime_local,y=prediction)) +
  geom_line(size=0.1,colour="lightblue") +
  geom_line(data=table.est,aes(x=year_month,y=value,linetype=parameter),colour="darkblue") +
  scale_x_datetime("Month",breaks=date_breaks(width="30 days"),date_labels="%Y-%m") +
  scale_y_continuous("Estimated water depth (m)") +
  scale_linetype_manual("",values=c(2,1,3)) +
  default +
  theme(axis.text.x=element_text(colour="black",size=10,angle=90,vjust=0.5))
pe

# print
pdf("./processed/plot_model_application.pdf",onefile=TRUE,paper="a4")
grid.arrange(pe,ncol=1,nrow=2)
dev.off()

# clean
rm(table.est,pe,model,tini,tfin)
dev.off()

#### END ####