#PREPARAZIONE DEI DATI DI OZONO DELL'ANALIZZATORE DELLO ZEPPELIN
#I dati validati formato EBAS vengono riformattati per il confronto con i dati della stazione low-cost
#Obiettivo: validazione automatica dei record di Ozono Low-Cost 
#Autore: Federico Dallo
#Data di creazione: 12 ottobre 2018
#Data di modifica: 

#IMPORTARE LE LIBRERIE
library("zoo")
library("openair")
library("ggplot2")
library("lubridate")
library("scales")
library("gridExtra")
library("ggthemes")
require( lubridate )
require( xts )

#clear workspace
rm(list = ls())

#load file EBAS OZONO
#########################################################################################################################################################################
today<-format(Sys.time(), "%Y_%m_%d")################################################################                     QUESTE TRE RIGHE SONO DA MODIFICARE           #
user<- Sys.getenv("LOGNAME")#########################################################################                       A SECONDA DEL SISTEMA OPERATIVO             #                     
setwd(file.path("/home/",user,"/OZO/git_proj"))######################################################                             E DEL PATH DEI DATI                   #
#########################################################################################################################################################################

EBAS_OZO_ZEPP <- read.csv("Ebas_181012_2340/NO0042G.20180725230000.20181012211209.uv_abs.ozone.air.3mo.1h.NO01L_uv_abs_uk_0042_NRT.NO01L_uv_abs.lev1.5.nas", header = TRUE, skip = 56, sep="")
submitted <- EBAS_OZO_ZEPP

#riprendere una data leggibile
submitted$jd<-as.integer(submitted$starttime)
submitted$day<-as.Date(submitted$starttime, origin=as.Date("2018-01-01 00:00:00"))
submitted$time.dec<-submitted$starttime-submitted$jd
submitted$time<-submitted$time.dec*1440+0.01
submitted$hour<-as.integer(submitted$time/60)
submitted$min<-as.integer(submitted$time-submitted$hour*60)
submitted$date <- paste(submitted$day," ",submitted$hour,":",submitted$min,":00",sep="")
submitted$date=as.POSIXct(strptime(submitted$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
head(submitted)

#####################################################################################################

plot(submitted$date, submitted$O3.1, ylim=c(0,120))

#####################################################################################################

write.csv(x = submitted, file = "last_ebas_ZEPP_readableData.csv", row.names = FALSE)
