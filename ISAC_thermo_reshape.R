#PREPARAZIONE DEI DATI DI OZONO DELL'ANALIZZATORE THERMO
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

EBAS_OZO_MRG <- read.csv("last_ebas_MRG.txt", header = TRUE, skip = 87, sep="")
submitted <- EBAS_OZO_MRG

#riprendere una data leggibile
submitted$jd<-as.integer(submitted$start_time)
submitted$day<-as.Date(submitted$start_time, origin=as.Date("2018-01-01 00:00:00"))
submitted$time.dec<-submitted$start_time-submitted$jd
submitted$time<-submitted$time.dec*1440+0.01
submitted$hour<-as.integer(submitted$time/60)
submitted$min<-as.integer(submitted$time-submitted$hour*60)
submitted$date <- paste(submitted$day," ",submitted$hour,":",submitted$min,":00",sep="")
submitted$date=as.POSIXct(strptime(submitted$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
head(submitted)

#####################################################################################################

plot(submitted$date, submitted$o3, ylim=c(0,120))

#####################################################################################################

write.csv(x = submitted, file = "last_ebas_MRG_readableData.csv", row.names = FALSE)
