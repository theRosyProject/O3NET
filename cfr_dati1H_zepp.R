#CONFRONTO E STATISTICA DEI DATI DI OZONO LOW-COST E REFERENCE ZEPPELIN
#Obiettivo: validazione automatica dei record di Ozono Low-Cost 
#Autore: Federico Dallo
#Data di creazione: 13 ottobre 2018
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

#########################################################################################################################################################################
today<-format(Sys.time(), "%Y_%m_%d")################################################################                     QUESTE TRE RIGHE SONO DA MODIFICARE           #
user<- Sys.getenv("LOGNAME")#########################################################################                       A SECONDA DEL SISTEMA OPERATIVO             #                     
setwd(file.path("/home/",user,"/OZO/recombined/"))##########################################################                             E DEL PATH DEI DATI                   #
#########################################################################################################################################################################

#CARICAMENTO DEI FILE

#DATI RIFERIMENTO DI OZONO
zepp_ref_instr <- read.csv(file = "last_ebas_ZEPP_readableData.csv", sep=",")
df_zepp_ref <- data.frame(TIME=zepp_ref_instr$date,
                            O3=zepp_ref_instr$O3.1
                            )
is.na(df_zepp_ref)<-sapply(df_zepp_ref, is.infinite)
df_zepp_ref<-(na.omit(df_zepp_ref))
df_zepp_ref$TIME <- as.POSIXct(strptime(df_zepp_ref$TIME, format = "%Y-%m-%d %H:%M:%S"))
df_zepp_ref <- df_zepp_ref[!(df_zepp_ref$O3 < 10), ]
df_zepp_ref <- df_zepp_ref[!(df_zepp_ref$O3 > 300), ]

#DATI LOWCOST
zepp_LCS <- read.csv(file = "ZEPPELIN_O3_recombined_01H.csv", sep=",")
############
df_zepp_LCS_1 <- data.frame(TIME=zepp_LCS$TIME,
                           WE1=zepp_LCS$WE1_mV, AUX1=zepp_LCS$AUX1_mV,
                           O3_1=zepp_LCS$O3_1)
df_zepp_LCS_1$TIME <- as.POSIXct(strptime(df_zepp_LCS_1$TIME, format = "%Y-%m-%d %H:%M:%S"))
df_zepp_LCS_1 <- df_zepp_LCS_1[!(df_zepp_LCS_1$O3_1 < 10), ]
df_zepp_LCS_1 <- df_zepp_LCS_1[!(df_zepp_LCS_1$O3_1 > 200), ]
df_zepp_LCS_1 <- subset(df_zepp_LCS_1, TIME > as.Date("2018-07-15 00:00:00"))
############
df_zepp_LCS_2 <- data.frame(TIME=zepp_LCS$TIME,
                           WE2=zepp_LCS$WE2_mV, AUX2=zepp_LCS$AUX2_mV,
                           O3_2=zepp_LCS$O3_2)
df_zepp_LCS_2$TIME <- as.POSIXct(strptime(df_zepp_LCS_2$TIME, format = "%Y-%m-%d %H:%M:%S"))
df_zepp_LCS_2 <- df_zepp_LCS_2[!(df_zepp_LCS_2$O3_2 < 10), ]
df_zepp_LCS_2 <- df_zepp_LCS_2[!(df_zepp_LCS_2$O3_2 > 200), ]
df_zepp_LCS_2 <- subset(df_zepp_LCS_2, TIME > as.Date("2018-07-15 00:00:00"))
############
df_zepp_LCS_3 <- data.frame(TIME=zepp_LCS$TIME, 
                           WE3=zepp_LCS$WE3_mV, AUX3=zepp_LCS$AUX3_mV,
                           O3_3=zepp_LCS$O3_3)
df_zepp_LCS_3$TIME <- as.POSIXct(strptime(df_zepp_LCS_3$TIME, format = "%Y-%m-%d %H:%M:%S"))
df_zepp_LCS_3 <- df_zepp_LCS_3[!(df_zepp_LCS_3$O3_3 < 10), ]
df_zepp_LCS_3 <- df_zepp_LCS_3[!(df_zepp_LCS_3$O3_3 > 200), ]
df_zepp_LCS_3 <- subset(df_zepp_LCS_3, TIME > as.Date("2018-07-15 00:00:00"))
############
#PLOT
smoothingSpline_df_zepp_ref <- smooth.spline(df_zepp_ref$TIME,df_zepp_ref$O3, spar=0.5)
smoothingSpline_df_LCS_1 <- smooth.spline(df_zepp_LCS_1$TIME,df_zepp_LCS_1$O3_1, spar=0.5)
smoothingSpline_df_LCS_2 <- smooth.spline(df_zepp_LCS_2$TIME,df_zepp_LCS_2$O3_2, spar=0.5)
smoothingSpline_df_LCS_3 <- smooth.spline(df_zepp_LCS_3$TIME,df_zepp_LCS_3$O3_3, spar=0.5)
plot(df_zepp_ref$TIME, df_zepp_ref$O3, ylim=c(0,100), xlim=as.POSIXct(c("2018-07-01 00:00:00","2018-10-10 00:00:00")))
lines(smoothingSpline_df_zepp_ref)
points(df_zepp_LCS_1$TIME, df_zepp_LCS_1$O3_1, col=2, cex=0.2)
lines(smoothingSpline_df_LCS_1, col=2)
points(df_zepp_LCS_2$TIME, df_zepp_LCS_2$O3_2, col=3, cex=0.2)
lines(smoothingSpline_df_LCS_2, col=3)
points(df_zepp_LCS_3$TIME, df_zepp_LCS_3$O3_3, col=4, cex=0.2)
lines(smoothingSpline_df_LCS_3, col=4)
