
#loads librarys

library("RODBC")
library("ggplot2")
library(gridExtra)
library('dplyr')
library('lubridate')
library(base64)
library(grid)
library(data.table)


# Establish connection with Sas server

#con<-odbcConnect ("SAS Share PSASLIV",believeNRows=FALSE)
con2<-odbcConnect("SAS Share PSASSKADE",believeNRows=FALSE)
#con3<-odbcConnect("SAS Share TSASSKADE",believeNRows=FALSE)
#con4<-odbcConnect("SAS Share PSASLIV",believeNRows=FALSE)
#con5<-odbcConnect("SAS Share PSASLIV",believeNRows=FALSE)
#con6<-odbcConnect("SAS Share PSASLIV",believeNRows=FALSE)
close(con2)


#Table exploring

#TablesPsas <- sqlTables(con2) #Loads tables pressent on Server
#Colldag <- sqlColumns(con2, "EMD_FORSIKRING") #Loads collumns in given table
#Coll <- sqlColumns(con2, "MA_WORK_AVTALE") #Loads collumns in given table
#View(Colldag)




query_premier <- paste("SELECT * FROM DV_M2_DI.EMD_FORSIKRING") #set SQL querry
data_premier <- sqlQuery(con2,query_premier, stringsAsFactors=FALSE ) #run SQL querry
data_premier <- subset(data_premier, FoerstOpprettDato>as.Date("2016-01-01") & ProduktKode == "PR" & DV_ForsikringStatus == "A") #subset based on created date.
                       max(data_premier$FoerstOpprettDato) # lookup max date
                       min(data_premier$FoerstOpprettDato) #lookup min date
                       
                       data_premier$Month=month(data_premier$FoerstOpprettDato) #add month collumn
                       data_premier$Year=year(data_premier$FoerstOpprettDato)
                       data_premier$Week <- week(as.Date(cut(data_premier$FoerstOpprettDato,
                                                           breaks = "week",
                                                           start.on.monday = TRUE))) #add weeknumber Collumn

Premier <- data.table(data_premier) #make dataframe in to data table                      
Premier[Uke==1652 & FoerstOpprettDato<as.Date("2016-02-02"), Uke := 1553] #recode missplaced week 52 for 2016
Premier[Uke==1752, Uke := 1652] #recode missplaced week 52 for 2017

Premier[,Avtaler:=.N, by=Uke] #Orders pr week
Premier[,UkesRev:=sum(Aarspremie), by=Uke] #Revenue pr week

Skade <- Premier[,.(Avtaler=mean(Avtaler), UkesRev=mean(UkesRev)), by=Uke]# new table by week
setorder(Skade, Uke) #sort by weeknumber
Skade[,Avtalesnitt:=round((UkesRev/Avtaler),2)]#rounded average revenue pr order 






                       
                       
                       
