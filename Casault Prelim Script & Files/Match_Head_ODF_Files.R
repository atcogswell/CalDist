# This script imports multinet file metadata from Erica Head and attempts to match with locaiton and time


library(xlsx)
library(dplyr)
library(readxl)
xl <- readRDS("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist/ODF_Meta_1978_2011.rds")

setwd("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist/CTD_profiles/Head_AZMP")

cs<-read.xlsx2("ctdmdsum_head.xlsx",sheetIndex = 1) #import summary of Erica's files
names(cs)<-toupper(names(cs))
sc<-read.xlsx2("stationswithdepths.xlsx",sheetIndex=1) #import nominal station locations to fill in cs file
sc<-sc[,c(1:3)]
sc$DEC.LONG<-sc$DEC.LONG*-1

#copy nominal coordinates to pl_lat and p_long in joined file
tmp<-dplyr::left_join(cs,sc,by="STATION")
index1 <- which( is.na( tmp$PL_LAT ) )
tmp[index1, "PL_LAT"] <- tmp[index1, "DEC.LAT."] #
index2 <- which( is.na( tmp$PL_LON ) )
tmp[index2, "PL_LON"] <- tmp[index2, "DEC.LONG"]
tmp$PL_LAT<-round(tmp$PL_LAT,digits = 4)
tmp$PL_LON<-round(tmp$PL_LON,digits = 4)

cs2<-tmp[,c(11,8,9,10,1,2,3,4,5,6,12,13,14,15)]
write.xlsx2(cs2,"ctdmdsum_head_w_coord.xlsx")
