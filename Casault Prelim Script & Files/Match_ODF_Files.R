## Read in and compile CTD excel metadata from ODF files 1978 to 2008 (only run to update .rds file) ####
library(xlsx)
library(dplyr)
library(readxl)

yr<-c(1978:2008)

for (i in 1:length(yr)){
  
  setwd(paste("//dcnsbiona01a/BIODataSvcArc/Archive/ctd/",yr[i],sep=""))
  path<-getwd()
  xlist<-list.files(pattern="*^.*metadata.*.xls$")
  out=c(paste("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/ODF xl files/",yr[i],"/", sep=""))

for (n in 1:length(xlist)) {
  
  dir.create(file.path(out), showWarnings = FALSE)
  file.copy(from=xlist[n],to=out, overwrite=T,recursive=F)  
  
 }
}


##Compile xcel table from local copy and add benoit's data where mission = HUD2008037 - do not run copy if possible - errors in source data ####
xl<-NULL
for (i in 1:length(yr)){
  
  setwd(paste("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/ODF xl files/",yr[i],"/", sep=""))
  path<-getwd()
  xlist<-list.files(pattern="*^.*metadata.*.xls$")
  tmp2<-NULL
  
  for (n in 1:length(xlist)){
    sh<-getSheets(loadWorkbook(xlist[n]))
    tmp1<-read_excel(xlist[n],sheet = length(sh))
    tmp1<-as.data.frame(tmp1)
    if (is.double(tmp1$Cruise_Number==TRUE)) tmp1$Cruise_Number<-as.numeric(tmp1$Cruise_Number)
    if (is.numeric(tmp1$Cruise_Number==TRUE)) tmp1$Cruise_Number<-as.character(tmp1$Cruise_Number)
    #tmp1<-read.xlsx(xlist[n],sheetIndex = length(sh),stringsAsFactors=F)
    tmp1<-subset(tmp1,!is.na(tmp1$File_Spec))
    tmp1$File_Name<-toupper(tmp1$File_Name)
    #index1 <- with(tmp1, grepl("DN", tmp1$File_Name)) 
    #tmp1<-tmp1[index1,] #keeps files only with DN in file name
    tmp1<-tmp1[c(1,4,13,18,20,21)]
    #tmp1<-tmp1[!(tmp1$Start_Date_Time== "17-NOV-1858 00:00:00.00"),]
    #tmp1$Start_Date_Time<-as.POSIXct(tmp1$Start_Date_Time)
    tmp2<-rbind(tmp2,tmp1)
    
  }
 
 tmp2<-dplyr::arrange(tmp2,Cruise_Number) 
 xl<-rbind(xl,tmp2) 
  
}

setwd("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist") 
xl$Cruise_Number[xl$Cruise_Number=="79031.000000"]<-"79031"
xl<-subset(xl,xl$Initial_Latitude<=90 & xl$Initial_Latitude>=-90)
xl<-subset(xl,xl$Initial_Longitude<=180 & xl$Initial_Longitude>=-180)
saveRDS(xl, "ODF_Meta_1978_2008.rds")

## Read in compiled xls metadata from ODF archive ####
setwd("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist") 
xl <- readRDS("ODF_Meta_1978_2008.rds")

xl$YEAR<-strftime(xl$Start_Date_Time,format="%Y")
xl$TIME <- strftime(xl$Start_Date_Time, format="%H%M",tz="GMT")
xl$MONTH<-strftime(xl$Start_Date_Time,format="%m")
xl$DAY<-strftime(xl$Start_Date_Time,format="%d")
xl<-xl[c(7,9,10,8,5,6,1)]
names(xl)<-c("YEAR","MONTH","DAY","TIME","LATITUDE","LONGITUDE","ODF_FILENAME")

## Bring in Benoit's PTRAN extraction and subset HUD2008037 because it is not summarized in xcel files in SvcArc
xlold<-read.table("CTD_Metadata_20161220.tsv",header=TRUE, sep="\t", stringsAsFactors=FALSE, na.strings="", check.names=FALSE, comment.char="")
xlold$ODF_FILENAME<-paste(xlold$ODF_FILENAME,".ODF",sep="")
index2 <- with(xlold, grepl("2008037", xlold$ODF_FILENAME)) 
xlold<-xlold[index2,] #keeps files only with CTD_HUD2008037 in file name
xl<-rbind(xl,xlold)


library(leaflet)

leaflet(xl) %>%
  fitBounds(-100,12,-8,80) %>%
addTiles() %>%  # Add default OpenStreetMap map tiles
addCircles(lng=xl$LONGITUDE, lat=xl$LATITUDE, weight = 3, radius=40, color="red", stroke = TRUE, fillOpacity = 1) 
## Code for matching ODF files ####

## load required package
library(dplyr)
library(tidyr)
library(lubridate)

# source custom functions
source("~/AZMP/Projects/Vertical distribution CTD project/CalDist/Casault Prelim Script & Files/dtr.R")

## clean memory
gc()

##--------------------------------------------------------------------------------------------
# load ctd metadata
# load("~/AZMP/Projects/Vertical distribution CTD project/CalDist/Casault Prelim Script & Files/CTD_Metadata_20161220.RData")

df_ctd_id<-xl

# add new variables
df_ctd_id <- df_ctd_id %>%
	tidyr::unite(., t, YEAR, MONTH, DAY, sep="-", remove=FALSE) %>%
	dplyr::mutate(., yday=yday(ymd(t))) %>%
	dplyr::mutate(., t=decimal_date(ymd(t))) %>%
	dplyr::mutate(., YEAR=as.numeric(YEAR), MONTH=as.numeric(MONTH), DAY=as.numeric(DAY), TIME=as.numeric(TIME))

# add easting/northing
df_ctd_id <- cbind(df_ctd_id, data.frame(dtr(df_ctd_id$LONGITUDE, df_ctd_id$LATITUDE, -61.75, 44.5)))

# rename columns to lowercase
names(df_ctd_id) <- paste0("ctd_", tolower(names(df_ctd_id)))

# sort rows
df_ctd_id <- df_ctd_id %>%
	dplyr::arrange(., ctd_year, ctd_month, ctd_day, ctd_time) %>%
	dplyr::mutate(., ctd_rec=seq(1, nrow(.)))

##--------------------------------------------------------------------------------------------
# load plankton data
ifile <- paste("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist/CTD_profiles/SS_BIONESS/SS_Calspp_VertDistr.txt",sep="")
df_plankton_all <- read.table(ifile, header=TRUE, quote="", sep="\t", stringsAsFactors=FALSE, na.strings="", check.names=FALSE, comment.char="")

# kepp only variables of interest
df_plankton <- df_plankton_all %>%
	dplyr::select(., Year, Month, Day, HEURE, LONGITUDE, LATITUDE)

# reduce to unique rows
df_plankton <- df_plankton %>%
	dplyr::distinct(.)

# add new variables
df_plankton <- df_plankton %>%
	tidyr::unite(., t, Year, Month, Day, sep="-", remove=FALSE) %>%
	dplyr::mutate(., yday=yday(ymd(t))) %>%
	dplyr::mutate(., t=decimal_date(ymd(t)))

# add easting/northing
df_plankton <- cbind(df_plankton, data.frame(dtr(df_plankton$LONGITUDE, df_plankton$LATITUDE, -61.75, 44.5)))

# rename columns to lowercase
names(df_plankton) <- paste0("pl_", tolower(names(df_plankton)))

# sort rows
df_plankton <- df_plankton %>%
	dplyr::arrange(., pl_year, pl_month, pl_day, pl_heure) %>%
	dplyr::mutate(., pl_rec=seq(1, nrow(.)))

##--------------------------------------------------------------------------------------------
df_match <- data.frame()

for(i in seq(1, nrow(df_plankton), 1)) {
	
	# check if exact (close) match
	match <- df_ctd_id %>% dplyr::filter(., ctd_year==df_plankton$pl_year[i] &
																			 	ctd_month==df_plankton$pl_month[i] &
																			 	ctd_day==df_plankton$pl_day[i] &
																			 	ctd_latitude==df_plankton$pl_latitude[i] &
																			 	ctd_longitude==df_plankton$pl_longitude[i])
	if(nrow(match)>0) {
		df_match <- rbind(df_match,
											cbind(df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_rec, pl_year, pl_month, pl_day, pl_heure, pl_latitude, pl_longitude),
														match %>% select(., ctd_rec, ctd_year, ctd_month, ctd_day, ctd_time, ctd_latitude, ctd_longitude, ctd_odf_filename) %>% dplyr::mutate(., match_code=1)))
	} else {
		
		# match day with +/- 0.2 deg location
		match <- df_ctd_id %>% dplyr::filter(., ctd_year==df_plankton$pl_year[i] &
																				 	ctd_month==df_plankton$pl_month[i] &
																				 	ctd_day==df_plankton$pl_day[i] &
																				 	ctd_latitude>=df_plankton$pl_latitude[i]-.2 & ctd_latitude<=df_plankton$pl_latitude[i]+.2 &
																				 	ctd_longitude>=df_plankton$pl_longitude[i]-.2 & ctd_longitude<=df_plankton$pl_longitude[i]+.2)
		if(nrow(match)>0) {
			df_match <- rbind(df_match,
												cbind(df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_rec, pl_year, pl_month, pl_day, pl_heure, pl_latitude, pl_longitude),
															match %>% select(., ctd_rec, ctd_year, ctd_month, ctd_day, ctd_time, ctd_latitude, ctd_longitude, ctd_odf_filename) %>% dplyr::mutate(., match_code=2)))
		} else {
			
			# match day - 3 closest locations
			match <- df_ctd_id %>% dplyr::filter(., ctd_year==df_plankton$pl_year[i] &
																					 	ctd_month==df_plankton$pl_month[i] &
																					 	ctd_day==df_plankton$pl_day[i])
			if(nrow(match)>0) {
				
				tmp <- cbind(match %>% dplyr::select(., ctd_rec, ctd_x, ctd_y),
										 df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_x, pl_y))
				
				# calculate (pseudo-)distance
				tmp <- tmp %>% dplyr::mutate(., d=sqrt((ctd_x-pl_x)^2 + (ctd_y-pl_y)^2))
				
				# sort by distance
				tmp <- tmp %>% dplyr::arrange(., d)
				
				# keep 3 closest locations
				tmp <- head(tmp, 3)
				
				# append to df_match
				df_match <- rbind(df_match,
													cbind(df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_rec, pl_year, pl_month, pl_day, pl_heure, pl_latitude, pl_longitude),
																match %>% 
																	dplyr::filter(., ctd_rec %in% tmp$ctd_rec) %>%
																	dplyr::select(., ctd_rec, ctd_year, ctd_month, ctd_day, ctd_time, ctd_latitude, ctd_longitude, ctd_odf_filename) %>% dplyr::mutate(., match_code=3)))
			} else {
				
				# match month +/- 2 days and 3 closest locations
				match <- df_ctd_id %>% dplyr::filter(., ctd_year==df_plankton$pl_year[i] &
																						 	ctd_month==df_plankton$pl_month[i] &
																						 	ctd_yday>=df_plankton$pl_yday[i]-3 & ctd_yday<=df_plankton$pl_yday[i]+3)
				if(nrow(match)>0) {
					
					tmp <- cbind(match %>% dplyr::select(., ctd_rec, ctd_x, ctd_y),
											 df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_x, pl_y))
					
					# calculate (pseudo-)distance
					tmp <- tmp %>% dplyr::mutate(., d=sqrt((ctd_x-pl_x)^2 + (ctd_y-pl_y)^2))
					
					# sort by distance
					tmp <- tmp %>% dplyr::arrange(., d)
					
					# keep 3 closest locations
					tmp <- head(tmp, 3)
					
					# append to df_match
					df_match <- rbind(df_match,
														cbind(df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_rec, pl_year, pl_month, pl_day, pl_heure, pl_latitude, pl_longitude),
																	match %>% 
																		dplyr::filter(., ctd_rec %in% tmp$ctd_rec) %>%
																		dplyr::select(., ctd_rec, ctd_year, ctd_month, ctd_day, ctd_time, ctd_latitude, ctd_longitude, ctd_odf_filename) %>% dplyr::mutate(., match_code=4)))					
				} else {
					# append to df_match
					df_match <- rbind(df_match,
														cbind(df_plankton %>% dplyr::filter(., row_number()==i) %>% dplyr::select(., pl_rec, pl_year, pl_month, pl_day, pl_heure, pl_latitude, pl_longitude),
																	data.frame(ctd_rec=NA, ctd_year=NA, ctd_month=NA, ctd_day=NA, ctd_time=NA, ctd_latitude=NA, ctd_longitude=NA, ctd_odf_filename=NA, match_code=0)))					
				}
			}
		}
	}
} # for loop

##--------------------------------------------------------------------------------------------
# sort rows
df_match <- df_match %>% dplyr::arrange(., pl_year, pl_month, pl_day, pl_heure)

# output to csv file
## print to csv file - climatology

ofile <-paste("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist/CTD_Profiles/SS_BIONESS/Plankton_CTD_Matches_",(as.numeric(format(Sys.Date(), "%Y%m%d"))),".csv",sep="")
cat("Match_Code", file=ofile, sep="\n", append=FALSE)
cat("0: no match", file=ofile, sep="\n", append=TRUE)
cat("1: match year/month/day/lat/lon", file=ofile, sep="\n", append=TRUE)
cat("2: match year/month/day and lat/lon +/-0.2 deg", file=ofile, sep="\n", append=TRUE)
cat("3: match year/month/day; 3 closest locations", file=ofile, sep="\n", append=TRUE)
cat("4: match year/month and day +/-3d; 3 closest locations", file=ofile, sep="\n", append=TRUE)
cat("", file=ofile, sep="\n", append=TRUE)
write.table(df_match, file=ofile, append=TRUE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)

setwd("C:/Users/CogswellA/Documents/AZMP/Projects/Vertical distribution CTD project/CalDist/CTD_Profiles/SS_BIONESS")


x1<-read.csv("Plankton_CTD_Matches_20161222.csv",skip=7)
x2<-subset(x1,x1$ctd_odf_filename!="NA")
print(paste("The count of unique tow records matched is",length(unique(x2$pl_rec)),sep=" "))
y1<-read.csv("Plankton_CTD_Matches_20170123.csv",skip=7)
y2<-subset(y1,y1$ctd_odf_filename!="NA")
print(paste("The count of unique tow records matched is",length(unique(y2$pl_rec)),sep=" "))
y3<-dplyr::left_join(y1,x1,by="pl_rec")


##--------------------------------------------------------------------------------------------
dtr <- function(lon, lat, ref_long, ref_lat) {
	
	# Convert lat/lon to easting/northing (rectangular) coordinates
	# based on the dtr.c code written by A.H. Sandstrom (Mar 3 1995)
	
	DEG_IN_RAD <- 57.295779
	FACTOR <- 0.0067686275
	X_FACT <- 0.032339
	Y_FACT <- 0.032559
	
	radlat <- (lat + ref_lat)/(2.0 * DEG_IN_RAD)
	radlon <- (lon - ref_long)/(2.0 * DEG_IN_RAD)
	tmp1 <- sin(radlat)
	tmp1 <- tmp1^2
	tmp2 <- 1.0 - FACTOR * tmp1
	a <- sqrt(tmp2)
	b <- tmp2^1.5
	
	x <- ((lon - ref_long)/(X_FACT * a)) * cos(radlat) * 3.6
	y <- ((lat - ref_lat)/(Y_FACT * b)) * cos(radlon) * 3.6
	
	return(list(x=x,y=y))
}

