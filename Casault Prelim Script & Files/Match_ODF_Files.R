## Code for matching ODF files

## load required package
library(dplyr)
library(tidyr)
library(lubridate)

# source custom functions
source("~/Projects/Utils/R/math_functions/dtr.R")

## clean memory
gc()

##--------------------------------------------------------------------------------------------
# load ctd metadata
load("~/Projects/KKrumhansl_Data/outputs/CTD_Metadata_20161220.RData")

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
ifile <- "~/Projects/KKrumhansl_Data/data/SS_Calspp_VertDistr.txt"
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
ofile <- "~/Projects/KKrumhansl_Data/outputs/Plankton_CTD_Matches_20161222.csv"
cat("Match_Code", file=ofile, sep="\n", append=FALSE)
cat("0: no match", file=ofile, sep="\n", append=TRUE)
cat("1: match year/month/day/lat/lon", file=ofile, sep="\n", append=TRUE)
cat("2: match year/month/day and lat/lon +/-0.2 deg", file=ofile, sep="\n", append=TRUE)
cat("3: match year/month/day; 3 closest locations", file=ofile, sep="\n", append=TRUE)
cat("4: match year/month and day +/-3d; 3 closest locations", file=ofile, sep="\n", append=TRUE)
cat("", file=ofile, sep="\n", append=TRUE)
write.table(df_match, file=ofile, append=TRUE, quote=TRUE, sep=",",
            eol="\n", na="NA", dec=".", row.names=FALSE, col.names=TRUE)


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

