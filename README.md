# CalDist
The aim of this project is to explore hypotheses about environmental factors that determine the vertical distribution of Calanus copepods in the region. We have depth-stratified Calanus abundance data from the Scotian Shelf and Slope, Gulf of Maine, Gulf of St. Lawrence, St. Lawrence Estuary, and the Newfoundland Shelf, which we’d like to link to CTD profiles. In the end, we’re hoping for:  1) measures of temperature and salinity conditions for each plankton depth bin (e.g. average, max, min, and the range), and 2) Summary metrics of the water column at each sampling station and for each sampling event (e.g. thermocline depth, mixed layer depth, bottom of cold intermediate layer). This will require:

-	Writing scripts to read CTD profile data of varying formats (CSV, TXT, ODF)  into R and derive variables of interest 
-	Writing scripts to calculate summary metrics of temperature and salinity conditions for each plankton depth bin
-	Writing scripts to calculate summary metrics of the water column at each plankton sampling station, for each sampling event
-	Writing scripts to match all environmental metrics to plankton data files in R
