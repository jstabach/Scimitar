# ***************************************************************************************************************************************
# ***************************************************************************************************************************************

# Project: Oryx
# Description: Script to read, visualize, and summarize GPS data.  Result sends an e-mail message with GPS collars animals
#				Data collected on first 21 oryx fit with GPS collars (Released August 14 2016)
#				Data collected on second 23 oryx fit with GPS collars (Released January 2017)
# Author: Jared Stabach
# Date: 11 January 2017

# ***************************************************************************************************************************************
# ***************************************************************************************************************************************

# Remove anything in memory
rm(list=ls())

# Load necessary libraries to process data in this script
library(proj4)
library(dismo)
library(rgdal)
library(adehabitatLT)
library(date)
library(zoom)
library(raster)
library(sendmailR)

# Set working directory where .csv files (data) are located
setwd("F:/Jared/Projects/Oryx/GPS_Data")

# Directory to move mortalities
mortality.folder <- "F:/Jared/Projects/Oryx/GPS_Data/Mortalities"

# Path to the Read_Functions.R file
source("C:/Users/Stabachj/Dropbox (Smithsonian)/Projects/Oryx/FieldSeason_2016/R_Code/Read_Functions_oryx.R")

# Set directories for script outputs
dir <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/"
dir.Animal <- "C:/Users/Stabachj/Dropbox (Smithsonian)/Projects/Oryx/Animal_Locations/"

# Read the animal names file
AnNames <-read.csv("F:/Jared/Projects/Oryx/GPS_Data/Animals/Group_Animals.csv", header = TRUE, sep = ",")

# Set animation directory
dir.An <- "F:/Jared/Projects/Oryx/GPS_Data/Animation/"

# Specify the survey collars that are currently collecting data
Group1.Collars <- c(22100,22108,22104,22095,22109,22113,22110,22092,22101,22103,22088,22087,22086,22093,22097,22102,22098,22099,22090,21395,21397)
Group2.Collars <- c(21396,21398,21788,21813,21819,21830,21823,22085,22089,22094,22107,22111) 
Group3.Collars <- c(21815,21824,21401,21400,20843,21399,21394,21832,22114,22106,21837) 

# All Collars that are monitoring
All.Collars <- c(Group1.Collars,Group2.Collars,Group3.Collars)

# Different sampling rates
Collars.1Hr <- c(22100,22108,22104,22095,22109,22113,22110,22092,22101,22103,22088,22087,22086,22093,22097,22102,22098,22099,22090,21788,21819,21823,21824,21830,21832,22085,22089,22094,22106,22107,22111,22114)
Collars.2Hr <- c(21395,21397,21813,21815,21837)
Collars.4Hr <- c(20843,21394,21396,21398,21399,21400,21401)

# Check
length(All.Collars)==length(Collars.1Hr)+length(Collars.2Hr)+length(Collars.4Hr)

Not.Working <- c(22103)

export.plot.option <- "NO"

# *************************************************************************************************************
# *************************************************************************************************************

# Input all data
Vect.all <- list.files(path="./", pattern='_Mortality_', all.files=FALSE, full.names=FALSE)

# If there's a Mortality record...delete the file from the directory.
# Move to the Mortality folder and send message
if(length(Vect.all)>0) {
	print("Mortalities reported. Copy to directory")
	file.copy(Vect.all,mortality.folder)
	file.remove(Vect.all)
		# Send an e-mail message to alert 
		from <- sprintf("<sendmailR@%s>", Sys.info()[4])
		to <- c("<jared_stabach@hotmail.com>","<stabachj@si.edu>")
		subject <- "Mortality Reported"
		body1 <- "Check input data.  Mortality Reported."
		# Settings
		mailControl=list(smtpServer="smtp.si.edu", smtp="25",verbose=TRUE)
		# Send Mail message
		sendmail(from=from, to=to, subject=subject, msg=body1, control=mailControl)
	} else {
	print("No mortalities reported.  Moving on to processing files.")
}
	
# Now read all the files again
Vect.all <- list.files(path="./", pattern='.csv', all.files=FALSE, full.names=FALSE)

# This is the slow way to read all the data in
# ********************
# Create Null data frame to append all the data
#Vect <- NULL

# Loop through the Vect.all file
#for (i in 1:length(Vect.all)){
  ##Read in data
  #temp <- read.csv(Vect.all[i],header=TRUE,sep=",")
  ## Bind together (if data from same sensor/company, structure 'should' be the same
  #Vect <- rbind(Vect,temp)
#}
# ********************

# Faster way....use lapply (about 4x's faster):
Vect <- lapply(Vect.all,FUN=read.csv,header=TRUE,sep=",")
Vect <- do.call(rbind,Vect)
# Could use the following to calculate the procedure time
# Before code: ptm <- proc.time()
# After code: proc.time() - ptm

# Remove exact duplicates
Vect <- unique(Vect)

# Remove fields you don't want...doing this manually
Vect <- Vect[,c(2:4,13:17,46:48)]

# Look at the data and the data structure
#str(Vect)

# Rename fields...not totally necessary, but simplifies names
names <- c("ID","UTC_Date","UTC_Time","Lat","Long","Height","DOP","FixType","Main","Beacon","Temp")
names(Vect) <- names

# Now remove the collars that don't want.  This basically is an easy way to subset the data.
nrow(Vect)
Vect <- Vect[Vect$ID %in% All.Collars,]
nrow(Vect)

# What are the unique IDs?
unique(Vect$ID)
# How many?  This is just a doublecheck
if(length(unique(Vect$ID)) == 44){
	print("All Collars Included")
	} else {
	print("# ************************************************** CHECK DATASET: NOT ALL COLLARS INCLUDED ************************************************** #")
}

# *************************************************************************************************************
# *************************************************************************************************************

# Create field containing Year-Month-Day and Hour-Minute-Seconds
Vect$Date <- as.POSIXct(x=paste0(Vect$UTC_Date," ",
                                Vect$UTC_Time),
                        format = "%m/%d/%Y %I:%M:%S %p", tz="Africa/Ndjamena")

# Check to see if converted properly.  The date should not change in this instance (GMT to GMT)	
attributes(Vect$Date)

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************

# Data Cleaning:

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************

Vect1 <- Vect[Vect$ID %in% Group1.Collars,]
Vect1 <- subset(Vect1,Date >= as.POSIXct("2016-08-14 00:00:00",tz="Africa/Ndjamena"))

Vect2 <- Vect[Vect$ID %in% Group2.Collars,]
Vect2 <- subset(Vect2,Date >= as.POSIXct("2017-01-10 00:00:00",tz="Africa/Ndjamena"))

Vect3 <- Vect[Vect$ID %in% Group3.Collars,]
Vect3 <- subset(Vect3,Date >= as.POSIXct("2017-01-10 00:00:00",tz="Africa/Ndjamena"))

Vect <- rbind(Vect1,Vect2,Vect3)

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
nrow(Vect)

# Order the dataset
temp.or <-order(Vect$ID,Vect$Date,decreasing=FALSE)
Vect <-Vect[temp.or,]
Vect[1:5,]

# Remove incomplete records...Lat/Long = NA
# Use 'complete.cases'
nrow(Vect)
Vect <- Vect[complete.cases(Vect[,4:5]),]
nrow(Vect)

#summary(Vect$Long)
#summary(Vect$Lat)

# Graph
plot(Vect$Long,Vect$Lat,xlab="Longitude",ylab="Latitude",type="p",pch=".",cex=4,frame=FALSE,asp = 1,col=Vect$ID)
#lines(Vect$Long,Vect$Lat,lwd=0.5,col="light gray")
#points(Vect$Long,Vect$Lat,pch=".",cex=3)

# ********************************
# ********************************
# Graph on top of imagery to verify
e <- extent(min(Vect$Long),max(Vect$Long), min(Vect$Lat), max(Vect$Lat))
r = gmap(e,type='satellite',zoom=8,lonlat=TRUE)
plot(r)

# How many unique IDs in the dataset?
ID.all <- unique(Vect$ID)

# Loop through the dataset, subsetting each animal and graphing individually
for(i in 1:length(ID.all)){
  temp <- subset(Vect, ID == ID.all[i])
	temp <- temp[complete.cases(temp[,4:5]),] # Only omits if these records are blank...shouldn't be any since I already removed
  xy <- temp[c("Long","Lat")]
  proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
  points(temp2,pch=".",cex=4,col=i)
}

# ********************************
# ********************************
# Filter data to remove positions with poor GPS precision (DOP - Dilution of Precision)
# Execute a data cleaner function to remove poor positions(3D position < 10.0 and 2D positions < 5.0)
nrow(Vect)
Vect <- Vect.DataCleaner(Vect)
nrow(Vect)

# Graph again
plot(Vect$Long,Vect$Lat,xlab="Longitude",ylab="Latitude",type="p",pch=".",cex=4,frame=FALSE,asp = 1,col=Vect$ID)

# ********************************
# ********************************
# Remove problem records (Height/elevation must be > 0)
Vect <- Vect[which(Vect$Height > 0),]

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************

# Calculate trajectory:
#   Determine the number of missing records
#   Determine average movement
#   Determine total distance traveled over study period

# Use Trajectory function to:
#	1) Project the data from Lat/Long (Geographic) coordinates to UTM Zone 14, WGS84 (allows us to calculate movement - meters)
# 	2) Generate summary files to examine the data file

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************

# Calculate two different trajectories....since schedules are different.

Vect.1Hr <- Vect[Vect$ID %in% Collars.1Hr,]
Vect.2Hr <- Vect[Vect$ID %in% Collars.2Hr,]
Vect.4Hr <- Vect[Vect$ID %in% Collars.4Hr,]

# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************
# *************************************************************************************************************

# ***************************************************
# ***************************************************
# 1 Hour Interval
# ***************************************************
# ***************************************************
Hr.Int <- 1
#   FunctionName(Dataset,ZoneToProjectData,StartDate)
Vect.Traj <- Trajectory(Vect.1Hr,34,"2016-08-14",Hr.Int)

# Output files from the function
Vect.Traj.Dataset <- Vect.Traj[[1]]

# Remove records that are data problems; Moved 10 km in 1 hour?
temp <- Vect.Traj.Dataset[which(Vect.Traj.Dataset$dist < 10000),]
temp <- droplevels(temp)

# Remove the extra fields and re-run
Vect.1Hr <- temp[,c(11,3,3,13:20,3)]

# Rename fields...not totally necessary, but simplifies names
names <- c("ID","UTC_Date","UTC_Time","Lat","Long","Height","DOP","FixType","Main","Beacon","Temp","Date")
names(Vect.1Hr) <- names

# Reformat
Vect.1Hr$UTC_Date <- as.factor(format(Vect.1Hr$UTC_Date,"%m/%d/%Y"))
Vect.1Hr$UTC_Time <- as.factor(format(Vect.1Hr$UTC_Time, "%I:%M:%S %p", tz="Africa/Ndjamena"))

# Run trajectory on Vect dataset
#   FunctionName(Dataset,ZoneToProjectData,StartDate)
Vect.Traj <- Trajectory(Vect.1Hr,34,"2016-08-14",Hr.Int)

# Output files from the function
Vect.Traj.Dataset <- Vect.Traj[[1]] # This is the dataframe with the trajectory information
Vect.Dataset <- Vect.Traj[[2]]  # This is the original data, with the updated UTM X and Y appended
Vect.Summary <- Vect.Traj[[3]] # Summary of records received
Vect.Traj <- Vect.Traj[[4]] # The trajectory (adehabitat format)

# Plot trajectory
plot(Vect.Traj) # View all
plot(Vect.Traj[1]) # Or just 1
	# Plotting function used with the adehabitat trajectory information
	plotltr(Vect.Traj[1], "DOP") # Graphic of DOP over time......these should all be < 10, since we've cleaned them above
	plotltr(Vect.Traj[1], "FixType") # In this case, FixType is all 3.  You should not use fixes < 2D
	plotltr(Vect.Traj[1],"dt/60") # Show when data were collected.
	
# Graph the data to look at the histogram of calculated movements	
par(mfrow=c(1,1))
hist(Vect.Traj.Dataset$dist, breaks=50, xlim=c(0,10000),xlab="Steplength (m)",main="Hourly Movements",freq=FALSE,col="gray")
text(6000,0.0015,paste0("Avg. Movement (m): ",round(mean(Vect.Traj.Dataset$dist,na.rm=TRUE),digits=2)),pos=4)
	
# Look at the data points.
# Graph again
plot(Vect.Traj.Dataset$Long,Vect.Traj.Dataset$Lat,xlab="Longitude",ylab="Latitude",type="p",pch=".",cex=4,frame=FALSE,asp = 1,col=Vect.Traj.Dataset$id)

# The summary dataset provides information on the completeness of the dataset
Vect.Summary

# ***************************************************
# ***************************************************
# 2 Hour Interval
# ***************************************************
# ***************************************************
Hr.Int <- 2
Vect.Surv1 <- Trajectory(Vect.2Hr,34,"2016-08-14",Hr.Int)

# Outputs
Vect.Surv1.Traj.Dataset <- Vect.Surv1[[1]]

# Clean dataset
temp <- Vect.Surv1.Traj.Dataset[which(Vect.Surv1.Traj.Dataset$dist < 10000),]
temp <- droplevels(temp)

# Remove the extra fields and re-run
# Some files I input to make sure were same as previous
Vect.2Hr <- temp[,c(11,3,3,13:20,3)]

# Rename fields...not totally necessary, but simplifies names
names <- c("ID","UTC_Date","UTC_Time","Lat","Long","Height","DOP","FixType","Main","Beacon","Temp","Date")
names(Vect.2Hr) <- names

# Reformat
Vect.2Hr$UTC_Date <- as.factor(format(Vect.2Hr$UTC_Date,"%m/%d/%Y"))
Vect.2Hr$UTC_Time <- as.factor(format(Vect.2Hr$UTC_Time, "%I:%M:%S %p", tz="Africa/Ndjamena"))

# Run trajectory on Vect dataset
#   FunctionName(Dataset,ZoneToProjectData,StartDate)
Vect.Surv1 <- Trajectory(Vect.2Hr,34,"2016-08-14",Hr.Int)

# Output files from the function
Vect.Surv1.Traj.Dataset <- Vect.Surv1[[1]] # This is the dataframe with the trajectory information
Vect.Surv1.Dataset <- Vect.Surv1[[2]]  # This is the original data, with the updated UTM X and Y appended
Vect.Surv1.Summary <- Vect.Surv1[[3]] # Summary of records received
Vect.Surv1.Traj <- Vect.Surv1[[4]] # The trajectory (adehabitat format)

# Plot trajectory
plot(Vect.Surv1.Traj) # View all
#plot(Vect.Surv1.Traj[1]) # Or just 1
	# Plotting function used with the adehabitat trajectory information
	plotltr(Vect.Surv1.Traj[1], "DOP") # Graphic of DOP over time......these should all be < 10, since we've cleaned them above
	plotltr(Vect.Surv1.Traj[1], "FixType") # In this case, FixType is all 3.  You should not use fixes < 2D
	plotltr(Vect.Surv1.Traj[1],"dt/60") # Show when data were collected...should be every 15 minutes

Vect.Surv1.Summary
	
# ***************************************************
# ***************************************************
# 4 Hour Interval
# ***************************************************
# ***************************************************
Hr.Int <- 4
Vect.Surv2 <- Trajectory(Vect.4Hr,34,"2016-08-14",Hr.Int)

# Outputs
Vect.Surv2.Traj.Dataset <- Vect.Surv2[[1]]
# Clean for unrealistic movements
temp <- Vect.Surv2.Traj.Dataset[which(Vect.Surv2.Traj.Dataset$dist < 10000),]
temp <- droplevels(temp)

# Remove the extra fields and re-run
# Some files I input to make sure were same as previous
Vect.4Hr <- temp[,c(11,3,3,13:20,3)]

# Rename fields...not totally necessary, but simplifies names
names <- c("ID","UTC_Date","UTC_Time","Lat","Long","Height","DOP","FixType","Main","Beacon","Temp","Date")
names(Vect.4Hr) <- names

# Reformat
Vect.4Hr$UTC_Date <- as.factor(format(Vect.4Hr$UTC_Date,"%m/%d/%Y"))
Vect.4Hr$UTC_Time <- as.factor(format(Vect.4Hr$UTC_Time, "%I:%M:%S %p", tz="Africa/Ndjamena"))

# Run trajectory on Vect dataset
#   FunctionName(Dataset,ZoneToProjectData,StartDate)
Vect.Surv2 <- Trajectory(Vect.4Hr,34,"2016-08-14",Hr.Int)

# Output files from the function
Vect.Surv2.Traj.Dataset <- Vect.Surv2[[1]] # This is the dataframe with the trajectory information
Vect.Surv2.Dataset <- Vect.Surv2[[2]]  # This is the original data, with the updated UTM X and Y appended
Vect.Surv2.Summary <- Vect.Surv2[[3]] # Summary of records received
Vect.Surv2.Traj <- Vect.Surv2[[4]] # The trajectory (adehabitat format)

# Plot trajectory
plot(Vect.Surv2.Traj) # View all
#plot(Vect.Surv1.Traj[1]) # Or just 1
	# Plotting function used with the adehabitat trajectory information
	plotltr(Vect.Surv2.Traj[1], "DOP") # Graphic of DOP over time......these should all be < 10, since we've cleaned them above
	plotltr(Vect.Surv2.Traj[1], "FixType") # In this case, FixType is all 3.  You should not use fixes < 2D
	plotltr(Vect.Surv2.Traj[1],"dt/60") # Show when data were collected...should be every 15 minutes

Vect.Surv2.Summary
	
# ***************************************************
# ***************************************************

# The summary dataset provides information on the completeness of the dataset
Vect.Summary <- rbind(Vect.Summary,Vect.Surv1.Summary,Vect.Surv2.Summary)
Vect.Summary

val <- which(as.numeric(as.character(Vect.Summary$id)) %in% Not.Working)
Vect.Summary$CollarStatus <- "OK"
Vect.Summary$CollarStatus[val] <- "Failure"
Vect.Summary

# ********************************************************
# ********************************************************

# Bind the hour dataset, 2-hour, and 4-hour dataset together
Vect.Traj.Dataset <- rbind(Vect.Traj.Dataset,Vect.Surv1.Traj.Dataset,Vect.Surv2.Traj.Dataset)

# Fix the date
Vect.Traj.Dataset$Date <- as.POSIXct(x=Vect.Traj.Dataset$date, format = "%m/%d/%Y %I:%M:%S %p", tz="Africa/Ndjamena")	

# ********************************************************
# ********************************************************

# Join with the Animal Names file
Vect.Summary <-merge(Vect.Summary,AnNames, by.x = "id", by.y = "ID")
Vect.Summary <- Vect.Summary[,c(1,11,14:17,19,3:10,21)]

# Make a subset to calculate the battery life over last week
LastWeek <- Sys.Date() - 7
Vect.Sub <- subset(Vect.Traj.Dataset, date >= as.POSIXct(LastWeek))

# Sort in order
Loc.or <-order(Vect.Sub$id,decreasing=FALSE)
Vect.Sub <-Vect.Sub[Loc.or,]

MainBattery <- tapply(Vect.Sub$Main,Vect.Sub$id,min,na.rm=TRUE)
BeaconBattery <- tapply(Vect.Sub$Beacon,Vect.Sub$id,min,na.rm=TRUE)

Main <- data.frame(id=names(MainBattery),MainBattery=MainBattery)
Beacon <- data.frame(id=names(BeaconBattery),BeaconBattery=BeaconBattery)

Vect.Summary <- merge(Vect.Summary,Main, by.x = "id", by.y = "id")
Vect.Summary <- merge(Vect.Summary,Beacon, by.x = "id", by.y = "id")

Loc.or <-order(Vect.Summary$Group,Vect.Summary$Belt,decreasing=FALSE)
Vect.Summary <-Vect.Summary[Loc.or,]
Vect.Summary

# Export file
write.csv(Vect.Summary,file=paste("F:/Jared/Projects/Oryx/GPS_Data/CollarSummary/Collars.csv",sep=""), quote = FALSE, row.names = FALSE)

# ********************************************************
# ********************************************************

# Now join two data.frames together
Vect.Traj.Dataset <-merge(Vect.Traj.Dataset,AnNames, by.x = "id", by.y = "ID")

# How many unique IDs in the dataset?
ID.all <- unique(Vect.Traj.Dataset$id)

# Plot one of the animals
temp <- subset(Vect.Traj.Dataset, id == ID.all[21])
# Omit any o fthe blank records (no X/Y information)
temp <- temp[complete.cases(temp[,1:2]),] 
#xy <- temp[c("Long","Lat")]
#proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
#plot(r)
#points(temp2,pch=".",cex=4,col="red")

# Or just plot without the background...
# Here, I'm plotting the UTM coordinates
plot(temp$x,temp$y,xlab="Easting",ylab="Northing",type="n",pch=".",cex=4,frame=FALSE,main=unique(temp$id),asp = 1)
lines(temp$x,temp$y,lwd=0.5,col="light gray")
points(temp$x,temp$y,pch=".",cex=3)
points(temp$x[1],temp$y[1],pch=17,cex=1,col="green")
points(temp$x[nrow(temp)],temp$y[nrow(temp)],pch=15,cex=1,col="red")
scalebar(10000,xy=c(min(temp$x),min(temp$y)),type='line',label=c("10 km"),lwd=1,lonlat=FALSE)

# Or plot lat/long
#plot(temp$Long,temp$Lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,main=unique(temp$Belt),asp = 1)
#lines(temp$Long,temp$Lat,lwd=0.5,col="light gray")
#points(temp$Long,temp$Lat,pch=".",cex=3)
#points(temp$Long[1],temp$Lat[1],pch=17,cex=1,col="green")
#points(temp$Long[nrow(temp)],temp$Lat[nrow(temp)],pch=15,cex=1,col="red")
#scalebar(10,xy=c(min(temp$Long),min(temp$Lat)),type='line',label=c("10 km"),lwd=1)

# Plot each individual
for (i in 1:length(ID.all)){
	temp <- subset(Vect.Traj.Dataset, id == ID.all[i])
	
	# Omit any o fthe blank records (no X/Y information)
	temp <- temp[complete.cases(temp[,1:2]),] 
	
	# Generate file name to save:
	#outfile <- paste0(dir,"Animals/Animal_",unique(temp$Belt),".png")
	outfile <- paste0(dir.Animal,"Animal_",unique(temp$Belt),".png")
	png(file=outfile, width=768, height=768)
	
	plot(temp$Long,temp$Lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,main=unique(temp$Belt),asp = 1)
		text(x=min(temp$Long),y=max(temp$Lat),paste0("Last Position: ",as.character(temp[nrow(temp),4])),pos=4,cex=1)
		lines(temp$Long,temp$Lat,lwd=0.5,col="light gray")
		points(temp$Long,temp$Lat,pch=".",cex=3)
		points(temp$Long[1],temp$Lat[1],pch=17,cex=1,col="green")
		points(temp$Long[nrow(temp)],temp$Lat[nrow(temp)],pch=15,cex=1,col="red")
		
		# Create Scale Bar
		Ext.Map <- cbind(temp$Long,temp$Lat)
		colnames(Ext.Map) <- c("Long","Lat")
		Pointlayer <- SpatialPoints(Ext.Map)
		d <- pointDistance(Pointlayer,longlat=T)
		max(d,na.rm=TRUE)
		
		# Create ScaleBar Extent
		Bar.Extent <- round((max(d,na.rm=TRUE)/3)/1000,digits=0)
		
		# Draw ScaleBar
		scalebar(Bar.Extent,xy=c(min(temp$Long),min(temp$Lat)),type='line',label=c(paste0(Bar.Extent," km")),lwd=1)
		
	# Output the file
	#outfile <- paste0(dir,"Animals/Animal_",unique(temp$Belt))
	#if(export.plot.option == "YES"){
		#savePlot(filename = outfile, type = c("png"), device = dev.cur(),restoreConsole = TRUE)
	#}
	dev.off()
}	

# Or, plot all the animals...same procedure as above....but now am looping through individuals
e <- extent(min(Vect$Long),max(Vect$Long), min(Vect$Lat), max(Vect$Lat))
r = gmap(e,type='satellite',zoom=9,lonlat=TRUE)
plot(r)

for(i in 1:length(ID.all)){
	temp <- subset(Vect.Traj.Dataset, id == ID.all[i])
	temp <- temp[complete.cases(temp[,1:2]),] # Only omits if these records are blank
	xy <- temp[c("Long","Lat")]
	proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
	temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
	points(temp2,pch=".",cex=4,col=i)
}

# *******************************************
# *******************************************
# Summarize movements
# *******************************************
# *******************************************

# Julian date into the data.frame
Year <- as.integer(format(Vect.Traj.Dataset$date,"%Y"))
Month <- as.integer(format(Vect.Traj.Dataset$date,"%m"))
Day <- as.integer(format(Vect.Traj.Dataset$date,"%d"))
Vect.Traj.Dataset$Julian <- mdy.date(Month,Day,Year,nineteen=TRUE,fillday=FALSE,fillmonth=FALSE)

# Create a blank frame to hold everything
# *******************************************
# *******************************************
Id.sum <- as.data.frame(ID.all)

Day.Sums.All <- NULL

for (i in 1:length(ID.all)){
	temp <- subset(Vect.Traj.Dataset, id == ID.all[i])
	Id.sum$Belt[i] <- as.character(unique(temp$Belt))
	# First, summarize how many days monitored
	Id.sum$Start[i] <- as.character(temp[1,4])
	Id.sum$End[i] <- as.character(temp[nrow(temp),4])
		Id.sum$Days[i] <- round(temp[nrow(temp),4]-temp[1,4],digits=1)
		# Or do
		#Id.sum$SumDays[i] <- round(sum(temp$dt,na.rm=TRUE)/86400,digits=0) #86400 is converting seconds to days (60 secs * 60 mins * 24 hour)
		
		# Add in the temporal resolution, calculating the mean
		Id.sum$Int[i] <- paste0(round(mean(temp$dt/3600,na.rm=TRUE))," Hour")

	# Sum of Movement within period monitored
	Id.sum$AvgMove[i] <- round(mean(temp$dist,na.rm=TRUE),digits=2) # This is meters	
	Id.sum$SumMove[i] <- round(sum(temp$dist,na.rm=TRUE)/1000,digits=2) # This is kilometers
	
	# Calculate the average daily movement.  Use Julian day
	Days <- unique(temp$Julian)
	
	# Create blank dataset to hold everything.
	Sum.Day2 <- NULL
	
	for (j in 1:length(Days)){
	  # Subset by day
		temp2 <- subset(temp, Julian == Days[j])
		Sum.Day1 <- round(sum(temp2$dist,na.rm=TRUE)/1000,digits=2)
		#Sum.Day2 <- rbind(Sum.Day2,Sum.Day1)
		Sum.Day2 <- c(Sum.Day2,Sum.Day1)
	}
	# Average how much animals move each day
	Id.sum$AvgDailyMve[i] <- round(mean(Sum.Day2),digits=2)
	Day.Sums.All <- c(Day.Sums.All,Sum.Day2)
}

Id.sum
write.csv(Id.sum,file=paste("F:/Jared/Projects/Oryx/GPS_Data/CollarSummary/Idsum.csv",sep=""), quote = FALSE, row.names = FALSE)

# Calculate the weighted mean of 1 hour data(otherwise, impacted by days monitored)
Id.sum.subset <- subset(Id.sum, Int == "1 Hour")
weighted.mean(Id.sum.subset$AvgDailyMve,Id.sum.subset$Days)

# Look at the movement histograms
hist(Vect.Traj.Dataset$dist,freq=FALSE,breaks=200,main="Movements",xlim=c(0,10000),xlab="Distance (m)")
hist(Day.Sums.All,freq=TRUE,breaks=100,main="Daily Movements",xlim=c(0,60),xlab="Distance (km)") # This has all the animals together....including pen animals

# *******************************************
# *******************************************

# Movement of one animal (herd animal)
Herd.An <- subset(Vect.Traj.Dataset, id == 22090)

# Calculate the average daily movement.  Use Julian day
Days.Herd.An <- unique(Herd.An$Julian)

# Create blank dataset to hold everything.
Sum.Herd.An <- NULL

for (j in 1:length(Days.Herd.An)){
	  # Subset by day
		temp2 <- subset(Herd.An, Julian == Days.Herd.An[j])
		Sum.Day1 <- round(sum(temp2$dist,na.rm=TRUE)/1000,digits=2)
		Sum.Herd.An <- c(Sum.Herd.An,Sum.Day1)
}

hist(Sum.Herd.An,freq=FALSE,breaks=100,main="Daily Movements",xlim=c(0,60),xlab="Distance (km)")
# Draw gamma distribution
v <-var(Sum.Herd.An, na.rm=TRUE)
mn <-mean(Sum.Herd.An, na.rm=TRUE)
shp <- mn^2/v
rate <- mn/v
x.seq <- seq(0,6000,1)
z=dgamma(x.seq,shape=shp,rate=rate)
lines(x.seq,z,col="red",lwd=2)

# *******************************************
# *******************************************

# Fix format of ID column to work with function
colnames(Vect.Traj.Dataset)[1] <- "ID"

# How many unique ids....use to iterate through
ID.All <- unique(Vect.Traj.Dataset$ID)

# Run function to create animation
##Displace.Animation(21,Vect.Traj.Dataset,dir.An,ID.All)
##Displace.Animation.pts(1,Vect.Traj.Dataset,dir.An,ID.All)

# *******************************************
# *******************************************
# What's the MCP area of each individual

library(adehabitatHR)
temp <- Vect.Traj.Dataset[complete.cases(Vect.Traj.Dataset[,2:3]),]
sp <- SpatialPoints(temp[,c(2,3)])
spdf <- SpatialPointsDataFrame(sp,temp)
plot(spdf)
plot(spdf,cex=0.5,pch=15,col=spdf@data$ID, xlab="Easting", ylab="Northing",type="n",asp=1)
scalebar(20000,xy=c(390000,min(spdf@coords[,2])),type='line',label=c("20 km"),lwd=1,lonlat=FALSE)
axis(1)
axis(2)

xy <- data.frame(ID = 1, X = 20.07676, Y = 14.88397)
coordinates(xy) <- c("X","Y")
proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
RS <- spTransform(xy,CRS("+proj=utm +zone=34 +units=m +datum=WGS84"))
points(RS,pch=17,cex=1,col="red")
text(RS,"Release Site",pos=1,cex=0.75)

mcp.test <- mcp(spdf,percent=100)
plot(mcp.test, add=TRUE,border="red")
mcpAll.area <-mcp.area(spdf, percent = seq(20,100, by = 5), unin = "m", unout = "km2")

# *******************************************
# *******************************************
# Calculate the distance to previous known position
# Load Geosphere library
library(geosphere)

# For each animal, loop through and group the latest record
ID.All <- unique(Vect.Traj.Dataset$ID)
proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

Last.Position <- NULL

for (i in 1:length(ID.All)){
	temp <- subset(Vect.Traj.Dataset, ID == ID.All[i])
	temp <- temp[complete.cases(temp[,13:14]),]
	# Grab last record
	temp.last <- temp[nrow(temp),]
		# Now fix the columns that you want and in the correct order
		temp.last <- temp.last[,c(24,1,29,32,13:14,4)]
		
		# Calculate time difference
		time.diff <- round(difftime(format(Sys.time(),tz="Africa/Ndjamena"),format(temp.last$date,tz="Africa/Ndjamena")),digits=1)
			time.unit <- attributes(time.diff)
			time.unit <- time.unit$units
			TimeLast <- paste0(as.numeric(time.diff)," ",time.unit)
		temp.Last <- cbind(temp.last,TimeLast)
	#Last.Position <- rbind(Last.Position,temp.Last)
	
	# Grab, second to last position
	Temp.2.last <- temp[nrow(temp)-1,]
	
		xy <- temp.Last[c("Long","Lat")]
		Lastxy <- Temp.2.last[c("Long","Lat")]

		# Get Bearing in degress from last position
		#Bearing <- round(bearing(Lastxy,xy),digits=2)
		Bearing <- round(bearingRhumb(Lastxy,xy),digits=2)
	
		# Switch bearing so that if -180...will be 0
		#Bearing <- ifelse(Bearing==-180,0,Bearing)
		Bearing[is.na(Bearing)] <- 0
	
		# Calculate distances moved
		#Distance <- round(distm(xy,Lastxy)/1000,digits=2)
		Distance <- round(Temp.2.last$dist/1000,digits=2)
		Distance[is.na(Distance)] <- 0
		
		# Append previous distance and bearing
		#temp.Last$Bearing_Prev <- ifelse(is.na(Temp.2.last$abs.angle),NA,round(Temp.2.last$abs.angle,digits=2))
		#temp.Last$Dist_Prev <- ifelse(is.na(Temp.2.last$dist),NA,round(Temp.2.last$dist/1000,digits=2))
		temp.Last$Bearing_Prev <- Bearing
		temp.Last$Dist_Prev <- Distance
	
	# Bind together
	Last.Position <- rbind(Last.Position,temp.Last)
}

# Set Column Names
colnames(Last.Position) <- c("Name","ID","Sex","Group","Lat","Long","Date","TimeLast","Bearing_Prev","Dist_Prev")

# Look at data
Last.Position

# Remove the collar that is not working
Last.Position <- Last.Position[!(Last.Position$ID %in% Not.Working),]

# Re-Order Frame
Loc.or <-order(Last.Position$Group,Last.Position$Name,decreasing=FALSE)
Last.Position <-Last.Position[Loc.or,]

# *******************************************
# *******************************************
# Append to previous records
#Last.Pos.All <- NULL
#Last.Pos.All <-read.csv("C:/Users/Stabachj/Dropbox (Smithsonian)/Projects/Oryx/Animal_Locations/LastPosAll.csv", header = TRUE, sep = ",")
#Last.Pos.All$Date <- as.POSIXct(as.character(Last.Pos.All$Date), tz="Africa/Ndjamena")
#Last.Pos.All$SysDate <- as.POSIXct(as.character(Last.Pos.All$SysDate), tz="Africa/Ndjamena")

#Daily <- Last.Position
#Daily$SysDate <- format(Sys.time(),tz="Africa/Ndjamena")
#Last.Pos.All <- rbind(Last.Pos.All,Daily)
#write.csv(Last.Pos.All,file="C:/Users/Stabachj/Dropbox (Smithsonian)/Projects/Oryx/Animal_Locations/LastPosAll.csv", quote = FALSE, row.names = FALSE)

# *******************************************
# *******************************************

# Put in format to export
xy <- Last.Position[c("Long","Lat")]
output <- SpatialPointsDataFrame(xy,Last.Position,proj4string = CRS(proj.info))
writeOGR(output, dsn=paste0(dir,"LatestPosition/GPS_Data_OryxPositions.kml"), layer="GPS_Data",driver="KML", overwrite_layer=TRUE)

# Export to GPX
output@data$name <- output@data$Name
writeOGR(output["name"], dsn=paste0(dir,"LatestPosition/GPS_Data_OryxPositions.gpx"), layer="GPS_Data",driver="GPX", overwrite_layer=TRUE)

# loading the required packages
# ***************************************************
library(ggplot2)
library(ggmap)
library(ggsn)

# Research Location
# This binds the research location with the mean of the last positions to form an arrow
coords <- cbind(20.07676,14.88397,mean(Last.Position$Long),mean(Last.Position$Lat))	
coords <- as.data.frame(coords)
coords$RSite <- "Research Site"
colnames(coords) <- c("Long","Lat","EndLong","EndLat","RSite")

# Create a unique file to load into get_map.  Include research site
Map.Extent <- rbind(Last.Position[,5:6], coords[,c(2,1)])

box <- make_bbox(Long,Lat,data=Map.Extent)
zoom.val <- calc_zoom(box)

# Get Map
#map.chad <- get_map(location = c(lon = min(Map.Extent$Long), lat = max(Map.Extent$Lat)), zoom = 12, maptype = "satellite", scale = 1)
map.chad <- get_map(location = box, zoom = 10, maptype = "satellite", scale=1)
					  
#Pointlayer <- SpatialPoints(cbind(data$x,data$y))
Map.Extent2 <- cbind(Map.Extent[,2],Map.Extent[,1])
colnames(Map.Extent2) <- c("Long","Lat")
Pointlayer <- SpatialPoints(Map.Extent2)
d <- pointDistance(Pointlayer,longlat=T)
max(d,na.rm=TRUE)

# Shapefiles to display
shpdir <- "C:/Jared/Data/Oryx/ProtectedArea"
OROAGR <- readOGR(dsn=shpdir, layer="OROAGR_Jul2016_DDWgs84")
OROAGR2 <- fortify(OROAGR)

# ***************************************************
# ***************************************************
# Assessment of date of last position
Current.Date <- as.Date(Sys.Date(),format="%m/%d/%Y",tz="Africa/Ndjamena")
Connect.Date <- as.Date(Last.Position[,7],format="%m/%d/%Y",tz="Africa/Ndjamena")
Diff.Date <- as.numeric(Current.Date - Connect.Date)
	NotCommune <- ifelse(Diff.Date > 1,"YES","NO")
	rowNO <- which(NotCommune=="YES")
	
	if(length(rowNO)>0){
		#test <- Last.Position[-rowNO,]
		NotCommuneRecs <- Last.Position[rowNO,]
		colnames(NotCommuneRecs) <- c("Name","ID","Sex","Group","Lat","Long","Date","TimeLast")
		printMessageNotCommune <- paste0("NOTE: ",NotCommuneRecs$Name," has not communicated since ",format(NotCommuneRecs$Date,"%m/%d/%Y",tz="WAST"),". Position reported is old.")
		}
		
# ***************************************************
# ***************************************************

# Create ScaleBar Extent
Bar.Extent <- round((max(d,na.rm=TRUE)/3)/1000,digits=0)

outfile = paste0(dir,"LatestImage/GPS_Data_OryxPositions.png")
png(file=outfile, width=768, height=768)

# plotting the map with some points on it
if(length(rowNO)==0){
(p <- ggmap(map.chad) +
  geom_point(data = Last.Position, aes(x = Long, y = Lat, fill="red"), size = 2.5, shape = 22) +
  #geom_text(data = Last.Position, aes(x = Long, y = Lat, label = Name), size = 3.0, vjust = 1.5, hjust = 0) +
	## Attach with body
	#if(length(rowNO)>0){
		#geom_text(data = NotCommuneRecs, aes(x = Long, y = Lat, label = paste0("Old Position: ",Date)), size = 3.0, vjust = 3, hjust = 0, color="red") +
	#}
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle(paste0("Scimitar-horned oryx GPS Locations (",nrow(xy),") - ",format(Sys.time(),"%d-%m-%Y %H:%M %p", tz="Africa/Ndjamena"))) + 
  coord_equal() + 
  #theme(axis.text.y = element_text(angle=90,hjust=0.5,vjust=1)) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  geom_segment(data = coords, aes(x = Long, y=Lat, xend=EndLong, yend=EndLat), arrow = arrow(), size=1, color="blue") + 
  geom_point(data = coords, aes(x = Long, y = Lat), size = 2.5, shape = 18, color="black") + 
  #geom_polygon(aes(x = long, y = lat, group = group),data=OROAGR2,alpha=0,size=1, color="red") + #,fill="blue"
  geom_text(data = coords, aes(x = Long, y = Lat, label = RSite), size = 3.5, vjust = 1.5, hjust = 0) +
  #geom_point(data = coords, aes(x = Long, y = Lat), size = 3.5, shape = 0) +
  scalebar(dist = Bar.Extent, dd2km = TRUE, model = 'WGS84', x.min = box[1], x.max = box[3], y.min = box[2]-0.05, y.max = box[4],location="bottomright",st.size = 4, st.bottom=TRUE,st.dist=0.05,height = 0.01))
  } else {
  (p <- ggmap(map.chad) +
  geom_point(data = Last.Position, aes(x = Long, y = Lat, fill="red"), size = 2.5, shape = 22) +
  #geom_text(data = Last.Position, aes(x = Long, y = Lat, label = Name), size = 3.0, vjust = 1.5, hjust = 0) +
  geom_text(data = NotCommuneRecs, aes(x = Long, y = Lat, label = paste0("Old Position (",Name,"): ",Date)), size = 3.0, vjust = 3, hjust = 0, color="red") +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle(paste0("Scimitar-horned oryx GPS Locations (",nrow(xy),") - ",format(Sys.time(),"%d-%m-%Y %H:%M %p", tz="Africa/Ndjamena"))) + 
  coord_equal() + 
  #theme(axis.text.y = element_text(angle=90,hjust=0.5,vjust=1)) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  geom_segment(data = coords, aes(x = Long, y=Lat, xend=EndLong, yend=EndLat), arrow = arrow(), size=1, color="blue") + 
  geom_point(data = coords, aes(x = Long, y = Lat), size = 2.5, shape = 18, color="black") + 
  #geom_polygon(aes(x = long, y = lat, group = group),data=OROAGR2,alpha=0,size=1, color="red") + #,fill="blue"
  geom_text(data = coords, aes(x = Long, y = Lat, label = RSite), size = 3.5, vjust = 1.5, hjust = 0) +
  #geom_point(data = coords, aes(x = Long, y = Lat), size = 3.5, shape = 0) +
  scalebar(dist = Bar.Extent, dd2km = TRUE, model = 'WGS84', x.min = box[1], x.max = box[3], y.min = box[2]-0.05, y.max = box[4],location="bottomright",st.size = 4, st.bottom=TRUE,st.dist=0.05,height = 0.01))
}
  
# Export the image
# Save plot
#outfile = paste0(dir,"LatestImage/GPS_Data_OryxPositions")
#if(export.plot.option == "YES"){
#	savePlot(filename = outfile,type = c("png"), device = dev.cur(),restoreConsole = TRUE)
#}
dev.off()

# ***************************************************
# ***************************************************
# Create a Table where all the data points are coordinated.  This is for Tim.
Old.Date <- min(Last.Position$Date)

# What to do if the old date is greater than 5 days from the current date?
Diff.Date <- as.numeric(Current.Date - as.Date(Old.Date,format="%m/%d/%Y",tz="Africa/Ndjamena"))
if(Diff.Date > 5){
	rowNO <- which(as.Date(Last.Position[,7],format="%m/%d/%Y",tz="Africa/Ndjamena")<(Current.Date-5))
	Last.Position2 <- Last.Position[-rowNO,]
	Old.Date <- min(Last.Position2$Date)
	}

# Determine if the Old.Date is Odd or even.  If odd, substract an hour to include the collars that are collecting every 2 hours (even)
odds <- seq(1,23,2)
if (format(Old.Date, "%H") %in% odds){
	Old.Date <- Old.Date - 3600 # Substract an hour, if true
	}
	
#Test.Set <- Vect.Traj.Dataset
#Test.Set$NewDate <- format(Test.Set$Date, "%Y-%m-%d %H", tz="Africa/Ndjamena")
#Test <- subset(Test.Set, NewDate == format(Old.Date, "%Y-%m-%d %H", tz="Africa/Ndjamena"))
# Just subset the dates to look for a match on the Y-m-d and the hour
Traj.Sub <- subset(Vect.Traj.Dataset, format(Date,"%Y-%m-%d %H", tz="Africa/Ndjamena") == format(Old.Date, "%Y-%m-%d %H", tz="Africa/Ndjamena"))

Traj.Sub <- Traj.Sub[complete.cases(Traj.Sub[,13:14]),]
	# Now fix the columns that you want and in the correct order
	Traj.Sub <- Traj.Sub[,c(24,1,29,32,13:14,4)]
		
		# Calculate time difference
		How.Old <- round(difftime(format(Sys.time(),tz="Africa/Ndjamena"),format(Traj.Sub$date,tz="Africa/Ndjamena")),digits=1)
			time.unit2 <- attributes(How.Old)
			time.unit2 <- time.unit2$units
			TimeLast2 <- paste0(as.numeric(How.Old)," ",time.unit2)
		Traj.Sub <- cbind(Traj.Sub,TimeLast2)

# Set Column Names
colnames(Traj.Sub) <- c("Name","ID","Sex","Group","Lat","Long","Date","TimeLast")

# And put in format to export
xy2 <- Traj.Sub[c("Long","Lat")]
output.old <- SpatialPointsDataFrame(xy2,Traj.Sub,proj4string = CRS(proj.info))
writeOGR(output.old, dsn=paste0(dir,"LatestPosition/GPS_Data_CoordinatedPositions.kml"), layer="GPS_Data",driver="KML", overwrite_layer=TRUE)

# Export to GPX
output.old@data$name <- output.old@data$Name
writeOGR(output.old["name"], dsn=paste0(dir,"LatestPosition/GPS_Data_CoordinatedPositions.gpx"), layer="GPS_Data",driver="GPX", overwrite_layer=TRUE)

# ***************************************************
# ***************************************************
# Load package
library(xtable)

# Convert the Last.Position dataset...to maintain digits
LP <- Last.Position
LP[,7] <- format(LP[,7])
LP[,5] <- as.character(LP[,5])
LP[,6] <- as.character(LP[,6])

# Send Plain Email
#from <- "<stabachj@si.edu>"
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- c("<Tim.Wacher@zsl.org>","<marc_dethier@hotmail.com>","<mht199@hotmail.com>","<john.newby@bluewin.ch>","<addax1949@gmail.com>", "<stabachj@si.edu>","<martinim@si.edu>","<maison1991@hotmail.de>","<monforts@si.edu>","<songerm@si.edu>","<leimgruberp@si.edu>","<ReedDM@si.edu>","<Tim.Wacher32@gmail.com>","<pretorian33@live.com>","<ricardo.pusey@ead.ae>", "<justin.chuven@ead.ae>", "<katherine.mertes@yale.edu>")
#to <- c("<jared_stabach@hotmail.com>","<stabachj@si.edu>")
subject <- paste0("Latest Fixes: ",Sys.Date()," (",nrow(xy)," Animals)")

# Message Body
body1 <- "Daily Report with latest positions for each animal.  Old positions (> 1 day) noted and marked in image. 
 
Data provided in .kml and .gpx file format, to be viewed in GoogleEarth (.kml) or imported into Garmin GPS units (.kml or .gpx).  An image file (.png) with the latest position of each animal also provided.  

The blue arrow in the image shows the direction to the geographic center of all the collars from the research site.

A table with the date and time that each collar last communicated is enclosed at the bottom of this message.  The bearing (degree) and distance (km) from the last reported position also provided.

Coordinated position files are the same data, but have been subset to an old position where each collar has transmitted data at the same time.  These files are coordinated/matched in time

Please reply to stabachj@si.edu with questions.

Thanks,

Jared
	
--
Jared Stabach
Post-Doctoral Research Fellow
Smithsonian Conservation Biology Institute
Conservation Ecology Center
National Zoological Park
1500 Remount Road, Front Royal, VA 22630
T: 540-635-6578 | Email: stabachj@si.edu 
"

#body1[["Content-Type"]] <-"text/html charset=UTF-8"

# Generate HTML Table
w <- print(xtable(LP),include.rownames=FALSE,type="html")
msg <- mime_part(w)

## Override content type.
msg[["headers"]][["Content-Type"]] <- "text/html"

# Attachments
attachmentGPX <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/LatestPosition/GPS_Data_OryxPositions.gpx"
attachmentKML <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/LatestPosition/GPS_Data_OryxPositions.kml"
attachmentPNG <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/LatestImage/GPS_Data_OryxPositions.png"
attachmentGPX2 <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/LatestPosition/GPS_Data_CoordinatedPositions.gpx"
attachmentKML2 <- "F:/Jared/Projects/Oryx/GPS_Data/Outputs/LatestPosition/GPS_Data_CoordinatedPositions.kml"

nameGPX <- paste0("Oryx Positions - ",format(Sys.Date(),tz="Africa/Ndjamena")," (",format(Sys.time(),"%H",tz="Africa/Ndjamena"),") - ",nrow(xy)," animals.GPX")
nameKML <- paste0("Oryx Positions - ",format(Sys.Date(),tz="Africa/Ndjamena")," (",format(Sys.time(),"%H",tz="Africa/Ndjamena"),") - ",nrow(xy)," animals.KML")
namePNG <- paste0("Oryx Positions - ",format(Sys.Date(),tz="Africa/Ndjamena")," (",format(Sys.time(),"%H",tz="Africa/Ndjamena"),") - ",nrow(xy)," animals.png")
nameGPX2 <- paste0("Coordinated Positions - ",format(Old.Date, "%Y-%m-%d")," (",format(Old.Date, "%H"),") - ",nrow(xy2)," animals.GPX")
nameKML2 <- paste0("Coordinated Positions - ",format(Old.Date, "%Y-%m-%d")," (",format(Old.Date, "%H"),") - ",nrow(xy2)," animals.KML")

#nameGPX <- "GPS_Data_OryxPositions.gpx"
#nameKML <- "GPS_Data_OryxPositions.kml"
#namePNG <- "GPS_Data_OryxPositions.png"

# Attachment Object
attachmentObject1 <- mime_part(x=attachmentGPX, name=nameGPX)
attachmentObject2 <- mime_part(x=attachmentKML, name=nameKML)
attachmentObject3 <- mime_part(x=attachmentPNG, name=namePNG)
attachmentObject4 <- mime_part(x=attachmentGPX2, name=nameGPX2)
attachmentObject5 <- mime_part(x=attachmentKML2, name=nameKML2)

# Attach with body
if(length(rowNO)==0){
bodyWithAttachment <- list(body1,msg,attachmentObject1,attachmentObject2,attachmentObject3,attachmentObject4,attachmentObject5)
} else {
bodyWithAttachment <- list(body1,printMessageNotCommune,msg,attachmentObject1,attachmentObject2,attachmentObject3,attachmentObject4,attachmentObject5)
}

# Settings
mailControl=list(smtpServer="smtp.si.edu", smtp="25",verbose=TRUE)

# Send Mail message
sendmail(from=from, to=to, subject=subject, msg=bodyWithAttachment, control=mailControl)

# ***************************************************
# ***************************************************
# Graph movements by hour...activity patterns
head(Vect.Traj.Dataset)

Vect.Traj.Dataset$Hour <- formatC(format(Vect.Traj.Dataset$date,"%H"),width=2,format="d",flag="0")
temp <- Vect.Traj.Dataset

# Sort in order
Loc.or <-order(temp$Hour,decreasing=FALSE)
temp <-temp[Loc.or,]

Hour.unique <- unique(temp$Hour)

# Summarize the Hourly Movements
moves <- aggregate(dist ~ Hour, data = temp, summary)
hour.move <- aggregate(dist ~ Hour, data = temp, mean)
plot(hour.move$Hour,hour.move$dist,type="b", xlab="Hour",ylab="Average Hourly Movement (m)", ylim=c(0,2000), frame=FALSE, axes=FALSE)
	axis(1, at=-0.5:22.5,lab=0:23)
	axis(2)
	
ID.All <- unique(temp$ID)

for (i in 1:length(ID.All)){
	temp.An <- subset(temp, ID == ID.All[20])
		hour.move <- aggregate(dist ~ Hour, data = temp.An, mean)
			plot(hour.move$Hour,hour.move$dist,type="b", xlab="Hour",ylab="Average Hourly Movement (m)", ylim=c(0,2000), frame=FALSE, axes=FALSE)
				axis(1, at=-0.5:22.5,lab=0:23)
				axis(2)
}

# ***************************************************************************************************************************************
# ***************************************************************************************************************************************

# End Code

# ***************************************************************************************************************************************
# ***************************************************************************************************************************************
