############################################
#  LinC-STEM -- Data Analysis
#
#  Nov. 2025	--	M.Ponce (CMS/UTSC)
############################################


source("auxFns.R")

#####################
# Plants with SALTS #
#####################

analysis_salt <- function(filename="plants_salts.csv", group=NA, savePlt=TRUE) {

	mprint("reading data for <<Plants with Salts treatment>>")

	# read data
	salts <- read.csv(filename)

	# select group data
	gid <- group_selection(list(1:4, 5:7, 8:10, 11:14), salts, group)

	# remove group.id data
	salts <- salts[,-1]
	salts <- salts[gid,]

	print(salts)

	pause()

	# plot boxplots for different treatments
	boxplot(salts)
	#boxplot(salts, col=c(2:ncol(salts)))

	if (savePlt) savePlot("salts_analysis.pdf")

	# T-tests
	ttests(salts, ttype=2)
}

##########################################

############################
## C o m p e t i t i o n  ##
############################
analysis_competition <- function(filename="competition.csv", group=NA, savePlt=TRUE) {

	mprint("reading data for <<Plants with competition>>")

	# read competition data
	comp_data <- read.csv(filename)

	# select group data
	gid <- group_selection(list(1:3, 4:7, 8:11, 12:15), comp_data, group)
	comp_data <- comp_data[gid,]

	# visual inspection of the data
	print(comp_data)

	pause()

	# define ratios
	low <- comp_data$Low.F/comp_data$Low.I
	medium <- comp_data$Med.F/comp_data$Med.I
	high <- comp_data$High.F/comp_data$High.I


	# graphical inspection of all the data
	plot(comp_data)
	#if (savePlt) savePlot("competition_analysis.pdf")

	pause()

	# comparison of the treatments
	boxplot(low, medium, high)
	#if (savePlt) savePlot("competition2_analysis.pdf")
	#
	pause()

	# change colors
	boxplot(low,medium,high, col=c(2,3,4))
	if (savePlt) savePlot("competition_analysis.pdf")

	pause()

	# change colors
	#boxplot(low,medium,high, col=c("yellow","blue","orange"))

	# T-tests
	ttests(data.frame(low,medium,high), ttype=2)

	ttests(data.frame(medium,high), ttype=2)
}

##########################################

###############################
###  S U N   vs   S H A D E  ##
###############################

analysis_sun_shade <- function(filename="Plants_shade_sun.csv", group=NA, savePlt=TRUE) {
	
	mprint("Reading data for <<Plants with sun/shade treatment>>")

	# read data
	plant_sun_shade_data <- read.csv("Plants_shade_sun.csv")

	# GROUPS
	# West Hill
	#group_WH <- 1:7
	# Pearson
	#group_Pearson <- 8:13
	# Cedarbrae #1
	#group_Cedar1 <- 14:18
	# Cedarbrae #2
	#group_Cedar2 <- 19:23
	group_id <- group_selection(list(14:18, 19:23, 8:13, 1:7), plant_sun_shade_data, group)

	plant_sun_shade_data <- plant_sun_shade_data[group_id,]

	print(plant_sun_shade_data)

	pause()

	boxplot(plant_sun_shade_data[,-1])
	if (savePlt) savePlot("sun_shade_analysis.pdf")
	pause()

	par(mfrow=c(1,4))
	boxplot(plant_sun_shade_data$Absorbance ~ plant_sun_shade_data$Plant.Exposure)
	boxplot(plant_sun_shade_data$Petiole.Length ~ plant_sun_shade_data$Plant.Exposure)
	boxplot(plant_sun_shade_data$Stomata.Density ~ plant_sun_shade_data$Plant.Exposure)
	boxplot(plant_sun_shade_data$Chlorophyll.Concentration ~ plant_sun_shade_data$Plant.Exposure)

	if (savePlt) savePlot("sun_shade_4_analysis.pdf")
	
	par(mfrow=c(1,1))
	
	# T-test
	#ttest_plants(plant_sun_shade_data)
	ttests(plant_sun_shade_data)
}

#########################################
#########################################

group_selection <- function(grp_entries, dataset, group) {

	# groups definitions
	# Cedarbrae #1
	group_Cedar1 <- grp_entries[[1]]
	# Cedarbrae #2
	group_Cedar2 <- grp_entries[[2]]
	# Pearson
	group_Pearson <- grp_entries[[3]]
	# West Hill
	group_WH <- grp_entries[[4]]

	if (is.na(group)) {
		gid <- 1:nrow(dataset)
	} else {
		if (group == "Cedarbrae1") {
			gid <- group_Cedar1
		} else if (group == "Cedarbrae2") {
			gid <- group_Cedar2
		} else if (group == "Pearson") {
			gid <- group_Pearson
		} else if (group == "WestHill") {
			gid <- group_WH
		} else {
			gid <- NA
			stop("error ",group," not recognized!")
		}
	}

	#print(paste(group, "--", gid))

	return(gid)
}

#########################################

# function to allow to analyze data per groups
ttests <- function(mydata, ix0=2, ttype=1) {

	# Observations
	obs <- colnames(mydata)

	# run T-test for all observations
	for (i in obs[ix0:length(obs)]) {
		mprint(paste(">>> Running T-test for ",obs[1], "vs", i,"..."))
		if (ttype==1) {
			print(t.test(mydata[,i] ~ mydata[,1]))
		} else {  
			print(t.test(mydata[,i], mydata[,1]))
		}
	}
}

##########################################


# function to allow to analyze data per fgroups
ttest_plants <- function(plant_sun_shade_data, group_id=1:nrow(plant_sun_shade_data)) {
	# define variables, slicing per group
	exposure <- plant_sun_shade_data$Plant.Exposure[group_id]
	absorbance <- plant_sun_shade_data$Absorbance[group_id] 
	chloro <- plant_sun_shade_data$Chlorophyll.Concentration[group_id] 
	petiole <- plant_sun_shade_data$Petiole.Length[group_id] 
	stomata <- plant_sun_shade_data$Stomata.Density[group_id] 

	# Observations
	obs <- colnames(plant_sun_shade_data)

	# Run tests
	mprint(">>> Running T-test for Absorvance... ")
	print(t.test(absorbance ~ exposure))

	mprint(">>> Running T-test for Chlorophyll.Concentration... ") 
	print(t.test(chloro ~ exposure))

	mprint(">>> Running T-test for Petiole Length... ") 
	print(t.test(petiole ~ exposure) )

	mprint(">>> Running T-test for Stomata Density... ") 
	print(t.test(stomata ~ exposure))


	# Observations
	obs <- colnames(plant_sun_shade_data)
	for (i in obs[2:length(obs)]) {
		mprint(paste(">>> Running T-test for ",i,"..."))
		print(t.test(data[,i] ~ data[,1]))
	}
}

##########################################

if (interactive()) {
	mprint(" >>> Available functions")
	cat( paste( ls()[grep("anal",ls())], collapse='\n' ) )
	cat("\n")
}

##########################################
