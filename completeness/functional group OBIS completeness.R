#################################
# Estimating and visualising completeness of OBIS data by region and functional group

# Tom Webb, t.j.webb@sheffield.ac.uk, Aug-Oct 2016

#################################

# The packages we use development versions for need to be installed like this:
devtools::install_github("ropensci/taxizesoap")
# If you have problems installing taxizesoap see https://github.com/ropensci/taxizesoap

# load required packages:
library(taxizesoap)
# devtools::install_github("iobis/robis")
# library(robis) # Only needed if you want to extract records from OBIS directly
library(ggplot2)
library(dplyr)
library(readr)

# You can get records for Belgium directly from OBIS using the robis package:
# belgian_recs <- occurrence(geometry = mr_as_wkt(mr_shp(name = "Belgian Exclusive Economic Zone")))
# Note that the resulting dataset is not identically structured to the one used in this code and so the code below is unlikely to work without editing it

# However here we use data obtained from the following link http://iobis.org/explore/#/area/21 and saved locally as a csv file
belgian_recs <- tbl_df(read_csv("belgian_obis_recs.csv.zip"))
# There may be some parsing errors, but none of these should affect variables we need here

# Check some details - quick look at the data:
glimpse(belgian_recs)
# Check its size
nrow(belgian_recs)
# 135223

# Check taxonomic ranks of taxa included
table(belgian_recs$taxonrank)
# Family      Genus    Species Subspecies 
#     10        127       1846          5 


# select relevant columns
belgian_recs <- dplyr::select(belgian_recs, id:qc, yearcollected, tname:worms_id, eventdate)

# get list of unique taxa
belgian_aphia <- unique(belgian_recs$worms_id)
length(belgian_aphia)
# 3936 taxa in this dataset

# Add full WoRMS classification and other info to this taxon list
# WARNING - this will take some time (around 10 minutes on my machine) and you can choose to rely on the
# classification within the OBIS dataset if you prefer
belgian_taxa <- tbl_df(worms_records(ids = belgian_aphia))

# restrict to species (or lower)
table(belgian_taxa$rank)
belgian_species <- subset(belgian_taxa, rank == "Species" | rank == "Subspecies" | rank == "Variety" | rank == "Forma")
nrow(belgian_species)
# 3197

# This is a function to identify species (by WoRMS AphiaID) as 'fish' in the broad sense, i.e. as members of the group Pisces. This is not a monophyletic group (includes bony fish and elasmobranchs) but is a useful functional group as all fish tend to be sampled together
get_pisces <- function(aphiaid){
	# get hierarchy for a given aphia ID
	if(exists("aphia_h")){rm(aphia_h)}
	pisces <- FALSE
	try(aphia_h <- worms_hierarchy(ids = aphiaid), silent = T)
	if(exists("aphia_h")){
		if("Pisces" %in% aphia_h$scientificname){
			pisces <- TRUE
		}
	}
	return(pisces)	
}

# Example running for the first species in the list:
get_pisces(belgian_species$AphiaID[1])

# Run for all species
# WARNING - this is slow as implemented here, takes ~10 minutes to run
belgian_species$pisces <- with(belgian_species,
	sapply(valid_AphiaID, function(valid_AphiaID){get_pisces(valid_AphiaID)}))
# Check
table(belgian_species$pisces)
# FALSE  TRUE 
#  3036   161 

# Other taxonomic group IDs to add to a new 'tax_group' variable are bird, mammal, reptile
belgian_species$tax_group <- NA
belgian_species$tax_group[belgian_species$pisces == T] <- "fish"
belgian_species$tax_group[belgian_species$class == "Aves"] <- "birds"
belgian_species$tax_group[belgian_species$class == "Mammalia"] <- "mammals"
belgian_species$tax_group[belgian_species$class == "Reptilia"] <- "reptiles"
table(belgian_species$tax_group)

#    birds     fish  mammals reptiles 
#       57      161       17        3 

# Add functional groups, using data from the WoRMS traits portal (http://www.marinespecies.org/traits/). At present these are not available via webservices, here we use a local copy obtained directly from WoRMS

# Read in the full dataset
worms_func_group <- read_tsv("WoRMS_biota_functional_group_2016-08-08.txt.zip")
# There are a few parsing errors but none that need concern us here
nrow(worms_func_group)
# 351446
# Restrict to adult life stage
worms_func_group <- subset(worms_func_group, stage == "adult")
nrow(worms_func_group)
# 176493
# NB - there is a formatting issue with the name of the first variable, correct that here:
names(worms_func_group)[1] <- "AphiaID"
# Check the classifications
table(worms_func_group$functional_group)
#       benthos        nekton phytoplankton      plankton   zooplankton 
#        146098          2776         16057            13         11549 

# Check how many of our belgian species are present in this list
sum(belgian_species$valid_AphiaID %in% worms_func_group$AphiaID)
#1508

# select relevant columns
worms_func <- dplyr::select(worms_func_group, AphiaID, functional_group)
worms_func$AphiaID <- as.character(worms_func$AphiaID)
# join into belgian_species dataset
belgian_species <- left_join(x = belgian_species, y = worms_func, by = c("valid_AphiaID" = "AphiaID"))

# combine the existing taxonomic groups with the new functional groups
belgian_species$group <- belgian_species$tax_group
belgian_species$group[is.na(belgian_species$tax_group)] <-
	belgian_species$functional_group[is.na(belgian_species$tax_group)]

table(belgian_species$group)
#       benthos         birds          fish       mammals        nekton phytoplankton 
#          1053            57           161            17            13           348 
#      reptiles   zooplankton 
#             3            94 

sum(is.na(belgian_species$group))
# 1451
sum(!is.na(belgian_species$group))
# 1746
sum(!is.na(belgian_species$group)) / nrow(belgian_species)
# So we have classified 55% of Belgian species to a relevant taxonomic or functional group

# Check the Classes and phyla of those missing a classification
with(subset(belgian_species, is.na(group)), table(class))
with(subset(belgian_species, is.na(group)), table(phylum))

# Some of these can be easily filled in, e.g. Macrophytes:
belgian_species$group[belgian_species$phylum == "Chlorophyta" | belgian_species$phylum == "Rhodophyta"] <- "macrophytes"
# Possible Ochrophyta could go there too - but some may be phytoplankton…
data.frame(dplyr::select(subset(belgian_species, phylum == "Ochrophyta"), class, functional_group))
# class Bacillariophyceae = diatoms, phytoplankton
# class Phaeophyceae = brown algae, macrophytes
# class Dictyochophyceae = phytoplankton
belgian_species$group[
	belgian_species$class == "Bacillariophyceae" | belgian_species$class == "Dictyochophyceae"] <- "phytoplankton"
belgian_species$group[
	belgian_species$class == "Phaeophyceae"] <- "macrophytes"

with(subset(belgian_species, is.na(group)), table(phylum))
# This process could be continued to fill in further gaps should the user wish

# For now - we work on this list.
# Next step: add group -> full OBIS table
belgian_recs$AphiaID <- as.character(belgian_recs$worms_id)
belgian_recs <- left_join(belgian_recs, belgian_species, by = "AphiaID")

table(belgian_recs$group)
#       benthos         birds          fish   macrophytes       mammals        nekton 
#         34663          1920         21420           281            80           555 
# phytoplankton      reptiles   zooplankton 
#          5728             4          3394 

# new dataset restricted to records with an associated group
belgian_group_recs <- subset(belgian_recs, !is.na(group))
nrow(belgian_group_recs)
# 68045

# In order to calculate Chao index of completeness, we need to further restrict the data to records including year. This excludes many records for some groups (e.g. birds) and all for others (e.g. macrophytes)
with(belgian_group_recs, sum(is.na(yearcollected)))
# 4253
belgian_group_recs <- subset(belgian_group_recs, !is.na(yearcollected))

# get Chao completeness by group - this function calculates the Choa2 index
calcChao2 <- function(df = belgian_group_recs, fgroup = NULL){ # sample-based estimator of species richness of Chao
	
	
	if(!is.null(fgroup)){df <- subset(df, group == fgroup)}
	
	df$n <- 1
	df <- aggregate(df$n, c(df["valid_AphiaID"], df["yearcollected"]), sum)
	taxa <- as.data.frame(table(df["valid_AphiaID"]))
	names(taxa) <- c("taxon","nYears")
	nTaxa <- length(taxa$taxon)
	
	years <- as.data.frame(table(df["yearcollected"]))
	names(years) <- c("year","nTaxa")
	nYears <- length(years$year)
	
	q1 <- length(subset(taxa, nYears==1)$taxon)
	q2 <- length(subset(taxa, nYears==2)$taxon)
	chao2u <- nTaxa+((nYears-1)/nYears)*(q1*(q1-1)/(2*(q2+1)))
	if(q2>0){
		chao2 <- chao2u
		vChao2 <- q2*((q1/q2)^2/2+(q1/q2)^3+(q1/q2)^4/4)
		h <- (nYears-1)/nYears
		vChao2u <- h*q1*(q1-1)/(2*(q2+1))+h^2*q1*(2*q1-1)^2/(4*(q2+1)^2)+h^2*q1^2*q2*(q1-1)^2/(4*(q2+1)^4)
	} else {
		chao2 <- nTaxa+q1^2/(2*q2)
		h <- (nYears-1)/nYears
		vChao2u <- h*q1*(q1-1)/2+h^2*q1*(2*q1-1)^2/4+h^2*q1^2*q2*(q1-1)^2/(4*chao2u)
		vChao2 <- vChao2u
	}
	
	tChao2 <- chao2-nTaxa
	kChao2 <- exp(1.96*log(1+vChao2u/tChao2^2)^0.5)
	lChao2 <- nTaxa+tChao2/kChao2
	uChao2 <- nTaxa+tChao2*kChao2
	
	#returns:
	# chao2 estimator of species richness (sample-based), 
	# chao2 unbiased, standard deviation, std unbiased, 
	# lower and upper 95% confidence intervals (same for biased and unbiased)
	chao2stats <- c(chao2, chao2u, vChao2^0.5, vChao2u^0.5, lChao2, uChao2)
	return(chao2stats)
	
} # end function calcChao2

# example - get stats for zooplankton
calcChao2(df = belgian_group_recs, fgroup = "zooplankton")

# get list of functional groups
fgroups <- unique(belgian_group_recs$group)

# Get Chao2 stats for each group
group_chao <- t(sapply(fgroups, function(fgroups){calcChao2(belgian_group_recs, fgroup = fgroups)}))
# Format them nicely
group_chao <- as.data.frame(group_chao)
names(group_chao) <- c("chao2", "chao2u", "vChao2_0.5", "vChao2u_0.5", "lChao2", "uChao2")
group_chao$group <- rownames(group_chao)
group_chao <- tbl_df(group_chao)

# Number of species recorded per group per year
by_group_yr <- group_by(belgian_group_recs, group, yearcollected)
sp_by_group_yr <- summarise(by_group_yr,
	species = n_distinct(valid_AphiaID)
	)
# Add in Chao2 results
sp_by_group_yr <- left_join(sp_by_group_yr, group_chao, by = "group")
# Get 'completeness' - observed species / predicted species
sp_by_group_yr$p_chao2u <- with(sp_by_group_yr, species / chao2u)

# get cumulative number of species
yrs <- sort(unique(belgian_group_recs$yearcollected))
group_yr_cumsum <- data.frame(group = numeric(0), cum_species = numeric(0), year = numeric(0))
for(yr in yrs){
		df <- subset(belgian_group_recs, yearcollected <= yr)
		df <- group_by(df, group)
		df <- summarise(df, cum_species = n_distinct(valid_AphiaID))
		df$year <- yr
		group_yr_cumsum <- rbind(group_yr_cumsum, df)
}
rm(df, yr)
sp_by_group_yr$grp_yr <- with(sp_by_group_yr, paste(group, yearcollected, sep = "_"))
group_yr_cumsum$grp_yr <- with(group_yr_cumsum, paste(group, year, sep = "_"))

# join to main results
sp_by_group_yr <- left_join(sp_by_group_yr,
	dplyr::select(group_yr_cumsum, cum_species, grp_yr), by = "grp_yr")
rm(group_yr_cumsum)

# Proportion of estimated species observed by each year
sp_by_group_yr$cum_p_chao2u <- with(sp_by_group_yr, cum_species / chao2u)
sp_by_group_yr <- rename(sp_by_group_yr, year = yearcollected)

# for now - remove reptiles (only one species in the dataset, no useful information)
sp_by_group_yr <- subset(sp_by_group_yr, group != "reptiles")

# to get the ribbon to work nicely on the plots, need to add null values for first and last years for each group

# fish
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[1]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year[1] <- 1903
sub_group$species[1] <- 0
sub_group$grp_yr[1] <- "fish_1903"
sub_group$cum_species[1] <- 0
sub_group <- sub_group[1,]

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# benthos
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[2]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year[2] <- 2014
sub_group$species[2] <- 0
sub_group$grp_yr[2] <- "benthos_2014"
sub_group <- sub_group[2,]

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# zooplankton
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[3]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year[2] <- 2014
sub_group$species[2] <- 0
sub_group$grp_yr[2] <- "zooplankton_2014"
sub_group <- sub_group[2,]

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# birds
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[4]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year <- c(1903, 2014)
sub_group$species <- c(0, 0)
sub_group$grp_yr <- c("birds_1903", "birds_2014")
sub_group$cum_species <- c(0, 29)

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# mammals
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[5]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$species <- c(0, 0)
sub_group$grp_yr <- c("mammals_1903", "mammals_2014")
sub_group$cum_species <- c(0, 2)

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# phytoplankton
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[6]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year[2] <- 2014
sub_group$species[2] <- 0
sub_group$grp_yr[2] <- "phytoplankton_2014"
sub_group <- sub_group[2,]

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# nekton
sub_group <- arrange(subset(sp_by_group_yr, group == fgroups[7]), year)
sub_group <- sub_group[c(1, nrow(sub_group)), ]
sub_group$year <- c(1903, 2014)
sub_group$species <- c(0, 0)
sub_group$grp_yr <- c("nekton_1903", "nekton_2014")
sub_group$cum_species <- c(0, 6)

sp_by_group_yr <- bind_rows(sp_by_group_yr, sub_group)

# Plot of cumulative species over time for each group (blue line), with species per year (grey bars), Chao2 estimated richness (orange line) and lower-upper limits (orange box)
(cumulative_species_group_yr <- ggplot(sp_by_group_yr, aes(x = year, y = cum_species)) +
	geom_bar(aes(y = species), stat = "identity", alpha = 1/2) +
	geom_ribbon(aes(ymin = lChao2, ymax = uChao2), fill = "darkorange", alpha = 1/5) +
	geom_segment(aes(x = 1903, xend = 2014, y = chao2u, yend = chao2u), colour = "darkorange") +
	geom_line(colour = "steelblue", size = 1) +
	facet_wrap(~group, ncol = 1, scales = "free_y") +
	xlim(1900, 2015)
)

# Save figure as pdf and png
ggsave("cumulative_species_group_year.pdf", cumulative_species_group_yr)
ggsave("cumulative_species_group_year.png", cumulative_species_group_yr)
