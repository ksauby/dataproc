#' Prepare Data For Transition Matrix Function
#'
#' @param Dat Dataset
#' @param SizeClass Classes into which to divide individuals based on size
#' @param TransitionYear Year
#' @param SeedSurvival Seed survival rate
#' @param SeedBankSize Number of seeds in the seed bank
#' @param SeedsPerFruit Number of seeds produced per fruit

#' @description Prepare Data For Transition Matrix Function
#'
#' @export

prepDataTransitionMatrix <- function(
	Dat,
	SizeClass,
	TransitionYear,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit
) {
	size.class <- cut(Dat$Size_t, SizeClass, include.lowest=T, labels=FALSE)
	Dat <- cbind(Dat, size.class)
	Dat_census <- Dat %>% 
		dplyr::select(
			SamplingYear, 
			PlantID, 
			Stage, 
			size.class, 
			Fruit_Flowers_t,
			Size_t,
			Island
		) %>%
		setnames("Stage", "stage")# %>%
		#setnames("size.class", "Size_t")
	# create stages based on sizes for adults 
	Sizes <- Dat_census[which(
		Dat_census$stage != "Dead" &
		Dat_census$stage != "Seedling" &
		Dat_census$stage != "Juvenile"
	), ]$size.class
	Dat_census[which(
		Dat_census$stage != "Dead" &
		Dat_census$stage != "Seedling" &
		Dat_census$stage != "Juvenile"
	), ]$stage <- Sizes
	Dat_census[which(Dat_census$stage == "Dead"), ]$stage <- "dead"
	Dat_census %<>% dplyr::select(-size.class)
	# merge year with year - 1
	trans <- subset(
		merge(
			Dat_census, 
			Dat_census, 
			by = "PlantID", 
			sort = FALSE
		), 
		SamplingYear.x == SamplingYear.y - 1
	)
	# rename rows and columns
	rownames(trans) <- 1:nrow(trans)
	colnames(trans)[2:10] <- c(
		"Year", 
		"stage",
		"Repro", 
		"Size",
		"Island",
		"Year2", 
		"fate",
		"Repro2",
		"Size2"
	)
	# year-specific transition matrix
	trans01 <- subset(trans, Year == TransitionYear, c(PlantID, stage, Repro, fate, Size, Size2, Island, Repro2))
	seedlings <- nrow(subset(
		Dat_census, 
		SamplingYear == TransitionYear & stage =="Seedling"
	))
	# trans01 %<>% filter(stage != "Dead")
	# number of seedlings estimated to have been produced by each stage class
	trans01$Seedling <- trans01$Repro/sum(trans01$Repro, na.rm=T) * seedlings
	# estimate seed to seedling transition
	Seedlings <- nrow(subset(trans, Year == TransitionYear & stage =="Seedling"))
	seeds.from.plants <- sum(trans01$Repro) * SeedsPerFruit
	recruitment.rate <- Seedlings/(SeedBankSize + seeds.from.plants)
	trans01$Seedling <- trans01$Repro/sum(trans01$Repro) * 
		seeds.from.plants * recruitment.rate
	trans01$Seed <- trans01$Repro * SeedsPerFruit * SeedSurvival
	# Create ordered list of stages
	stages <- c(unique(c(trans01$stage, trans01$fate))) %>% 
		as.numeric %>%
		na.omit %>% 
		as.vector %>%
		sort
	stages <- c("Seed", "Seedling", "Juvenile", stages, "dead")
	trans01$stage <- factor(trans01$stage, levels=stages, ordered=T)
	trans01$fate <- factor(trans01$fate, levels=stages, ordered=T)
	return(list(trans01, recruitment.rate, stages))
}