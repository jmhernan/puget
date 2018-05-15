# aws s3 cp s3://hild-datasets/ids_with_record_linkage_pids.csv .

# ==========================================================================
# Exploratory Data Analysis of linked PHA and HMIS data
# Data file: ids_with_record_linkage.csv
# Tim Thomas - t77@uw.edu
# Start Data: 20180512
# ==========================================================================

	options(width = 90)
	library(colorout)
	library(tidyverse)

# ==========================================================================
# Data
# ==========================================================================

	linkage <- data.table::fread("data/HILD/ids_with_record_linkage_pids.csv")

	hmis <- data.table::fread("/home/ubuntu/data/HMIS/puget_preprocessed.csv") %>%
			mutate(pid0 = paste("HMIS0_",PersonalID,sep=""))

	load("/home/ubuntu/data/Housing/OrganizedData/pha_longitudinal.Rdata")

	pha <- pha_longitudinal %>%
		   mutate(pid0 = paste("PHA0_",pid, sep = ""))

	hild <- bind_rows()

# ==========================================================================
# Checks
# ==========================================================================

### number of individuals in the linkage ###
	linkage %>%
		select(linkage_PID) %>%
		distinct %>%
		summarise(n())
	# 203,727 unique individuals

### number of repeats ###
	links <- linkage %>%
			group_by(linkage_PID) %>%
			filter(n() >1) %>%
			arrange(linkage_PID)
	# 58,880 links

### Spot check linkages()
	links %>%
	select(ssn:mname, linkage_PID) %>%
	arrange(fname, lname, mname) %>%
	data.frame()

	# List of linkage_PID that might be problematic based on spot checks
	checks <- c(19984, #
				26684, #
				3365,
				20228,
				10855,
				11350,
				3769,
				9379,
				27300)

	# pid0's that are located in the above linkage_PID vector
	check2 <- linkage %>%
	filter(linkage_PID %in% checks) %>%
	arrange(linkage_PID) %>%
	data.frame() %>%
	select(pid0)

	# pid0's
	linkage %>%
		filter(pid0 %in% check2[,1]) %>% head(,3)

	linkage %>%
		filter(pid0 == "PHA0_30430")

	linkage %>%
	group_by(pid0) %>%
	filter(n()>1) %>%
	summarise(n())
	## there are 4,717 duplicate pid0's

# ==========================================================================
# Summary statistics
# ==========================================================================

