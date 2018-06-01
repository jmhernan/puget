# aws s3 cp s3://hild-datasets/ids_with_record_linkage_pids.csv .

# ==========================================================================
# Exploratory Data Analysis of linked PHA and HMIS data
# Data file: ids_with_record_linkage.csv
# Tim Thomas - t77@uw.edu
# Start Data: 20180512
# ==========================================================================
	rm(list=ls()) #reset
	# options(width = 78)

	library(colorout)
	library(lubridate)
	library(magrittr)
	library(tidyverse)

# ==========================================================================
# Data
# ==========================================================================

	links <- data.table::fread("data/HILD/ids_with_record_linkage_pids.csv")

	hmis <- data.table::fread("/home/ubuntu/data/HMIS/puget_preprocessed.csv") %>%
			mutate(pid0 = paste("HMIS0_",PersonalID,sep=""),
				   pid1 = paste0("HMIS1_",
		   		   				 stringr::str_pad(seq(1,nrow(.)),6,pad='0')))

	load("/home/ubuntu/data/Housing/OrganizedData/pha_longitudinal.Rdata")

	pha <- pha_longitudinal %>%
		   mutate(pid0 = paste("PHA0_",pid, sep = ""),
		   		  pid1 = paste0("PHA1_",
		   		  				stringr::str_pad(seq(1,nrow(.)),6,pad='0')))

# ==========================================================================
# Clean
# ==========================================================================
	hmis_c <- hmis %>%
			  group_by(HouseholdID) %>%
			  mutate(hh_ct = n()) %>%
			  ungroup() %>%
			  mutate(agency = "HMIS",
			  		 EntryDate = lubridate::ymd(EntryDate),
			  		 ExitDate = lubridate::ymd(ExitDate),
			  		 DOB = lubridate::ymd(DOB),
			  		 RelationshipToHoH = factor(RelationshipToHoH)) %>%
			  select(pid0,
			  		 hh_id = HouseholdID,
			  		 hh_ct,
			  		 relcode = RelationshipToHoH,
					 lname = LastName,
					 fname = FirstName,
					 mname = MiddleName,
					 dob = DOB,

					 entry = EntryDate,
					 exit = ExitDate,
					 agency,
					 proj_type = ProjectType)

	pha_c <- pha %>%
			 group_by(hhold_id_new) %>%
			 mutate(hh_ct = n()) %>%
			 ungroup() %>%
			 mutate(admit_date = lubridate::ymd(admit_date),
			 		max_date2 = lubridate::ymd(max_date2),
			 		dob = lubridate::ymd(dob),
			 		relcode = factor(relcode)) %>%
			 select(pid0,
			 		hh_id = hhold_id_new,
			 		hh_ct,
			 		relcode,
					lname = lname_new,
					fname = fname_new,
					mname = mname_new,
					dob,
					entry = startdate,
					exit = enddate,
					agency = agency_new,
					proj_type = prog_type)

	agency_df <- bind_rows(hmis_c,pha_c) %>%
				 left_join(., links, by = "pid0")

				 # agency_df <- agency_df %>%
				 # filter(!grepl("REFUSED",lname.x),
					# 	!grepl("REFUSED",fname.x),
					# 	!grepl("ANONYMOUS",lname.x),
					# 	!grepl("ANONYMOUS",fname.x))

	glimpse(agency_df)

# ==========================================================================
# Summary Statistics
# ==========================================================================

### Number of unique links found in the data.
	links %>%
	summarise(unique_ind = n_distinct(linkage_PID))
		# 195692 (unique links) - 229939 (unique pid0) = -34247

### Venn diagram of individuals in each program
	venn <- agency_df %>%
		   select(linkage_PID,agency) %>%
		   na.omit() %>%
		   distinct() %>%
		   group_by(linkage_PID) %>%
		   # filter(n()>1) %>%
		   # arrange(linkage_PID) %>%
		   tidyr::spread(agency, agency, fill = "") %>%
		   mutate(programs = trimws(paste(HMIS,KCHA,SHA, sep = " "))) %>%
		   mutate(programs = ifelse(programs == "HMIS  SHA", "HMIS SHA", programs)) %>%
		   ungroup() %>%
		   data.frame()

	glimpse(venn)

	# Venn diagram counts
	data.frame(table(venn$programs))


### order of agencies ###
	order <- agency_df %>%
			 mutate(exit2 = ifelse(is.na(exit), entry, exit),
			 		exit2 = as_date(exit2)) %>%
			 arrange(linkage_PID, entry, exit) %>%
			 select(linkage_PID, relcode, entry, exit, exit2, agency, proj_type) %>%
			 group_by(linkage_PID) %>%
			 distinct() %>% # there are several cases with dupe dates
			 mutate(prog.index = rle(agency) %>%
			 					 extract2("lengths") %>%
			 					 rep(seq_along(.), .)) %>%
			 filter(!is.na(linkage_PID)) %>%
			 mutate(prog.time = exit - entry,
		   			prog.diff.time = entry - lag(exit))

	heads <- order %>%
			 filter(relcode == "1" | relcode == "H")

	prog.order <- heads %>%
				  select(linkage_PID, prog.index, agency) %>%
				  distinct() %>%
				  group_by(linkage_PID) %>%
				  summarise(prog.order = paste0(agency, collapse = "."))


# ==========================================================================
# Checks
# ==========================================================================
	head(data.frame(order), 20)

	left_join(data.frame(table(order %>% ungroup() %>% filter(agency == "HMIS", !is.na(exit)) %>% select(proj_type))), data.frame(table(order %>% ungroup() %>% filter(agency == "HMIS", !is.na(exit)) %>% select(proj_type))), by = "Var1")

	# same date entrance into lagged group
	check <- heads %>%
			 mutate(lag.agency = lag(agency)) %>%
			 filter(entry == lag(entry) & is.na(exit) & lag.agency != "HMIS")

	data.frame(table(check$proj_type))
	data.frame(table(check$lag.agency))



# ==========================================================================
# LEFT OFF
# ==========================================================================

	gc()


	group_by(linkage_PID) %>%
	select(agency) %>%
	unique() %>%
	mutate(prog.order = paste0(agency, collapse = "."))


	order <- agency_df %>%
			arrange(linkage_PID, entry) %>%
			select(linkage_PID, agency) %>%
  			filter(!is.na(linkage_PID),
  				   !(linkage_PID == lead(linkage_PID) &
  				   agency == lead(agency)) |
  				   is.na(lead(linkage_PID))) %>%
  			group_by(linkage_PID) %>%
  			summarise(order = paste0(agency, collapse = ".")) %>%
  			left_join(agency_df,.) %>%
  			mutate(order = ifelse(is.na(order), paste("CENSORED", agency), order))
  	#######################################
  	##### THE ABOVE CAN BE CLEANED UP #####
  	#######################################
  	data.frame(table(order$order)) %>% arrange(desc(Freq))


### HMIS before PHA
  	# subset heads of households


	# Pre-HMIS
	prehmis <-
			heads %>%
			select(linkage_PID, entry, exit, agency, order) %>%
			group_by(linkage_PID) %>%
			filter(str_detect(order, "^HMIS"),
				   order != "HMIS",
				   linkage_PID == 6 |
				   linkage_PID == 17|
				   linkage_PID == 20) %>%
			arrange(linkage_PID,
					entry,exit) %>%
			group_by(linkage_PID) %>%
			mutate(index = rle(agency) %>%
						   extract2("lengths") %>%
						   rep(seq_along(.), .)) %>%
			group_by(linkage_PID, index) %>%
			mutate(first = first(entry),
				   last = last(exit)) %>%
			group_by(linkage_PID) %>%
			mutate(prog.time = exit - entry,
				   pre.diff = first - lag(last)) %>%
			group_by(linkage_PID, index) %>%
			filter(row_number()==1) %>%
			group_by(linkage_PID) %>%
			data.frame() %>%
			select(linkage_PID, entry, exit, agency, index, first:pre.diff)

### This only works for difference between differences in PHA

# ==========================================================================
# TESTBED
# 	test <-
# sum(2014-06-09,2014-08-15)


# 	surv %>%
# 			arrange(linkage_PID,entry) %>%
# 				filter(linkage_PID == 6) %>%

# 	# get times
# 	surv %>%
# 			arrange(linkage_PID,entry) %>%
# 				filter(linkage_PID == 6 | linkage_PID == 17 | linkage_PID == 20) %>%
# 			group_by(linkage_PID, agency) %>%
# 			mutate(program.time = exit - entry,
# 				   St = entry - lag(exit)) %>%
# 			data.frame()

# 			%>%
# 			group_by(agency) %>%
# 			summarise(diff = sum(program.time, na.rm = T) + sum(St, na.rm = T))
			# glimpse()
# ==========================================================================
    df <-
    data.table::fread(
        "ID entry 		exit       	agency
         6 	2014-06-09 	2014-06-27	HMIS
         6 	2014-06-19 	2014-07-05	HMIS
         6 	2014-07-02 	2014-08-15	HMIS
         6 	2014-07-02 	2014-08-17	SHA
         6 	2014-09-08 	2014-09-27	HMIS
         6 	2016-08-04 	2016-12-31	HMIS
        17 	2011-06-01 	      <NA>	HMIS
        17 	2011-06-20 	2013-06-09	KCHA
        17 	2013-06-10 	2016-05-31	KCHA
        17 	2016-06-01 	2017-06-01	KCHA
        20 	2012-08-17 	2013-08-01	HMIS
        20 	2013-02-18 	2013-04-14	HMIS
        20 	2013-08-02 	2013-08-03	SHA
        20 	2014-12-09 	2015-02-17	HMIS
        20 	2015-09-23 	2015-09-24	HMIS
        20 	2015-10-01 	2016-06-30	SHA
        20 	2016-02-03 	2016-04-10	HMIS"
    	) %>%
    mutate(entry = ymd(entry),
    	   exit = ymd(exit))

    # f <- rle(df$agency)
    # f$lengths
    # f$values



		# st = first(entry) - lag(exit))
	# Can't double group, it'll only calculate within the group
	# ,
	# 	   st = first(entry))



# ==========================================================================
# LEFT OFF HERE
# ==========================================================================

# hmis before pha
	prehmis <- left_join(agency_df, inp %>% select(linkage_PID,programs)) %>% data.frame()

	### check ###
	# prehmis %>% filter(linkage_PID == 1) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 3) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 5) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 6) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 7) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 16) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 17) %>% arrange(entry)
	# prehmis %>% filter(linkage_PID == 20) %>% arrange(entry)


	order <- prehmis %>%
			 filter(programs == "HMIS KCHA" |
		   		 	programs == "HMIS KCHA SHA" |
		   		 	programs == "HMIS SHA")

	first_last <- order %>%
			 select(linkage_PID, entry, agency) %>%
			 arrange(linkage_PID,entry) %>%
			 group_by(linkage_PID) %>%
			 summarize(first = dplyr::first(agency),
			  		 last = dplyr::last(agency),
			  		 diff = difftime(last(entry),
			  		 				 first(entry),
			  		 				 unit = "days")/365) %>%
			 mutate(order = paste(first,last,sep = "_"))

	head(first_last)

	table(first_last$first,first_last$last)
	round(prop.table(table(first_last$first,first_last$last)),2) * 100

	x2 <- chisq.test(first_last$first,first_last$last)
	x2$observed

	first_last.plot <- data.frame(table(first_last$first_last))

	ggplot(first_last, aes(x = first_last)) +
	geom_histogram(stat = "count") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Time between episodes
	order %>%
	group_by(linkage_PID) %>%
	arrange(agency) %>%
	mutate(agency.change=cumsum(c(1, diff(agency) != 0))) %>%
	glimpse()



# ==========================================================================
# Testbed

	test <- agency_df %>%
			select(linkage_PID, entry, exit, agency) %>%
			na.omit() %>%
			arrange(linkage_PID,entry,exit) %>%
			distinct()

	head(test,50)

# ==========================================================================

# ==========================================================================
# ==========================================================================
# ==========================================================================
#  LEFT OFF HERE
# ==========================================================================
# ==========================================================================
# ==========================================================================








	%>% data.frame %>% head(20)
	### model ###
	agency_df %>%
	# ungroup() %>%
	filter(programs == c("HMIS KCHA", "HMIS KCHA SHA", "HMIS SHA")) %>%
	# filter(linkage_PID == 16) %>%
	select(linkage_PID, entry, agency) %>%
	group_by(linkage_PID) %>%
	arrange(linkage_PID,entry) %>%
	# filter(row_number()==1 | row_number()==n()) %>% data.frame() %>% head(50)
	# ungroup() %>%
	# group_by(linkage_PID, entry) %>%
	# summarise(first = first(agency),
	# 		  last = last(agency),
	# 		  diff = difftime(last(entry), first(entry), unit = "days")/365) %>%

	mutate(first = first(agency),
			  last = last(agency),
			  diff = difftime(last(entry), first(entry), unit = "days")/365) %>%
	data.frame() %>%
	head(50)

	# df %>%
 #   group_by(event) %>%
 #   summarise(First = first(time),
 #             Last = last(time) ,
 #             difference= difftime(last(time), first(time), unit='hour'))



	group_by(linkage_PID) %>%
	mutate(in_sha = ifelse(agency == "SHA", 1, 0),
		   in_kcha = ifelse(agency == "KCHA", 1, 0),
		   in_hmis = ifelse(agency == "HMIS", 1, 0)) %>%
	mutate(programs = ifelse(in_sha==1 & in_kcha==1 & in_hmis==0, "sha_kcha",
					  ifelse(in_sha==1 & in_kcha==0 & in_hmis==1, "sha_hmis",
					  ifelse(in_sha==0 & in_kcha==1 & in_hmis==1, "kcha_hmis", NA))))





	links %>%
	select(linkage_PID, in_sha) %>%
	filter(in_sha == TRUE) %>%
	distinct() %>%
	dim()
		# 68459

	links %>%
	select(linkage_PID, in_kcha) %>%
	filter(in_kcha == TRUE) %>%
	distinct() %>%
	dim()
		# 84304

	links %>%
	select(linkage_PID, in_hmis) %>%
	filter(in_hmis == TRUE) %>%
	distinct() %>%
	dim()
		# 80231

	# program intersections

			 links %>%
			 # select(pid0, linkage_PID, in_sha) %>%
			 filter(in_sha == TRUE) %>%
			 glimpse()


# ==========================================================================
# Checks
# ==========================================================================

### check that pid1 is right ###
	hmis %>% filter(pid1 == "HMIS1_200000") %>% glimpse()
	links %>% filter(pid1 == "HMIS1_200000") %>% glimpse()
	hmis %>% filter(pid0 == "HMIS0_55213") %>% glimpse()
	links %>% filter(pid0 == "HMIS0_55213") %>% glimpse()
	# it's not, going to use pid0 to link old data

	check <- left_join(hmis,links, by = "pid0")

### number of individuals in the links ###
	links %>%
		select(linkage_PID) %>%
		distinct %>%
		summarise(n())
	# 203,727 unique individuals

### number of repeats ###
	links <- links %>%
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
	check2 <- links %>%
	filter(linkage_PID %in% checks) %>%
	arrange(linkage_PID) %>%
	data.frame() %>%
	select(pid0)

	# pid0's
	links %>%
		filter(pid0 %in% check2[,1]) %>% head(,3)

	links %>%
		filter(pid0 == "PHA0_30430")

	links %>%
	group_by(pid0) %>%
	filter(n()>1) %>%
	summarise(n())
	## there are 4,717 duplicate pid0's

# ==========================================================================
# Summary statistics
# ==========================================================================

### vin diagram ###
	# number of links


	intersects <- order %>%
	select(linkage_PID, in_sha:in_hmis) %>%
	# gather(program, value, in_sha:in_hmis) %>%
	group_by(linkage_PID) %>%
	mutate(programs = ifelse(in_sha==TRUE & in_kcha==TRUE & in_hmis==FALSE, "sha_kcha",
					  ifelse(in_sha==TRUE & in_kcha==FALSE & in_hmis==TRUE, "sha_hmis",
					  ifelse(in_sha==FALSE & in_kcha==TRUE & in_hmis==TRUE, "kcha_hmis", NA))))

	data.frame(table(intersects$programs)) %>%
	ungroup() %>%
	select(programs) %>%
	group_by(programs) %>%
	summarise(n())


	))

			  kcha_sha = n_distinct,
			  sha_hmis = ,
			  kcha_hmis =,
			  kcha_hmis_sha = )

