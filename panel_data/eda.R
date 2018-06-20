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

	# ==========================================================================
	# TEMP
	# ==========================================================================

	# load("data/Housing/temp/180615_temp.RData")

	links <- data.table::fread("data/HILD/ids_with_record_linkage_pids.csv")
		# Linked data between PHA and HMIS made by Ariel
		# You can find on the S3 bucket hild-datasets

	hmis <- data.table::fread("/home/ubuntu/data/HMIS/puget_preprocessed.csv") %>%
			mutate(pid0 = paste("HMIS0_",PersonalID,sep=""),
				   pid1 = paste0("HMIS1_",
		   		   				 stringr::str_pad(seq(1,nrow(.)),6,pad='0')))
		# Located in S# bucket kcdhs/HMIS

	load("/home/ubuntu/data/Housing/OrganizedData/pha_longitudinal.Rdata")
		# located in hild-datasets/pha_longitudinal.csv

	pha <- pha_longitudinal %>%
		   mutate(pid0 = paste("PHA0_",pid, sep = ""), # pid comes from PHA, then pid0 is an id for our purposes
		   		  pid1 = paste0("PHA1_",
		   		  				stringr::str_pad(seq(1,nrow(.)),6,pad='0')))

	#=======================================================================
	# Codebook: PID's and SSN's
	# pid0 = personal ID from pha and hmis - Alastair's linkage and HMIS ID's
	# pid1 = generated pid by Tim within pha and hmis - unique id for each row
	# pid2 = generated pid by Tim after df merge - unique id for each row
	# ssn_dq =
	#	 1 = 9-digit ssn or HMIS dq == 1
	#	 2 = less than 9 digits or HMIS dq == 2
	#    3 = NA, all same digit, or HMIS dq == 3
	# ssn  = original ssn
	# ssn1 = ssn quality == 1 and 9 digits
	# dob1 = dob that is not in the list of very frequent 1/1 dates
	#=======================================================================

# ==========================================================================
# Clean - foundation that can be built
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
					 prog_type = ProjectType)

	pha_c <- pha %>%
			 group_by(hhold_id_new) %>%
			 mutate(hh_ct = n()) %>%
			 ungroup() %>%
			 mutate(startdate = lubridate::ymd(startdate),
			 		enddate = lubridate::ymd(enddate),
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
					prog_type = prog_type)

	agency_df <- bind_rows(hmis_c,pha_c) %>%
				 left_join(., links %>%
				 			  select(pid0, pid1, pid2, linkage_PID), by = "pid0")

	gc()
# ==========================================================================
# Summary Statistics
# ==========================================================================

#
#  Number of unique links found in the data.
#
	links %>%
	summarise(unique_ind = n_distinct(linkage_PID))
		# 195692 (unique links) - 229939 (unique pid0) = -34247

#
#  Venn diagram of individuals in each program
#
	venn <- agency_df %>%
		   select(linkage_PID,agency) %>%
		   na.omit() %>%
		   distinct() %>%
		   group_by(linkage_PID) %>%
		   tidyr::spread(agency, agency, fill = "") %>%
		   mutate(programs = trimws(paste(HMIS,KCHA,SHA, sep = " "))) %>%
		   mutate(programs = ifelse(programs == "HMIS  SHA", "HMIS SHA", programs)) %>%
		   ungroup() %>%
		   data.frame()

	glimpse(venn)

	# Venn diagram counts
	data.frame(table(venn$programs))

#
# order of agencies ###
#
	gc()
	# ==========================================================================
	# JOSE: think about better ways to improve the below time_to_next_pr, and intersect. How can we improve our understanding of overlap and gaps in events
	# ==========================================================================
	order1 <- agency_df %>%
			 mutate(exit2 = ifelse(is.na(exit), entry, exit),
			 		exit2 = as_date(exit2)) %>%
			 	# !!! Relook at exit2 to include the 90 day situation mentioned by Stephanie
			 arrange(linkage_PID, entry, exit) %>%
			 select(linkage_PID, relcode, entry, exit, exit2, agency, prog_type) %>%
			 group_by(linkage_PID) %>%
			 distinct() %>% # there are several cases with dupe dates
			 mutate(prog_index = rle(agency) %>%
			 					 extract2("lengths") %>%
			 					 rep(seq_along(.), .)) # index changes when there's an agency change
	order2 <- order1 %>%
			 filter(!is.na(linkage_PID)) %>%
			 mutate(in_prog_time = exit - entry, # needs work
					agency_trans = ifelse(is.na(lead(agency)),
										agency,
										paste(agency, lead(agency), sep = " to ")))

	### THIS TAKES A LONG TIME ###
	system.time(
	order3 <- order2 %>%
		   	 mutate(time_to_next_pr = lead(entry) - exit,
		   		 	lag_intersect = day(as.period(
		   		 							lubridate::intersect(
		   		 								interval(entry, exit),
		   		 								interval(lag(entry),lag(exit))))),
		   	 		lead_intersect = day(as.period(
		   		 							lubridate::intersect(
			   		 							interval(entry, exit),
			   		 							interval(lead(entry),lead(exit))))))
		   	 )
		#!!! Maybe clean up the understanding of

	# ==========================================================================
	# JOSE: What other scenerios are we missing in the below code?
	# ==========================================================================

	### !!!
	# TODO (below):
	# agency_trans - when end date is NA, or there is an end data, define
	# 	   	 	   whether person is still in the program or if it ended. this gets at stephanys comments
	###

	gc() # memory dump

	# if you want to work with a subset, un-comment the next three comments

	# lin <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
system.time(
	overlap <- order3 %>%
					# filter(linkage_PID %in% lin) %>%
					# arrange(linkage_PID, entry, exit) %>%
			   group_by(linkage_PID) %>%
			   mutate(overlap = int_overlaps(interval(entry,exit),
											 interval(lead(entry),
										 			  lead(exit))),
				   	  align = int_aligns(interval(entry,exit),
										 interval(lead(entry),
										 		  lead(exit))),
				   	  lag = ifelse(!is.na(lead_intersect) &
			   					   !is.na(exit) & # is current exit a 1-day?
			   					   !is.na(lead(exit)) &
 			   		  			   entry <= lead(entry) &
			   		  			   exit <= lead(exit) &
			   		  			   exit >= lead(entry), 1, 0),
			   		  mid = ifelse(!is.na(lag(lead_intersect)) &
			   		  			   !is.na(exit) &
			   		  			   !is.na(lag(exit)) &
			   		  			   !is.na(lag(entry)) &
			   		  			   # !is.na(lead(entry)) &
			   		  			   lag(entry) <= entry &
			   		  			   lag(exit) >= exit, 1,0),
			   		  lead = ifelse(!is.na(exit) &
			   		  				!is.na(lag(entry)) & # is this the first row for this person
			   		  				!is.na(lag(exit)) & # is prior a 1-day
 			   		  				entry >= lag(entry) &
			   		  		 		exit >= lag(exit) &
			   		  		 		entry <= lag(exit), 1, 0),
			   		  open = ifelse(is.na(exit),1,0),
			   		  over = ifelse(lag == 1 |
			   		  				# lead == 1 |
			   		  				open == 1, 0,
			   		  		 ifelse(!is.na(lead(entry)) & # last row for the person
			   		  				lead(mid)==1, 1,
			   		  		 ifelse(!is.na(lead(entry)) &
			   		  		 		lead(open == 1) &
			   		  		 		lead(entry) %within% interval(entry,exit), 1,
			   		  		 ifelse(!is.na(lead(entry)) &
			   		  		 		!is.na(lag(entry)) &
			   		  		 		lead(entry) %within% interval(entry,exit) &
			   		  		 		open == 0 &
			   		  		 		entry >= lag(entry), 1, 0)))),
			   		  	# Over = when over mid or over opens
			   		  solo = ifelse(is.na(lead_intersect) &
			   		  				lag == 0 &
			   		  				mid == 0 &
			   		  				lead == 0 &
			   		  				over == 0 &
			   		  				open == 0, 1, 0)) %>%
			   ungroup() %>%
			   rename(agency_trans = prog_trans,
			   		  prog_type = proj_type)
	)


	agency_order <- order3 %>%
				  select(linkage_PID, prog_index, agency) %>%
				  distinct() %>%
				  group_by(linkage_PID) %>%
				  summarise(agency_order = paste0(agency, collapse = "."))

	# for the purpose of maths, adding lag and lead program
	# and agency information to create "transition"
	order <- left_join(overlap,agency_order) %>%
			 group_by(linkage_PID) %>%
			 mutate(agency_trans = ifelse(row_number()==n(), agency, agency_trans),
					lag_agency = lag(agency),
					lag_prog = lag(prog_type),
					lead_agency = lead(agency),
					lead_prog = lead(prog_type),
					transition = paste0(agency, "::", prog_type, " --> ",lead_agency, "::", lead_prog))

	# overlap %>% filter(linkage_PID %in% lin) %>% arrange(linkage_PID, entry, exit) %>% data.frame() %>% head(20)

# ==========================================================================
# More descriptives
# ==========================================================================

#
# Program Frequency
#

	prog.freq <- data.frame(table(agency_order$agency_order))

	heads <- order %>%
			 filter(relcode == "1" | relcode == "H") %>%
			 ungroup()

	head.prog.freq <- heads %>%
					  select(linkage_PID, agency_order) %>%
					  distinct()

	head.prog.freq <- data.frame(table(head.prog.freq$agency_order))

	ind.head.prog <- left_join(prog.freq,head.prog.freq, by = "Var1") %>%
					 rename(Programs = Var1, Ind_Freq = Freq.x, HH_Freq = Freq.y)

#
# Households with HMIS services before PHA
#

	pre_hmis <- heads %>%
				filter(startsWith(agency_order, "HMIS."))

	# proportion of households that recieved HMIS services before PHA
	tot.heads <- heads %>%
				 select(linkage_PID) %>%
				 distinct() %>%
				 summarise(n())
	hmis.pha <- pre_hmis %>%
				select(linkage_PID) %>%
				distinct() %>%
				summarise(n())

	hmis.sha <- heads %>%
				filter(startsWith(agency_order, "HMIS.SHA")) %>%
				select(linkage_PID) %>%
				distinct() %>%
				summarise(n())

	hmis.kcha <- heads %>%
				filter(startsWith(agency_order, "HMIS.KCHA")) %>%
				select(linkage_PID) %>%
				distinct() %>%
				summarise(n())

	hmis.pha/tot.heads
	hmis.sha/tot.heads
	hmis.kcha/tot.heads

#
# Households with HMIS services after PHA
#

	# proportion of households that recieved HMIS services after PHA

	post.hmis <- heads %>%
				 filter(endsWith(agency_order, ".HMIS"))

	pha.hmis <- post.hmis %>%
				select(linkage_PID) %>%
				distinct() %>%
				summarise(n())

	sha.hmis <- heads %>%
				filter(endsWith(agency_order, "SHA.HMIS")) %>%
				select(linkage_PID) %>%
				distinct() %>%
				summarise(n())

	kcha.hmis <- heads %>%
				 filter(endsWith(agency_order, "KCHA.HMIS")) %>%
				 select(linkage_PID) %>%
				 distinct() %>%
				 summarise(n())

	pha.hmis/tot.heads
	sha.hmis/tot.heads
	kcha.hmis/tot.heads

# ==========================================================================
# By program type
# ==========================================================================
	heads2 <- heads

### HMIS to PHA ###
	hmis_pha <- heads2 %>%
	filter(agency_trans == "HMIS to SHA" | agency_trans == "HMIS to KCHA") %>%
	data.frame()

	hmis_pha_freq <- data.frame(table(hmis_pha$transition)) %>%
					 arrange(desc(Freq)) %>%
					 left_join(.,
					 			heads2 %>%
					 			ungroup() %>%
					 			group_by(transition) %>%
					 			summarise(mean_t_trans = mean(time_to_next_pr, na.rm = T),
					 					  median_t_trans = median(time_to_next_pr, na.rm = T)),
					 			by = c("Var1" = "transition"))

### PHA to HMIS ###
	pha_hmis <- heads2 %>%
				filter(agency_trans == "SHA to HMIS" | agency_trans == "KCHA to HMIS") %>%
				data.frame()

	pha_hmis_freq <- data.frame(table(pha_hmis$transition)) %>%
					 arrange(desc(Freq)) %>%
					 left_join(.,
					 			heads2 %>%
					 			ungroup() %>%
					 			group_by(transition) %>%
					 			summarise(mean_t_trans = mean(time_to_next_pr, na.rm = T),
					 					  median_t_trans = median(time_to_next_pr, na.rm = T)),
					 			by = c("Var1" = "transition"))
	write.csv()

### frequency of HMIS services from PHA ###
	data.frame(table(pha_hmis$lead_prog)) %>% arrange(desc(Freq))

### frequency of PHA services from HMIS ###
	data.frame(table(hmis_pha$lead_prog)) %>% arrange(desc(Freq))

# ==========================================================================
# ***** END WORKING CODE *****
# ==========================================================================

# ==========================================================================
# Time between programs
# ==========================================================================

	# Time lapse between HMIS to PHA

	head(data.frame(pre_hmis %>% select(-prog_type)), 50)

	time <- heads %>% select(-V1) %>%
			group_by(linkage_PID, prog_index) %>%
			filter(row_number()==n())

	order %>%
	ungroup() %>%
	summarise_at(vars(one_day:lead_overlap), funs(sum),na.rm=T)


	# HMIS.PHA
	time %>%
	filter(agency_trans == "HMIS to SHA" |
		   agency_trans == "HMIS to KCHA" &
		   solo == 1) %>%
	select(time_to_next_pr) %>%
	summary()

	ggplot(data = plot, aes(x = time_to_next_pr)) +
	geom_histogram(stat = "count")



	sum(heads$mid_prog, na.rm = T) # there are 3254 cases of mid
								   # programs different from lagged programs

	data.frame(table(heads %>% filter(mid_prog ==1) %>% select(agency_order))) %>% arrange(Freq)

	# Time laps for people that didn't have mid program events.

	time %>%
	filter(agency_trans == "HMIS to SHA") %>%
	summary()

	ggplot(data = time %>% filter(agency_trans == "HMIS to SHA"),
		   aes(time_to_next_pr)) +
	geom_histogram(stat = "count")

	time %>%
	filter(time_to_next_pr < -1000)
	head(time)

# ==========================================================================
# Checks
# ==========================================================================
	head(data.frame(order), 20)

	left_join(data.frame(table(order %>% ungroup() %>% filter(agency == "HMIS", !is.na(exit)) %>% select(prog_type))), data.frame(table(order %>% ungroup() %>% filter(agency == "HMIS", !is.na(exit)) %>% select(prog_type))), by = "Var1")

	# same date entrance into lagged group
	check <- heads %>%
			 mutate(lag.agency = lag(agency)) %>%
			 filter(entry == lag(entry) & is.na(exit) & lag.agency != "HMIS")

	peeps <- (check %>%
			 select(linkage_PID) %>%
			 distinct())$linkage_PID


	order %>%
	filter(linkage_PID %in% peeps) %>%
	data.frame

	data.frame(table(check$prog_type))
	data.frame(table(check$lag.agency))

	peeps <- order %>%
			 filter(entry == lag(entry) & is.na(exit) & lag.agency != "HMIS") %>%
			 select(linkage_PID)


	gc()


	group_by(linkage_PID) %>%
	select(agency) %>%
	unique() %>%
	mutate(agency_order = paste0(agency, collapse = "."))


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


# ### HMIS before PHA
#   	# subset heads of households


# 	# Pre-HMIS
# 	prehmis <-
# 			heads %>%
# 			select(linkage_PID, entry, exit, agency, order) %>%
# 			group_by(linkage_PID) %>%
# 			filter(str_detect(order, "^HMIS"),
# 				   order != "HMIS",
# 				   linkage_PID == 6 |
# 				   linkage_PID == 17|
# 				   linkage_PID == 20) %>%
# 			arrange(linkage_PID,
# 					entry,exit) %>%
# 			group_by(linkage_PID) %>%
# 			mutate(index = rle(agency) %>%
# 						   extract2("lengths") %>%
# 						   rep(seq_along(.), .)) %>%
# 			group_by(linkage_PID, index) %>%
# 			mutate(first = first(entry),
# 				   last = last(exit)) %>%
# 			group_by(linkage_PID) %>%
# 			mutate(in_prog_time = exit - entry,
# 				   pre_diff = first - lag(last)) %>%
# 			group_by(linkage_PID, index) %>%
# 			filter(row_number()==1) %>%
# 			group_by(linkage_PID) %>%
# 			data.frame() %>%
# 			select(linkage_PID, entry, exit, agency, index, first:pre_diff)

# ### This only works for difference between differences in PHA

# # ==========================================================================
# # TESTBED
# # 	test <-
# # sum(2014-06-09,2014-08-15)


# # 	surv %>%
# # 			arrange(linkage_PID,entry) %>%
# # 				filter(linkage_PID == 6) %>%

# # 	# get times
# # 	surv %>%
# # 			arrange(linkage_PID,entry) %>%
# # 				filter(linkage_PID == 6 | linkage_PID == 17 | linkage_PID == 20) %>%
# # 			group_by(linkage_PID, agency) %>%
# # 			mutate(program.time = exit - entry,
# # 				   St = entry - lag(exit)) %>%
# # 			data.frame()

# # 			%>%
# # 			group_by(agency) %>%
# # 			summarise(diff = sum(program.time, na.rm = T) + sum(St, na.rm = T))
# 			# glimpse()
# # ==========================================================================
#     df <-
#     data.table::fread(
#         "ID entry 		exit       	agency
#          6 	2014-06-09 	2014-06-27	HMIS
#          6 	2014-06-19 	2014-07-05	HMIS
#          6 	2014-07-02 	2014-08-15	HMIS
#          6 	2014-07-02 	2014-08-17	SHA
#          6 	2014-09-08 	2014-09-27	HMIS
#          6 	2016-08-04 	2016-12-31	HMIS
#         17 	2011-06-01 	      <NA>	HMIS
#         17 	2011-06-20 	2013-06-09	KCHA
#         17 	2013-06-10 	2016-05-31	KCHA
#         17 	2016-06-01 	2017-06-01	KCHA
#         20 	2012-08-17 	2013-08-01	HMIS
#         20 	2013-02-18 	2013-04-14	HMIS
#         20 	2013-08-02 	2013-08-03	SHA
#         20 	2014-12-09 	2015-02-17	HMIS
#         20 	2015-09-23 	2015-09-24	HMIS
#         20 	2015-10-01 	2016-06-30	SHA
#         20 	2016-02-03 	2016-04-10	HMIS"
#     	) %>%
#     mutate(entry = ymd(entry),
#     	   exit = ymd(exit))

#     # f <- rle(df$agency)
#     # f$lengths
#     # f$values



# 		# st = first(entry) - lag(exit))
# 	# Can't double group, it'll only calculate within the group
# 	# ,
# 	# 	   st = first(entry))



# # ==========================================================================
# # LEFT OFF HERE
# # ==========================================================================

# # hmis before pha
# 	prehmis <- left_join(agency_df, inp %>% select(linkage_PID,programs)) %>% data.frame()

# 	### check ###
# 	# prehmis %>% filter(linkage_PID == 1) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 3) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 5) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 6) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 7) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 16) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 17) %>% arrange(entry)
# 	# prehmis %>% filter(linkage_PID == 20) %>% arrange(entry)


# 	order <- prehmis %>%
# 			 filter(programs == "HMIS KCHA" |
# 		   		 	programs == "HMIS KCHA SHA" |
# 		   		 	programs == "HMIS SHA")

# 	first_last <- order %>%
# 			 select(linkage_PID, entry, agency) %>%
# 			 arrange(linkage_PID,entry) %>%
# 			 group_by(linkage_PID) %>%
# 			 summarize(first = dplyr::first(agency),
# 			  		 last = dplyr::last(agency),
# 			  		 diff = difftime(last(entry),
# 			  		 				 first(entry),
# 			  		 				 unit = "days")/365) %>%
# 			 mutate(order = paste(first,last,sep = "_"))

# 	head(first_last)

# 	table(first_last$first,first_last$last)
# 	round(prop.table(table(first_last$first,first_last$last)),2) * 100

# 	x2 <- chisq.test(first_last$first,first_last$last)
# 	x2$observed

# 	first_last.plot <- data.frame(table(first_last$first_last))

# 	ggplot(first_last, aes(x = first_last)) +
# 	geom_histogram(stat = "count") +
# 	theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ## Time between episodes
# 	order %>%
# 	group_by(linkage_PID) %>%
# 	arrange(agency) %>%
# 	mutate(agency.change=cumsum(c(1, diff(agency) != 0))) %>%
# 	glimpse()



# # ==========================================================================
# # Testbed

# 	test <- agency_df %>%
# 			select(linkage_PID, entry, exit, agency) %>%
# 			na.omit() %>%
# 			arrange(linkage_PID,entry,exit) %>%
# 			distinct()

# 	head(test,50)

# # ==========================================================================

# # ==========================================================================
# # ==========================================================================
# # ==========================================================================
# #  LEFT OFF HERE
# # ==========================================================================
# # ==========================================================================
# # ==========================================================================








# 	%>% data.frame %>% head(20)
# 	### model ###
# 	agency_df %>%
# 	# ungroup() %>%
# 	filter(programs == c("HMIS KCHA", "HMIS KCHA SHA", "HMIS SHA")) %>%
# 	# filter(linkage_PID == 16) %>%
# 	select(linkage_PID, entry, agency) %>%
# 	group_by(linkage_PID) %>%
# 	arrange(linkage_PID,entry) %>%
# 	# filter(row_number()==1 | row_number()==n()) %>% data.frame() %>% head(50)
# 	# ungroup() %>%
# 	# group_by(linkage_PID, entry) %>%
# 	# summarise(first = first(agency),
# 	# 		  last = last(agency),
# 	# 		  diff = difftime(last(entry), first(entry), unit = "days")/365) %>%

# 	mutate(first = first(agency),
# 			  last = last(agency),
# 			  diff = difftime(last(entry), first(entry), unit = "days")/365) %>%
# 	data.frame() %>%
# 	head(50)

# 	# df %>%
#  #   group_by(event) %>%
#  #   summarise(First = first(time),
#  #             Last = last(time) ,
#  #             difference= difftime(last(time), first(time), unit='hour'))



# 	group_by(linkage_PID) %>%
# 	mutate(in_sha = ifelse(agency == "SHA", 1, 0),
# 		   in_kcha = ifelse(agency == "KCHA", 1, 0),
# 		   in_hmis = ifelse(agency == "HMIS", 1, 0)) %>%
# 	mutate(programs = ifelse(in_sha==1 & in_kcha==1 & in_hmis==0, "sha_kcha",
# 					  ifelse(in_sha==1 & in_kcha==0 & in_hmis==1, "sha_hmis",
# 					  ifelse(in_sha==0 & in_kcha==1 & in_hmis==1, "kcha_hmis", NA))))





# 	links %>%
# 	select(linkage_PID, in_sha) %>%
# 	filter(in_sha == TRUE) %>%
# 	distinct() %>%
# 	dim()
# 		# 68459

# 	links %>%
# 	select(linkage_PID, in_kcha) %>%
# 	filter(in_kcha == TRUE) %>%
# 	distinct() %>%
# 	dim()
# 		# 84304

# 	links %>%
# 	select(linkage_PID, in_hmis) %>%
# 	filter(in_hmis == TRUE) %>%
# 	distinct() %>%
# 	dim()
# 		# 80231

# 	# program intersections

# 			 links %>%
# 			 # select(pid0, linkage_PID, in_sha) %>%
# 			 filter(in_sha == TRUE) %>%
# 			 glimpse()


# # ==========================================================================
# # Checks
# # ==========================================================================

# ### check that pid1 is right ###
# 	hmis %>% filter(pid1 == "HMIS1_200000") %>% glimpse()
# 	links %>% filter(pid1 == "HMIS1_200000") %>% glimpse()
# 	hmis %>% filter(pid0 == "HMIS0_55213") %>% glimpse()
# 	links %>% filter(pid0 == "HMIS0_55213") %>% glimpse()
# 	# it's not, going to use pid0 to link old data

# 	check <- left_join(hmis,links, by = "pid0")

# ### number of individuals in the links ###
# 	links %>%
# 		select(linkage_PID) %>%
# 		distinct %>%
# 		summarise(n())
# 	# 203,727 unique individuals

# ### number of repeats ###
# 	links <- links %>%
# 			group_by(linkage_PID) %>%
# 			filter(n() >1) %>%
# 			arrange(linkage_PID)
# 	# 58,880 links

# ### Spot check linkages()
# 	links %>%
# 	select(ssn:mname, linkage_PID) %>%
# 	arrange(fname, lname, mname) %>%
# 	data.frame()

# 	# List of linkage_PID that might be problematic based on spot checks
# 	checks <- c(19984, #
# 				26684, #
# 				3365,
# 				20228,
# 				10855,
# 				11350,
# 				3769,
# 				9379,
# 				27300)

# 	# pid0's that are located in the above linkage_PID vector
# 	check2 <- links %>%
# 	filter(linkage_PID %in% checks) %>%
# 	arrange(linkage_PID) %>%
# 	data.frame() %>%
# 	select(pid0)

# 	# pid0's
# 	links %>%
# 		filter(pid0 %in% check2[,1]) %>% head(,3)

# 	links %>%
# 		filter(pid0 == "PHA0_30430")

# 	links %>%
# 	group_by(pid0) %>%
# 	filter(n()>1) %>%
# 	summarise(n())
# 	## there are 4,717 duplicate pid0's

# # ==========================================================================
# # Summary statistics
# # ==========================================================================

# ### vin diagram ###
# 	# number of links


# 	intersects <- order %>%
# 	select(linkage_PID, in_sha:in_hmis) %>%
# 	# gather(program, value, in_sha:in_hmis) %>%
# 	group_by(linkage_PID) %>%
# 	mutate(programs = ifelse(in_sha==TRUE & in_kcha==TRUE & in_hmis==FALSE, "sha_kcha",
# 					  ifelse(in_sha==TRUE & in_kcha==FALSE & in_hmis==TRUE, "sha_hmis",
# 					  ifelse(in_sha==FALSE & in_kcha==TRUE & in_hmis==TRUE, "kcha_hmis", NA))))

# 	data.frame(table(intersects$programs)) %>%
# 	ungroup() %>%
# 	select(programs) %>%
# 	group_by(programs) %>%
# 	summarise(n())


# 	))

# 			  kcha_sha = n_distinct,
# 			  sha_hmis = ,
# 			  kcha_hmis =,
# 			  kcha_hmis_sha = )

# # ==========================================================================
# # EXCESS CODE
# # ==========================================================================


# gc()
# # ==========================================================================
# # TESTBED: overlap
# 	library("lubridate")
# 	preg_start<-as.Date(c("2011-01-01","2012-01-01","2013-01-01"))
# 	preg_end<-preg_start+270 # end after 9 months
# 	smoke_start<-as.Date(c("2011-02-01","2012-08-01","2014-01-01"))
# 	smoke_end<-smoke_start+100 # all three smoked 100 days

# 	smoke <- interval(smoke_start, smoke_end, tzone="UTC")
# 	preg <- interval(preg_start, preg_end, tzone="UTC")
# 	day(as.period(lubridate::intersect(smoke, preg), "days"))

# 	options(width = 180)

# 	test <- order %>%
# 			filter(linkage_PID == 6) %>%
# 			mutate(intersect = day(as.period(lubridate::intersect(interval(entry, exit), interval(lead(entry), lead(exit)))))) %>%
# 			data.frame()


# 				   period = as_date(period)) %>%
# 			data.frame()

# 		   intersect = day(as.period(lubridate::intersect(period, lead(period))))) %>%
# 	data.frame()

# # ==========================================================================

 			   # mutate(overlap = ifelse(is.na(intersect),
			   # 						   "solo",
					 # 		   	ifelse(entry < lead(entry) &
					 # 		   		   exit < lead(exit),
					 # 		   		   "lag_overlap",
					 # 		   	ifelse(lag(entry) < entry &
					 # 		   		   lag(exit) > exit,
					 # 		   		   "mid_overlap",
					 # 		   	ifelse(lag(entry) < entry &
					 # 		   		   lag(exit) < exit,
					 # 		   		   "lead_overlap",
					 # 		   		   NA))))) %>%
			   # mutate(overlap = ifelse(overlap == "solo" &
			   # 						   entry == lead(entry) &
					 # 		   		   is.na(exit),
					 # 		   		   "one_day",
					 # 		   		   overlap)) %>%
			   # mutate(overlap = ifelse(lag(overlap) == "one_day",
			   # 						   "one_day_leading_overlap",
			   # 						   overlap))

# ==========================================================================
# mutating lag and lead overlaps
# ==========================================================================


		   			# mid_prog = ifelse(entry > lag(entry) &
			 		# 				  exit < lag(exit) &
			 		# 				  agency != lag(agency),
			 		# 				  1,
			 		# 				  0),
			 		# mid_prog = ifelse(is.na(mid_prog), 0, mid_prog))

# ==========================================================================
# TESTBED
	test <- overlap %>%
			mutate(int = interval(entry,exit)) %>%
			group_by(linkage_PID) %>%
			mutate(overlap = int_overlaps(
								interval(entry,exit),
										 interval(lag(entry),
										 		  lag(exit))),
				   align = int_aligns(
								interval(entry,exit),
										 interval(lag(entry),
										 		  lag(exit))),
				   lag = ifelse(overlap == T &
								entry <= lead(entry) &
								exit <= lead(exit) &
								exit > lead(entry), 1, 0),
			   	   mid = ifelse(overlap ==T &
								lag(entry) <= entry &
								lag(exit) >= exit, 1,0),
			   	   lead = ifelse(overlap ==T &
								 entry >= lag(entry) &
								 exit >= lag(exit) &
								 entry < lag(exit), 1, 0),
			   	   open = ifelse(is.na(exit),1,0),
			   	   over = ifelse(!is.na(lead(entry)) &
								 lead(mid)==1, 1,
			   		      ifelse(!is.na(lag(entry)) &
								 lag(open) == 1 &
								 entry >= lag(entry), 1, 0)),
			   		   # Over = when over mid or over 1-days
			   	   solo = ifelse(is.na(intersect) &
								 lag == 0 &
								 mid == 0 &
								 lead == 0 &
								 over == 0 &
								 open == 0, 1, 0))


			group_by(linkage_PID) %>%



			mutate(t = interval(int_start(entry), int_end(exit))),
					overlap = int_overlaps(interval(t), interval(lag(t))))

			,
					int_diff = int_diff(t, lead(t)))

	test %>% select(-prog_type, -agency_trans, -exit2, -entry,-exit) %>% data.frame
 # End TESTBED
# ==========================================================================