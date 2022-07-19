################################################################################
# ph_tc_process_hist_events.R
# This script process the historical events, containing impact data for major historical
# tropical cyclone events affecting the Philippines. The data is from ECMWF or IbTRACS?
# The imapct estimates are generated from the Netherlands Red Cross 510 machine 
# learning forecasting model.
# Author: Alex Saunders
# Date created: 12/07/2022
# Date modified: 19/07/2022
################################################################################


################################################################################
# Install packages
################################################################################

rm(list=ls())

packages = c("")


################################################################################
# Set up working environment
################################################################################

input_path <- "C:/Users/alexa/Documents/02_work/02_start/06_technical_advisor_work/02_countries/03_philippines/01_tc/04_510_build/04_deliverables/hist_events/"
input_files <- list.files(input_path)
output_path <- "C:/Users/alexa/Documents/02_work/02_start/06_technical_advisor_work/02_countries/03_philippines/01_tc/07_r/02_outputs/05_hist_events/"


################################################################################
# Description of inputs
################################################################################

# the file contains impact data per municipality for historical typhoons, both 
# reported (from DROMIC) and predicted (by the 510 forecasting model)
# the file contains:
# typhoon - historical typhoon name + _ + year
# Mun_Code - pcode for each of the municipalities
# DAM_number_Hous_DROMIC - reported number of completely damaged houses
# DAM_number_Hous_Model - predicted number of completely damaged houses


################################################################################
# Create a summary of historical events
################################################################################

# read the input
input_file <- input_files[1] # number of damaged houses (other file is %)
hist_event_table <- read.csv(paste0(input_path, input_file))

# aggregate losses by event
hist_event_losses <- aggregate(x = list(loss_bldg_reported = hist_event_table$DAM_number_Hous_DROMIC, 
                                    loss_bldg_model = hist_event_table$DAM_number_Hous_Model),
                               by = list(typhoon = hist_event_table$typhoon),
                               FUN = sum)

# add name and year columns and order by year of occurrence
hist_event_losses$name <- gsub("[0-9]+", "", hist_event_losses$typhoon)
hist_event_losses$year <- as.numeric(gsub("[^0-9]", "", hist_event_losses$typhoon))
hist_event_losses <- hist_event_losses[order(hist_event_losses$year, hist_event_losses$name),]

# write out the summary of historical event details and losses
write.csv(hist_event_losses, paste0(output_path, "hist_event_losses.csv"), row.names = FALSE)



################################################################################
# Extract the losses per event for target municipalities
################################################################################

# read list of target municipalities
mun_input_path <- "C:/Users/alexa/Documents/02_work/02_start/06_technical_advisor_work/02_countries/03_philippines/01_tc/03_data/hdx/"
mun_input_file <- "target_mun.csv"
target_mun <- read.csv(paste0(mun_input_path, mun_input_file))
target_pcode <- target_mun$admin3Pcode


# extract the historical losses per event and municipality, for only the target 
# municipalities
hist_event_losses_by_mun <- hist_event_table[which(hist_event_table$Mun_Code %in% target_pcode),]
length(unique(hist_event_losses_by_mun$Mun_Code)) # 73 municipalities
length(unique(target_pcode)) # 73


# tidy up the table and write out as csv
hist_event_losses_by_mun <- subset(hist_event_losses_by_mun, select = -c(X))
hist_event_losses_by_mun <- merge(hist_event_losses_by_mun, hist_event_losses[,c("typhoon","name","year")], by = "typhoon")
colnames(hist_event_losses_by_mun) <-  c("typhoon","Mun_Code","loss_bldg_reported", "loss_bldg_model", "name", "year") 
hist_event_losses_by_mun <- hist_event_losses_by_mun[order(hist_event_losses_by_mun$year, hist_event_losses_by_mun$name, hist_event_losses_by_mun$Mun_Code),]
write.csv(hist_event_losses_by_mun, paste0(output_path, "hist_event_losses_by_target_mun.csv"), row.names = FALSE)



################################################################################
# Aggregate the losses per event for target provinces
################################################################################



# aggregate losses by event and adm2
colnames(hist_event_losses_by_mun) <- c("typhoon", "admin3Pcode", "loss_bldg_reported", "loss_bldg_model","name","year")
hist_event_losses_by_mun_wadm2 <- merge(hist_event_losses_by_mun, subset(target_mun, select = c(admin3Pcode, admin2Pcode)), by = "admin3Pcode")


hist_event_losses_by_prov <- aggregate(x = list(loss_bldg_reported = hist_event_losses_by_mun_wadm2$loss_bldg_reported, 
                                                loss_bldg_model = hist_event_losses_by_mun_wadm2$loss_bldg_model),
                                                by = list(typhoon = hist_event_losses_by_mun_wadm2$typhoon, 
                                                                   name = hist_event_losses_by_mun_wadm2$name, 
                                                                   year = hist_event_losses_by_mun_wadm2$year,
                                                                   admin2Pcode = hist_event_losses_by_mun_wadm2$admin2Pcode), 
                            FUN = sum)

# write out table of historical event loss by province for target provinces
write.csv(hist_event_losses_by_prov, paste0(output_path, "hist_event_losses_by_target_prov.csv"), row.names = FALSE)


