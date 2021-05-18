# Reads Police UK Stop and Search Data (MOPAC Hackathon)
# Author: Alex Jackson <alex.jackson@kcl.ac.uk>
#
# This script reads each of the csv files in turn, generates a single dataframe
# containing the atomic information from each observation, and writes this out 
# to a single csv file.
#
# # Input Files
# This script expects the `DATA_DIR` directory to exist and contain the 
# following three files.
#  - puk_stop_and_search_2018.csv
#  - puk_stop_and_search_2019.csv
#  - puk_stop_and_search_2020.csv
#
# In practice, these can be copied/linked from the sharepoint directory paths.
#  - DATA/Stop and Search/2018-metropolitan-stop-and-search.csv
#  - DATA/Stop and Search/2019-metropolitan-stop-and-search.csv
#  - DATA/Stop and Search/2020-metropolitan-stop-and-search.csv
#
# # Output Files
# The script expects the `OUTPUT_DIR` directory to exist and will write out the
# following single file.
#  - puk_stop_and_search.csv (consolidated time series without extra columns)

# Process Source Data ==========================================================
## Configuration ---------------------------------------------------------------
DATA_DIR = "./data/input/stop_and_search/"
OUTPUT_DIR = "./data/output/reference/"
DATA_PREFIX = "puk_stop_and_search"
YEARS = c(2020, 2019, 2018)

## Read and Clean CSV Data -----------------------------------------------------
all_csv_files <- paste0(DATA_DIR, DATA_PREFIX, "_", YEARS, ".csv")
data <- do.call(rbind, lapply(all_csv_files, read.csv))

# Drop unnecessary variables
data$Time <- data$Date..dd.mm.yyyy. <- data$Month.Year <- NULL # Duplicate info
data$Outcome.linked.to.object.of.search <- NULL # Always NA
data$Removal.of.more.than.just.outer.clothing <- NULL # Always NA

# Remove colon from offset and parse date as datetime list object
data$Date <- paste0(substr(data$Date, 1, 22), "00")
data$Date <- strptime(data$Date, "%Y-%m-%dT%H:%M:%S%z")

data$Age.range[data$Age.range == "Oct-17"] <- "10-17" # Correct excel issue

# Set empty values to NA
data$Gender[data$Gender == ""] <- NA
data$Age.range[data$Age.range == ""] <- NA
data$Self.defined.ethnicity[data$Self.defined.ethnicity == ""] <- NA
data$Officer.defined.ethnicity[data$Officer.defined.ethnicity == ""] <- NA
data$Object.of.search[data$Object.of.search == ""] <- NA

# Order chronologically
data <- data[order(data$Date), ]
data$id <- 1:nrow(data)
row.names(data) <- data$id
data <- data[, c(ncol(data), 1:(ncol(data) - 1))]

# Rename variables to shortened lower case
names(data) <- c("id", "type", "date", "lat", "long", "gender", "age_range", 
                 "ethnicity_self_rpt", "ethnicity_officer_rpt", "legislation",
                 "search_object", "outcome")

## Write Reference Copy to File ------------------------------------------------
write.csv(data, paste0(OUTPUT_DIR, DATA_PREFIX, "_raw.csv"), row.names = F)

# Clean Workspace
rm(list = ls())
