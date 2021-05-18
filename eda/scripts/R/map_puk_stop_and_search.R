# Maps Coordinates from Search Data to Output Areas (MOPAC Hackathon)
# Author: Alex Jackson <alex.jackson@kcl.ac.uk>
#
# This script reads the cleaned Police UK Stop and Search Data and joins 
# higher-level descriptive information about the location of the search. To do
# so, the script reads shape data from the economic Output Area (OA) boundary 
# data retrieved from the GLA datastore and projects each pair of coordinates 
# onto the area boundaries to determine its relative location.
#
# Additionally, this script will read in the London-specific Output Area 
# Classifications (LOAC), also retrieved from the GLA datastore, and merge this 
# with the above data.
#
# # Input Files
# This script expects the `SOURCE_DIR` and `REFERENCE_DIR` directories to exist.
# The reference directory should contain the file `puk_stop_and_search_raw.csv`;
# and the source directory should contain the following.
#  - OA Shape Files, i.e. `OA_2011_London_gen_MHW.shp` etc.
#  - LOAC Mapping Table, `LOAC classification.xls`.
#
# # Output Files
# The script will write out the following single output file.
#  - puk_stop_and_search_extended.csv (extended version of raw data).

# Process Source Data ==========================================================
library(sf)
library(tidyverse)
library(readxl)

## Configuration ---------------------------------------------------------------
SOURCE_DIR = "./data/source/"
LOAC_MAP_FILE = paste0(SOURCE_DIR, "cis_spatial/LOAC classification.xls")
OA_DIR = paste0(SOURCE_DIR, "gla_spatial/statistical-gis-boundaries-london/")
OA_SHAPE_FILE = paste0(OA_DIR, "ESRI/OA_2011_London_gen_MHW.shp")

REFERENCE_DIR = "./data/output/reference/"
SEARCH_DATA_FILE = paste0(REFERENCE_DIR, "puk_stop_and_search_raw.csv")
OUTPUT_FILE = paste0(REFERENCE_DIR, "puk_stop_and_search_extended.csv")

## Read Spatial and Reference Data ---------------------------------------------
search_data <- read.csv(SEARCH_DATA_FILE)
loac_map <- readxl::read_xls(LOAC_MAP_FILE, sheet = "LOAC classification")
shp_oa <- sf::st_read(OA_SHAPE_FILE) %>% sf::st_transform(4326)

# Project Searches Data into Output Areas ======================================
mapped_search_data <- search_data %>%
  dplyr::select(id, long, lat) %>%
  dplyr::filter(!is.na(long) & !is.na(lat)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
  sf::st_join(shp_oa, join = sf::st_within) %>%
  dplyr::select(-c(13:18)) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::right_join(search_data, by = c("id")) %>%
  dplyr::rename(
    oa_11_code = OA11CD,
    lsoa_11_code = LSOA11CD,
    lsoa_11_name = LSOA11NM,
    msoa_11_code = MSOA11CD,
    msoa_11_name = MSOA11NM,
    ward_11_code = WD11CD_BF,
    ward_11_name = WD11NM_BF,
    lad_11_code = LAD11CD,
    lad_11_name = LAD11NM) %>%
  dplyr::relocate(type:outcome, .after = id) %>%
  dplyr::relocate(lsoa_11_name, .after = lsoa_11_code) %>%
  dplyr::relocate(msoa_11_name, .after = msoa_11_code) %>%
  dplyr::mutate(RGN11CD = NULL, RGN11NM = NULL)

# Join London Output Area Classifications ======================================
mapped_search_data_ldn <- mapped_search_data %>%
  dplyr::left_join(loac_map, by = c("oa_11_code" = "OA")) %>%
  dplyr::mutate("Local Authority" = NULL) %>%
  dplyr::rename(
    loac_super_grp_code = "Super Group",
    loac_super_grp_name = "Super Group Name",
    loac_grp_code = "Group",
    loac_grp_name = "Group Name",
    loac_sub_grp_code = "Sub Group")

# Write Output File ============================================================
mapped_search_data_ldn %>%
  write.csv(OUTPUT_FILE, row.names = FALSE)

rm(list = ls())
