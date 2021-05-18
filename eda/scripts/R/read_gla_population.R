# Process Source Data ==========================================================
library(tidyverse)
library(readxl)

## Configuration ---------------------------------------------------------------
DATA_DIR = "./data/input/population/"
EG_PROJECTIONS_FILE = paste0(
  DATA_DIR, "gla_ethnic_group_projections_central_2016.xlsx")
ATLAS_FILE = paste0(DATA_DIR, "gla_lsoa_atlas_2011.xls")
ATLAS_COL_NAMES_FILE = paste0(DATA_DIR, "meta/atlas_column_names.csv")

REFERENCE_DIR = "./data/output/reference/"
OUTPUT_FILE = paste0(REFERENCE_DIR, "gla_borough_ethnic_grp_raw.csv")

## Read and Write CSV Data -----------------------------------------------------
sheets <- c("Population - Males", "Population - Females")

do.call(rbind, lapply(sheets, function(sheet_name) 
  readxl::read_xlsx(EG_PROJECTIONS_FILE, sheet = sheet_name))) %>%
  tidyr::pivot_longer(cols = 6:45, names_to = "year", values_to = "count") %>%
  write.csv(OUTPUT_FILE, row.names = FALSE)

tbl_atlas_1 <- readxl::read_xls(
  ATLAS_FILE,
  sheet = "iadatasheet1",
  skip = 3,
  col_names = read.csv(ATLAS_COL_NAMES_FILE)$name,
  na = c("."))

f <- function(x) stringr::str_split
test <- tbl_atlas_1
test2 <- test %>%
  tidyr::pivot_longer(
    cols = 3:185, 
    names_to = "observation_of")
test3 <- test2 %>%
  dplyr::mutate(
    observation_of_1 = stringr::str_split(observation_of, "_", simplify = TRUE)[, 1],
    observation_of_2 = stringr::str_split(observation_of, "_", simplify = TRUE)[, 2],
    observation_of_3 = stringr::str_split(observation_of, "_", simplify = TRUE)[, 3])

# rm(list = ls())
