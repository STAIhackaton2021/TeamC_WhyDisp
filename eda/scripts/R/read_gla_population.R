# Process Source Data ==========================================================
library(tidyverse)
library(readxl)

## Configuration ---------------------------------------------------------------
DATA_DIR = "./data/input/population/"
EG_PROJECTIONS_FILE = paste0(
  DATA_DIR, "gla_ethnic_group_projections_central_2016.xlsx")
ATLAS_FILE = paste0(DATA_DIR, "gla_lsoa_atlas_2011.xls")
ATLAS_COL_NAMES_FILE = paste0(DATA_DIR, "meta/atlas_column_names.csv")
ATLAS_COL_NAMES_FILE_2 = paste0(DATA_DIR, "meta/atlas_column_names_2.csv")

REFERENCE_DIR = "./data/output/reference/"
OUTPUT_FILE = paste0(REFERENCE_DIR, "gla_borough_ethnic_grp_raw.csv")

## Read and Write CSV Data -----------------------------------------------------
sheets <- c("Population - Males", "Population - Females")

do.call(rbind, lapply(sheets, function(sheet_name) 
  readxl::read_xlsx(EG_PROJECTIONS_FILE, sheet = sheet_name))) %>%
  tidyr::pivot_longer(cols = 6:45, names_to = "year", values_to = "count") %>%
  write.csv(OUTPUT_FILE, row.names = FALSE)

rm(sheets, EG_PROJECTIONS_FILE)

## Read and Process Atlas Data -------------------------------------------------
tbl_atlas_1 <- readxl::read_xls(
    ATLAS_FILE,
    sheet = "iadatasheet1",
    skip = 3,
    col_names = read.csv(ATLAS_COL_NAMES_FILE)$name,
    na = c(".")) %>%
  tidyr::pivot_longer(cols = 3:185, names_to = "variable") %>%
  dplyr::mutate(
    var_1 = stringr::str_split(variable, "_", simplify = TRUE)[, 1],
    var_2 = stringr::str_split(variable, "_", simplify = TRUE)[, 2],
    var_3 = stringr::str_split(variable, "_", simplify = TRUE)[, 3])

tbl_atlas_2 <- readxl::read_xls(
    ATLAS_FILE,
    sheet = "iadatasheet2",
    skip = 3,
    col_names = read.csv(ATLAS_COL_NAMES_FILE_2)$name,
    na = c(".", "-")) %>%
  tidyr::pivot_longer(cols = 3:94, names_to = "variable") %>%
  dplyr::mutate(
    var_1 = stringr::str_split(variable, "_", simplify = TRUE)[, 1],
    var_2 = stringr::str_split(variable, "_", simplify = TRUE)[, 2],
    var_3 = stringr::str_split(variable, "_", simplify = TRUE)[, 3])

lst_tbls <- tbl_atlas_1 %>%
  dplyr::group_by(var_1) %>%
  dplyr::group_split() %>%
  c(tbl_atlas_2 %>%
      dplyr::group_by(var_1) %>%
      dplyr::group_split())

starts_with_perc <- function(x) stringr::str_starts(x, "%")
ends_with_perc <- function(x) stringr::str_ends(x, "(%)")

lst_new_tbls = list()
lst_new_tbls[[1]] <- lst_tbls[[1]] %>%
  dplyr::mutate(year = 2011) %>%
  dplyr::mutate(variable = NULL, var_1 = NULL, var_2 = NULL) %>%
  dplyr::rename(age_range = var_3, count = value) %>%
  dplyr::filter(!(age_range %in% c("All Ages", "Working-age"))) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[2]] <- lst_tbls[[2]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(birth_country = var_2, year = var_3, count = value) %>%
  dplyr::filter(!starts_with_perc(birth_country)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[3]] <- lst_tbls[[3]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(dwelling_type = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(dwelling_type)) %>%
  dplyr::filter(dwelling_type != "All Households") %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[4]] <- lst_tbls[[4]] %>%
  dplyr::relocate(value, .after = dplyr::last_col()) %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(ethnic_group = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(ethnic_group)) %>%
  dplyr::filter(ethnic_group != "BAME") %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[5]] <- lst_tbls[[5]] %>%
  tidyr::pivot_wider(id_cols = c(codes, names, var_3), names_from = var_2) %>%
  dplyr::rename(median_price = "Median Price (£)", sales = Sales, year = var_3)

lst_new_tbls[[6]] <- lst_tbls[[6]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(household_comp = var_2, year = var_3, count = value) %>%
  dplyr::filter(!starts_with_perc(household_comp)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[7]] <- lst_tbls[[7]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(household_lang = var_2, year = var_3, count = value) %>%
  dplyr::filter(!starts_with_perc(household_lang)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)
  
lst_new_tbls[[8]] <- lst_tbls[[8]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL, var_2 = NULL) %>%
  dplyr::rename(total_households = value, year = var_3) %>%
  dplyr::filter(!starts_with_perc(total_households)) %>%
  dplyr::relocate(total_households, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)
  
lst_new_tbls[[9]] <- lst_tbls[[9]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(age_range = var_2, year = var_3, projected_pop = value) %>%
  dplyr::filter(age_range != "All Ages") %>%
  dplyr::relocate(projected_pop, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[10]] <- lst_tbls[[10]] %>%
  tidyr::pivot_wider(id_cols = c(codes, names, var_3), names_from = var_2) %>%
  dplyr::rename(
    year = var_3, 
    area = "Area (Hectares)", 
    density = "Persons per hectare")

lst_new_tbls[[10]] <- lst_new_tbls[[10]] %>%
  dplyr::filter(year != "Constant") %>%
  dplyr::mutate(area = NULL) %>%
  dplyr::left_join(
    lst_new_tbls[[10]] %>% 
      dplyr::filter(year == "Constant") %>% 
      dplyr::select(codes, area))

lst_new_tbls[[11]] <- lst_tbls[[11]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(religion = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(religion)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[12]] <- lst_tbls[[12]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(tenure = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(tenure)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[13]] <- lst_tbls[[13]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(adult_employment = var_2, year = var_3, count = value) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[14]] <- lst_tbls[[14]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(car_availability = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(car_availability)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[15]] <- lst_tbls[[15]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(child_benefit = var_2, year = var_3, count = value) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[16]] <- lst_tbls[[16]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(economic_activity = var_2, year = var_3, count = value) %>%
  dplyr::filter(!stringr::str_ends(economic_activity, "(Total)|(Rate)")) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[17]] <- lst_tbls[[17]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(health = var_2, year = var_3, count = value) %>%
  dplyr::filter(!ends_with_perc(health)) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[18]] <- lst_tbls[[18]] %>%
  tidyr::pivot_wider(id_cols = c(codes, names, var_3), names_from = var_2) %>%
  dplyr::rename(
    year = var_3, 
    mean_household_income = dplyr::starts_with("Mean"), 
    median_household_income = dplyr::starts_with("Median"))

lst_new_tbls[[19]] <- lst_tbls[[19]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(lone_parents = var_2, year = var_3, count = value) %>%
  dplyr::filter(!stringr::str_ends(lone_parents, "%")) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[20]] <- lst_tbls[[20]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(transport_access = var_2, ptal = var_3, count = value) %>%
  dplyr::filter(stringr::str_starts(transport_access, "Number")) %>%
  dplyr::mutate(
    year = 2014,
    transport_access = NULL,
    score = as.numeric(stringr::str_extract(ptal, "\\d")) + 1)

lst_new_tbls[[20]] <- lst_new_tbls[[20]] %>%
  dplyr::left_join(lst_new_tbls[[20]] %>%
      dplyr::group_by(codes, names, year) %>%
      dplyr::summarise(avg_score = sum(count * score) / sum(count))) %>%
  dplyr::relocate(count, avg_score, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, ptal, .after = 1) %>% 
  dplyr::mutate(score = NULL)

lst_new_tbls[[21]] <- lst_tbls[[21]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(qualification = var_2, year = var_3, count = value) %>%
  dplyr::filter(!stringr::str_starts(qualification, "%")) %>%
  dplyr::relocate(count, .after = dplyr::last_col()) %>%
  dplyr::relocate(codes, names, year, .after = 1)

lst_new_tbls[[22]] <- lst_tbls[[22]] %>%
  dplyr::mutate(variable = NULL, var_1 = NULL) %>%
  dplyr::rename(year = var_2, type = var_3, count = value) %>%
  dplyr::filter(!stringr::str_ends(type, "Total")) %>%
  dplyr::relocate(count, .after = dplyr::last_col())

tbl_names <- c(
  "lsoa_population_age_2011",
  "lsoa_birth_country_2011",
  "lsoa_household_type_2011",
  "lsoa_ethnic_group_2011",
  "lsoa_house_sales_2009_14",
  "lsoa_household_composition_2011",
  "lsoa_household_language_2011",
  "lsoa_total_households_2011",
  "lsoa_mid_year_projection_age_2001_13",
  "lsoa_population_density_2012_13",
  "lsoa_religion_2011",
  "lsoa_household_tenure_2011",
  "lsoa_household_adult_employment_2011",
  "lsoa_household_car_availability_2011",
  "lsoa_child_benefit_2013",
  "lsoa_economic_activity_2011",
  "lsoa_health_2011",
  "lsoa_household_income_2013",
  "lsoa_household_lone_parent_2011",
  "lsoa_public_transport_access_level_2014",
  "lsoa_qualifications_2011",
  "lsoa_road_traffic_accidents_2010_14"
)

names(lst_new_tbls) <- tbl_names

lst_new_tbls %>% 
  purrr::iwalk(~ write.csv(.x, paste0(REFERENCE_DIR, "gla_atlas_", .y, ".csv")))
