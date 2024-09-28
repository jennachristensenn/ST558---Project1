library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

PUMS_URL_MAIN_STUB <- "https://api.census.gov/data/"
PUMS_URL_ACS_STUB <- "/acs/acs1/pums"
PUMS_URL_QUERYSTRING_STUB <- "?get=PWGTP&for=state:02"

DEFAULT_YEARS <- c(2022)
#PWGTP is always included so is in base querystring stub
DEFAULT_NUM_VARS <- c("AGEP")
DEFAULT_CAT_VARS <- c("SEX")
ALL_GEO <- c("REGION", "DIVISION", "ST")
DEFAULT_GEO_VARS <- c("ALL_GEO")

#account for lack of 2020 data on site, then compare years as character for consistency
AVAILABLE_YEARS <- as.character(c(seq(2010, 2019), seq(2021, 2022)))
AVAILABLE_NUM_VARS <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
AVAILABLE_CAT_VARS <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
AVAILABLE_GEO_VARS <- c("REGION", "DIVISION", "ST", "ALL_GEO")

fetch_census_raw <- function(year=2022, varstring=""){
  
  prepared_census_url <- paste(PUMS_URL_MAIN_STUB, year, PUMS_URL_ACS_STUB, PUMS_URL_QUERYSTRING_STUB, sep = "")
  if(nchar(varstring) > 0){prepared_census_url <-  paste(prepared_census_url, varstring, sep = ",")}
  
  return(GET(prepared_census_url))
}

#still need to fix dates
parse_census_response <- function(census_raw){
  census_tbl_in_progress <- rawToChar(census_raw) |>  fromJSON()
  census_tbl <- as_tibble(census_tbl_in_progress[-1,])
  colnames(census_tbl) <- census_tbl_in_progress[1,]
  num_col <- c("AGEP", "GASP", "GRPIP", "JWMNP")
  char_col <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
  census_tbl <- census_tbl |>
    mutate(across(all_of(numeric_columns), ~ as.numeric(.))) |>
    mutate(across(all_of(char_col), ~ as.factor(.)))
  return(census_tbl)
}

fetch_census_data <- function(years=DEFAULT_YEARS, num_vars=DEFAULT_NUM_VARS, cat_vars=DEFAULT_CAT_VARS, geo_vars = DEFAULT_GEO_VARS){
  
  num_vars_checked <- num_vars[num_vars %in% AVAILABLE_NUM_VARS]
  num_vars_failed <- num_vars[!(num_vars %in% AVAILABLE_NUM_VARS)]
  if(length(num_vars_failed > 0)){warning("Invalid numeric variable(s) excluded: ", paste(num_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid numeric variables supplied. Using default AGEP and PWGTP.")
    num_vars_checked = DEFAULT_NUM_VARS
  }
  
  cat_vars_checked <- cat_vars[cat_vars %in% AVAILABLE_CAT_VARS]
  cat_vars_failed <- cat_vars[!(cat_vars %in% AVAILABLE_CAT_VARS)]
  if(length(cat_vars_failed > 0)){warning("Invalid categorical variable(s) excluded: ", paste(cat_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid categorical variables supplied. Using default SEX.")
    cat_vars_checked = DEFAULT_CAT_VARS
  }
  
  years_checked <- as.character(years)[years %in% AVAILABLE_YEARS]
  years_failed <- as.character(years)[!(years %in% AVAILABLE_YEARS)]
  if(length(years_checked) == 0){
    warning("No valid years supplied. Using default 2022.")
    years_checked = DEFAULT_YEARS
  }
  
  geo_vars_checked <- geo_vars[geo_vars %in% AVAILABLE_GEO_VARS]
  geo_vars_failed <- geo_vars[!(geo_vars %in% AVAILABLE_GEO_VARS)]
  if(length(geo_vars_failed) > 0){warning("Invalid geographic variable(s) excluded: ", paste(geo_vars_failed))}
  if(length(geo_vars_checked) == 0){
    warning("No valid geographic variables supplied. Using default ALL.")
    geo_vars_checked = DEFAULT_GEO_VARS
  }
  
  querystring_var_list <-  paste(num_vars_checked, cat_vars_checked, sep = ",") #years_checked, geo_vars_checked
  
  fetch_census_single_year <- function(year){
    return(current_iter_response <- fetch_census_raw(varstring = querystring_var_list)$content |> 
             parse_census_response())
  }
  
  return(map_dfr(years, fetch_census_single_year))
}

test <- fetch_census_data(num_vars = c("GASP"), cat_vars = c("FER"))


