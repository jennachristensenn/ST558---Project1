library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

PUMS_URL_MAIN_STUB <- "https://api.census.gov/data/"
PUMS_URL_ACS_STUB <- "/acs/acs1/pums"
PUMS_URL_QUERYSTRING_STUB <- "?get=PWGTP"
#&forr=state:02

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

fetch_census_raw <- function(year=2022, varstring="", geo_vars = "state:02"){
  
  prepared_census_url <- paste(PUMS_URL_MAIN_STUB, year, PUMS_URL_ACS_STUB, sep = "")
  if (nchar(varstring) > 0) {
    prepared_census_url <- paste(prepared_census_url, "?get=PWGTP,", varstring, sep = "")
  } else {
    prepared_census_url <- paste(prepared_census_url, "?get=PWGTP", sep = "")
  }
  prepared_census_url <- paste(prepared_census_url, "&for=", geo_vars, sep = "")
  
  response <- GET(prepared_census_url)
  
  if (http_error(response)) {
    stop("Failed to fetch data from the API. Check query and internet connection.")
  }  
  return(response)
}

#still need to fix dates
parse_census_response <- function(census_raw){
  census_tbl_in_progress <- rawToChar(census_raw) |>  fromJSON()
  census_tbl <- as_tibble(census_tbl_in_progress[-1,])
  colnames(census_tbl) <- census_tbl_in_progress[1,]
  num_col <- c("AGEP", "GASP", "GRPIP", "JWMNP")
  char_col <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
  census_tbl <- census_tbl |>
    mutate(across(all_of(numeric_col), ~ as.numeric(.))) |>
    mutate(across(all_of(char_col), ~ as.factor(.)))
  class(census_tbl) <- c("census", class(census_tbl))
  return(census_tbl)
}

fetch_census_data <- function(years=DEFAULT_YEARS, num_vars=DEFAULT_NUM_VARS, cat_vars=DEFAULT_CAT_VARS, geo_vars = DEFAULT_GEO_VARS){
  
  num_vars_checked <- num_vars[num_vars %in% AVAILABLE_NUM_VARS]
  num_vars_failed <- num_vars[!(num_vars %in% AVAILABLE_NUM_VARS)]
  if(length(num_vars_failed) > 0){warning("Invalid numeric variable(s) excluded: ", paste(num_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid numeric variables supplied. Using default AGEP and PWGTP.")
    num_vars_checked = DEFAULT_NUM_VARS
  }
  
  cat_vars_checked <- cat_vars[cat_vars %in% AVAILABLE_CAT_VARS]
  cat_vars_failed <- cat_vars[!(cat_vars %in% AVAILABLE_CAT_VARS)]
  if(length(cat_vars_failed) > 0){warning("Invalid categorical variable(s) excluded: ", paste(cat_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid categorical variables supplied. Using default SEX.")
    cat_vars_checked = DEFAULT_CAT_VARS
  }
  
  years_checked <- as.character(years)[years %in% AVAILABLE_YEARS]
  years_failed <- as.character(years)[!(years %in% AVAILABLE_YEARS)]
  if(length(years_checked) > 0){warning("Invalid year variable(s) excluded: ", paste(years_failed))}
  if(length(years_checked) == 0){
    warning("No valid years supplied. Using default 2022.")
    years_checked = DEFAULT_YEARS
  }
  
  geo_vars_checked <- if ("ALL_GEO" %in% geo_vars || length(geo_vars) == 0) {
    "state:02"
  } else {
    paste(geo_vars[geo_vars %in% AVAILABLE_GEO_VARS], collapse = ",")
  }
  
  querystring_var_list <-  paste(c(num_vars_checked, cat_vars_checked, geo_vars_checked, years_checked), collapse = ",") 
  
  fetch_census_single_year <- function(year) {
    current_iter_response <- fetch_census_raw(year, varstring = querystring_var_list)$content
    parse_census_response(current_iter_response)
  }
  
  return(map_dfr(years, fetch_census_single_year))
}

#not working in current state
test <- fetch_census_data(num_vars = c("GASP"), cat_vars = c("FER"))

summary.census <- function(){}
