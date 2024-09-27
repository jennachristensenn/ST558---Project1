library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

PUMS_URL_MAIN_STUB <- "https://api.census.gov/data/"
PUMS_URL_ACS_STUB <- "/acs/acs1/pums"
PUMS_URL_QUERYSTRING_STUB <- "?get=PWGTP"

DEFAULT_YEARS <- c(2022)
#PWGTP is always included so is in base querystring stub
DEFAULT_NUM_VARS <- c("AGEP")
DEFAULT_CAT_VARS <- c("SEX")
#Adding in the geography component
ALL_GEO <- c("REGION", "DIVISION", "ST")
DEFAULT_GEO_VARS <- c("ALL_GEO")

#account for lack of 2020 data on site
AVAILABLE_YEARS <- c(seq(2010, 2019), seq(2021, 2022))
AVAILABLE_NUM_VARS <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
AVAILABLE_CAT_VARS <- c("FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL", "SEX")
#Adding in the geography component
AVAILABLE_GEO_VARS <- c("REGION", "DIVISION", "ST", "ALL_GEO")

fetch_census_raw <- function(year=2022, varstring=""){
  
  prepared_census_url <- paste(PUMS_URL_MAIN_STUB, year, PUMS_URL_ACS_STUB, PUMS_URL_QUERYSTRING_STUB, sep = "")
  if(nchar(varstring) > 0){prepared_census_url <-  paste(prepared_census_url, varstring, sep = ",")}
  
  return(GET(prepared_census_url))
}

parse_census_response <- function(census_raw){
  census_tbl_in_progress <- rawToChar(census_raw) |>  fromJSON()
  
  return(census_tbl_in_progress)
}

#added geography component & years check
fetch_census_data <- function(years=DEFAULT_YEARS, num_vars=DEFAULT_NUM_VARS, cat_vars=DEFAULT_CAT_VARS, geo_vars = DEFAULT_GEO_VARS){
  
# years variable check: commented out -- casuing issues
#  year_vars_checked <- years[years %in% AVAILABLE_YEARS]
#  year_vars_failed <- years[!(years %in% AVAILABLE_YEARS)]
#  if(length(year_vars_failed) > 0){warning("Invalid year variable(s) excluded: ", paste(year_vars_failed))}
#  if(length(year_vars_checked) == 0){
#    warning("No valid year variables supplied. Using default year 2022.")
#    year_vars_checked = DEFAULT_YEARS
#  }
  
  #numeric variables check
  num_vars_checked <- num_vars[num_vars %in% AVAILABLE_NUM_VARS]
  num_vars_failed <- num_vars[!(num_vars %in% AVAILABLE_NUM_VARS)]
  #changed parenthesis here
  if(length(num_vars_failed) > 0){warning("Invalid numeric variable(s) excluded: ", paste(num_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid numeric variables supplied. Using default AGEP and PWGTP.")
    num_vars_checked = DEFAULT_NUM_VARS
  }
  
  #categorical variables check
  cat_vars_checked <- cat_vars[cat_vars %in% AVAILABLE_CAT_VARS]
  cat_vars_failed <- cat_vars[!(cat_vars %in% AVAILABLE_CAT_VARS)]
  #changed parenthesis here
  if(length(cat_vars_failed) > 0){warning("Invalid categorical variable(s) excluded: ", paste(cat_vars_failed))}
  if(length(num_vars_checked) == 0){
    warning("No valid categorical variables supplied. Using default SEX.")
    cat_vars_checked = DEFAULT_CAT_VARS
  }
  
  #geographic variables check
  geo_vars_checked <- geo_vars[geo_vars %in% AVAILABLE_GEO_VARS]
  geo_vars_failed <- geo_vars[!(geo_vars %in% AVAILABLE_GEO_VARS)]
  if(length(geo_vars_failed) > 0){warning("Invalid geographic variable(s) excluded: ", paste(geo_vars_failed))}
  if(length(geo_vars_checked) == 0){
    warning("No valid geographic variables supplied. Using default ALL.")
    geo_vars_checked = DEFAULT_GEO_VARS
  }
  
  #attempting change to tibble
  querystring_var_list = paste(c(num_vars_checked, cat_vars_checked, geo_vars_checked), collapse = ",")
  parsed_data <- fetch_census_raw(varstring = querystring_var_list)$content |> 
           parse_census_response()
  parsed_data <- as_tibble(parsed_data)
  return(parsed_data)
}

test <- fetch_census_data(num_vars = c("GASP"), cat_vars = c("FER"), geo_vars = c("ST"))


