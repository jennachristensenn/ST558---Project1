library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

source("./scripts/cat_var_mapping_tools.R")

PUMS_URL_MAIN_STUB <- "https://api.census.gov/data/"
PUMS_URL_ACS_STUB <- "/acs/acs1/pums"
PUMS_URL_QUERYSTRING_STUB <- "?for=state:02&get=PWGTP"

DEFAULT_YEARS <- c(2022)
#PWGTP is always included so is in base querystring stub
DEFAULT_NUM_VARS <- c("AGEP")
DEFAULT_CAT_VARS <- c("SEX")
DEFAULT_GEO_VARS <- c("REGION", "DIVISION", "ST")

#account for lack of 2020 data on site, then compare years as character for consistency
AVAILABLE_YEARS <- as.character(c(seq(2010, 2019), seq(2021, 2022)))
AVAILABLE_NUM_VARS <- c("PWGTP", "AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
AVAILABLE_TIME_VARS <- c("JWAP", "JWDP")
AVAILABLE_CAT_VARS <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
AVAILABLE_GEO_VARS <- c("REGION", "DIVISION", "ST")
 
fetch_census_raw <- function(year=2022, varstring="", geo_subset = ""){
  
  prepared_census_url <- paste(PUMS_URL_MAIN_STUB, year, PUMS_URL_ACS_STUB, PUMS_URL_QUERYSTRING_STUB, sep = "")
  if(nchar(varstring) > 0){prepared_census_url <-  paste(prepared_census_url, varstring, sep = ",")}
  if (nchar(geo_subset) > 0) {prepared_census_url <- paste(prepared_census_url, "&", geo_subset, sep = "")}
  
  census_resp <-GET(prepared_census_url)  
  
  if(census_resp$status_code != 200){
    stop(paste("One or more selected variables is not included in the years you have selected. Please consult the census microdata documentation to be sure the years you select support your chosen variables. The API response for year ", 
               year, 
               "failed to recognize variable", 
               strsplit(rawToChar(census_resp$content), " ")[[1]][5]))
  }
  
  return(census_resp$content)
}

parse_census_response <- function(census_raw){
  census_tbl_in_progress <- rawToChar(census_raw) |>  fromJSON()
  census_tbl <- as_tibble(census_tbl_in_progress[-1,]) 
  colnames(census_tbl) <- census_tbl_in_progress[1,]
  
  num_col <- intersect(colnames(census_tbl), AVAILABLE_NUM_VARS)
  time_col <- intersect(colnames(census_tbl), AVAILABLE_TIME_VARS)
  cat_col <- intersect(colnames(census_tbl), AVAILABLE_CAT_VARS)
  census_tbl <- census_tbl |> 
    mutate(across(all_of(num_col), as.numeric),
           across(all_of(time_col), ~ as.numeric(convert_census_time_strings(.x, cur_column()))),
           across(all_of(cat_col), ~ factorize_column(.x, cur_column())))
  
  class(census_tbl) <- c("census", class(census_tbl))
  
  return(census_tbl)
}

fetch_census_data <- function(
    years=DEFAULT_YEARS, 
    num_vars=DEFAULT_NUM_VARS, 
    cat_vars=DEFAULT_CAT_VARS, 
    geo_vars = NULL,
    geo_sub = NULL){
  num_vars_checked <- num_vars[num_vars %in% AVAILABLE_NUM_VARS]
  num_vars_failed <- num_vars[!(num_vars %in% AVAILABLE_NUM_VARS)]
  if(length(num_vars_failed > 0)){
    warning("Invalid numeric variable(s) excluded: ", paste(num_vars_failed))
  }
  if(length(num_vars_checked) == 0){
    warning("No valid numeric variables supplied. Using default AGEP and PWGTP.")
    num_vars_checked = DEFAULT_NUM_VARS
  }
  
  cat_vars_checked <- cat_vars[cat_vars %in% AVAILABLE_CAT_VARS]
  cat_vars_failed <- cat_vars[!(cat_vars %in% AVAILABLE_CAT_VARS)]
  if(length(cat_vars_failed > 0)){
    warning("Invalid categorical variable(s) excluded: ", paste(cat_vars_failed))
  }
  if(length(num_vars_checked) == 0){
    warning("No valid categorical variables supplied. Using default SEX.")
    cat_vars_checked = DEFAULT_CAT_VARS
  }
  
  years_checked <- as.character(years)[years %in% AVAILABLE_YEARS]
  years_failed <- as.character(years)[!(years %in% AVAILABLE_YEARS)]
  if(length(years_failed > 0)){
    warning("Invalid year(s) excluded: ", paste(years_failed))
  }
  if(length(years_checked) == 0){
    warning("No valid years supplied. Using default 2022.")
    years_checked = DEFAULT_CAT_VARS
  }
  
  geo_vars_checked <- geo_vars[geo_vars %in% AVAILABLE_GEO_VARS]
  geo_vars_failed <- geo_vars[!(geo_vars %in% AVAILABLE_GEO_VARS)]
  if(length(geo_vars_failed) > 0){
    warning("Invalid geographic variable(s) excluded: ", paste(geo_vars_failed))
  }
  if(length(geo_vars_checked) == 0){
    warning("No valid geographic variables supplied. Using default ALL.")
    geo_vars_checked = DEFAULT_GEO_VARS
  }
  
  if (is.null(geo_vars) || length(geo_vars_checked) == 0) {
    set_geo <- "for=state:02"  
  } else {
    if ("ST" %in% geo_vars_checked) {
      if (!is.null(geo_sub)) {
        set_geo <- paste0("for=state:", geo_sub)
      } else {
        set_geo <- "for=state:*"
      }
    } else if ("REGION" %in% geo_vars_checked) {
      if (!is.null(geo_sub)) {
        set_geo <- paste0("for=region:", geo_sub)  
      } else {
        set_geo <- "for=region:*" 
      }
    } else if ("DIVISION" %in% geo_vars_checked) {
      if (!is.null(goe_sub)) {
        set_geo <- paste0("for=division:", geo_sub)  
      } else {
        set_geo <- "for=division:*" 
      }
    }
  }
  
  querystring_var_list <-  paste(c(num_vars_checked, cat_vars_checked, geo_vars_checked), collapse = ",")
  
  fetch_census_single_year <- function(year){
    current_iter_response <- fetch_census_raw(year=year, varstring = querystring_var_list, geo_subset = set_geo) |> 
      parse_census_response()
    
    current_iter_response$YEAR <- as.numeric(year)
    return(current_iter_response)
  }
  
  return(map_dfr(years_checked, fetch_census_single_year))
}

test <- fetch_census_data(num_vars = c("JWDP"), cat_vars = c("SCHL"), years = c(2012, 2021), geo_sub = "state:11")
