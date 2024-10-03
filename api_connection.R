library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

PUMS_URL_MAIN_STUB <- "https://api.census.gov/data/"
PUMS_URL_ACS_STUB <- "/acs/acs1/pums"
PUMS_URL_QUERYSTRING_STUB <- "?get=PWGTP" #?for=state:02&get=PWGTP


DEFAULT_YEARS <- c(2022)
#PWGTP is always included so is in base querystring stub
DEFAULT_NUM_VARS <- c("AGEP")
DEFAULT_CAT_VARS <- c("SEX")
DEFAULT_GEO_VARS <- c("REGION", "DIVISION", "ST") 

#account for lack of 2020 data on site, then compare years as character for consistency
AVAILABLE_YEARS <- as.character(c(seq(2010, 2019), seq(2021, 2022)))
AVAILABLE_NUM_VARS <- c("PWGTP", "AGEP", "GASP", "JWAP", "JWDP", "GRPIP", "JWMNP") 
AVAILABLE_CAT_VARS <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
AVAILABLE_GEO_VARS <- c("REGION", "DIVISION", "ST")

VAR_URL_STUB = "https://api.census.gov/data/2022/acs/acs1/pums/variables/"

# Function for single variable
extract_var_mapping <- function(var_name){
  endpoint_url <- paste0(VAR_URL_STUB, var_name, ".json")
  response <- GET(endpoint_url) 
  mapping_raw <- rawToChar(response$content)
  #print(mapping_raw)
  mapping <- fromJSON(mapping_raw)
  return(mapping$values$item)
}

#Function for multiple vars
extract_multiple_var_mappings <- function(var_list) {
  mappings <- list()  
  for (var_name in var_list) {
    mappings[[var_name]] <- extract_var_mapping(var_name)
  }
  return(mappings)  
}


fetch_census_raw <- function(year=2022, varstring="",  geo_subset = ""){
  
  prepared_census_url <- paste(PUMS_URL_MAIN_STUB, year, PUMS_URL_ACS_STUB, PUMS_URL_QUERYSTRING_STUB, sep = "")
  if(nchar(varstring) > 0){prepared_census_url <-  paste(prepared_census_url, varstring, sep = ",")}
  if (nchar(geo_subset) > 0) {prepared_census_url <- paste(prepared_census_url, "&", geo_subset, sep = "")}
  
  print(prepared_census_url) #debugging
  return(GET(prepared_census_url))
}

#fetch_census_raw()

parse_census_response <- function(census_raw){
  census_tbl_in_progress <- rawToChar(census_raw) |>  fromJSON()
  census_tbl <- as_tibble(census_tbl_in_progress[-1,])
  colnames(census_tbl) <- census_tbl_in_progress[1,]
  
  num_col <- intersect(colnames(census_tbl), AVAILABLE_NUM_VARS)
  cat_col <- intersect(colnames(census_tbl), AVAILABLE_CAT_VARS)
  census_tbl <- census_tbl |> 
    mutate(across(all_of(num_col), as.numeric),
           across(all_of(cat_col), as.factor))
  
  class(census_tbl) <- c("census", class(census_tbl))
  
  return(census_tbl)
}

fetch_census_data <- function(years = DEFAULT_YEARS, 
                              num_vars = DEFAULT_NUM_VARS, 
                              cat_vars = DEFAULT_CAT_VARS, 
                              geo_vars = NULL,   # Default is now State
                              geo_sub = NULL) {   # Default is now state:02 (Alaska)
  
  # Check numeric vars
  num_vars_checked <- num_vars[num_vars %in% AVAILABLE_NUM_VARS]
  num_vars_failed <- num_vars[!(num_vars %in% AVAILABLE_NUM_VARS)]
  if (length(num_vars_failed > 0)) {
    warning("Invalid numeric variable(s) excluded: ", paste(num_vars_failed))
  }
  if (length(num_vars_checked) == 0) {
    warning("No valid numeric variables supplied. Using default AGEP and PWGTP.")
    num_vars_checked <- DEFAULT_NUM_VARS
  }
  
  # Check categorical vars
  cat_vars_checked <- cat_vars[cat_vars %in% AVAILABLE_CAT_VARS]
  cat_vars_failed <- cat_vars[!(cat_vars %in% AVAILABLE_CAT_VARS)]
  if (length(cat_vars_failed > 0)) {
    warning("Invalid categorical variable(s) excluded: ", paste(cat_vars_failed))
  }
  if (length(cat_vars_checked) == 0) {
    warning("No valid categorical variables supplied. Using default SEX.")
    cat_vars_checked <- DEFAULT_CAT_VARS
  }
  
  # Check years
  years_checked <- as.character(years)[years %in% AVAILABLE_YEARS]
  years_failed <- as.character(years)[!(years %in% AVAILABLE_YEARS)]
  if (length(years_failed > 0)) {
    warning("Invalid year(s) excluded: ", paste(years_failed))
  }
  if (length(years_checked) == 0) {
    warning("No valid years supplied. Using default 2022.")
    years_checked <- DEFAULT_YEARS
  }
  
  # Check geo vars and subset
  geo_vars_checked <- geo_vars[geo_vars %in% AVAILABLE_GEO_VARS]
  geo_vars_failed <- geo_vars[!(geo_vars %in% AVAILABLE_GEO_VARS)]
  if (length(geo_vars_failed) > 0) {
    warning("Invalid geographic variable(s) excluded: ", paste(geo_vars_failed))
  }
  
  # default to state:02 
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
  
  querystring_var_list <- paste(c(num_vars_checked, cat_vars_checked), collapse = ",")
  
  cat_var_labels <- extract_multiple_var_mappings(cat_vars_checked)
  
  fetch_census_single_year <- function(year) {
    # Fetch data for a single year
    current_iter_response <- fetch_census_raw(year = year, varstring = querystring_var_list, geo_subset = set_geo)$content |> 
      parse_census_response()
    
    current_iter_response$YEAR <- year
    
    for (cat_var in cat_vars_checked) {
      if (!is.null(cat_var_labels[[cat_var]])) {
        current_iter_response[[cat_var]] <- factor(current_iter_response[[cat_var]], 
                                                   levels = names(cat_var_labels[[cat_var]]),
                                                   labels = cat_var_labels[[cat_var]])
      }
    }
    
    return(current_iter_response)
  }
  
  return(map_dfr(years_checked, fetch_census_single_year))
}

test <- fetch_census_data(num_vars = c("GASP", "AGEP"), cat_vars = c("HISPEED"), years = c(2014))

print(head(test))