library(httr)
library(hms)
library(dplyr)
library(stringr)

vAR_URL_STUB = "https://api.census.gov/data/2022/acs/acs1/pums/variables/"

#it probably doesn't make much sense to use JWDP as default, so we can change
extract_var_mappings <- function(var_name="JWDP"){
  var_resp <- GET(paste(vAR_URL_STUB, var_name, ".json", sep = ""))
  
  mapping <- NULL
  
  if(var_resp$status_code == 200){
    mapping_raw=var_resp$content
    mapping <- fromJSON(rawToChar(mapping_raw))$values$item
  }

  return(mapping)
}

process_census_time_mapping <- function(strings){
  begin_times_raw <- substring(strings, 1, 10) |> 
    str_replace_all(c("a\\.m\\." = "AM", "p\\.m\\." = "PM"))
  begin_times <- as.numeric(format(strptime(begin_times_raw, format = "%I:%M %p"), "%H%M"))
    
  end_times_raw <- substring(strings, 14, 25) |> 
    str_replace_all(c("a\\.m\\." = "AM", "p\\.m\\." = "PM"))
  end_times <- as.numeric(format(strptime(end_times_raw, format = "%I:%M %p"), "%H%M"))
  
  times_converted <- end_times - .5 * (end_times - begin_times)
  names(times_converted) <- names(strings)
  
  return(times_converted)
}

convert_census_time_strings <- function(strings, var){
  times_converted <- extract_var_mappings(var) |> 
    process_census_time_mapping()
  return(sapply(strings, function(x) times_converted[x]))
}

factorize_column <- function(strings, var){
  mapping <- extract_var_mappings(var)
  
  if(is.null(mapping)){
    return(rep_len(NA_character_, length(strings)))
  }
  
  return(factor(strings, levels = names(mapping), labels = as.character(mapping)))
}