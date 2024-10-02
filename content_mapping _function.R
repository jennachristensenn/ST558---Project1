library(httr)
library(jsonlite)

vAR_URL_STUB = "https://api.census.gov/data/2022/acs/acs1/pums/variables/"

# Function for single variable
extract_var_mapping <- function(var_name){
  
  url <- paste0(vAR_URL_STUB, var_name, ".json")
  
  response <- GET(url) 
  
  mapping_raw <- rawToChar(response$content)
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

var_list <- c("FER", "SEX") 

test_var <- extract_multiple_var_mappings(var_list)

print(test_var$FER)
