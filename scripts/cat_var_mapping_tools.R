library(httr)

vAR_URL_STUB = "https://api.census.gov/data/2022/acs/acs1/pums/variables/"

#it probably doesn't make much sense to use JWDP as default, so we can change
extract_var_mappings <- function(var_name="JWDP"){
  mapping_raw=GET(paste(vAR_URL_STUB, var_name, ".json", sep = ""))$content
  
  mapping <- fromJSON(rawToChar(mapping_raw))$values$item
  return(mapping)
}

test <- extract_var_mappings()
test["100"]