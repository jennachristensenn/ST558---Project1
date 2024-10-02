library(ggplot2)
library(dplyr)

#writing the summary function 

summary.census <- function(census_tbl = test, cat_vars){ #num_vars,
  

  
  cat_summary <- census_tbl |>
    group_by(across(all_of(cat_vars))) |>
    summarize(count = n()) |>
    ungroup() |>
    list() 
  
  values_list <- c(cat_summary) #num_summary,
  
  return(values_list)
  
}

test_summary <- summary.census(test, c("FER", "SEX"))
print(test_summary)







 # plot funtion
plot.census <- function(num_vars="AGEP", cat_vars="FER") {
  ggplot(test,
       aes(x = get(cat_vars), y = get(num_vars), weight = PWGTP)) +
    geom_boxplot() +
    labs(title = paste(num_vars, "by", cat_vars),
       x = cat_vars, y = num_vars)
}

plot.census("GASP","SEX")