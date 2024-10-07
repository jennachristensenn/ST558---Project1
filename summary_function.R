library(ggplot2)
library(dplyr)

# summary function 
summary.census <- function(census_tbl = test, num_vars, cat_vars){ 
  
  num_summary <- census_tbl |>
    mutate(across(all_of(num_vars),
                  .fns = list(
                    mean = function(x) {sum(x*PWGTP, na.rm = TRUE) / sum(PWGTP, na.rm = TRUE)},
                    sd = function(x) {sqrt(sum(x^2*PWGTP, na.rm = TRUE) / sum(PWGTP, na.rm = TRUE) - 
                           (sum(x*PWGTP, na.rm = TRUE) / sum(PWGTP, na.rm = TRUE))^2)}),
                    .names = "{.col}_{.fn}")) |>
    list()
  
  cat_summary <- census_tbl |>
    group_by(across(all_of(cat_vars))) |>
    summarize(count = n()) |>
    list() 
  
  values_list <- list(
    "Numeric Variable Summary" = num_summary, 
    "Categorical Variable Counts" = cat_summary) 
  
  return(values_list)
  
}

test_summary <- summary.census(test, num_vars = c("GASP", "AGEP"), cat_vars = c("FER", "SEX"))
print(test_summary)



 # plot function
plot.census <- function(num_vars="AGEP", cat_vars="SEX") {
  ggplot(test_tbl,
       aes(x = get(cat_vars), y = get(num_vars), weight = PWGTP)) +
    geom_boxplot() +
    labs(title = paste(num_vars, "by", cat_vars),
         x = cat_vars, y = num_vars)
}

plot.census()