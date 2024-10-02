library(ggplot2)

#writing the summary function 

summary.census <- function(test, num_vars = num_vars_checked, cat_vars = cat_vars_checked){
  weight_vector <- "PWGPT"
  sum_list <- list()
  
  sample_mean <- sum(num_vars*weight_vector)/sum(weight_vector)
  
  sample_sd <- sqrt(sum(num_vars*weight_vector)/sum(weight_vector)-sample_meanˆ2)
  
  return(list(sample_mean = sample_mean, sample_sd = sample_sd))
  
}





 # plot funtion
plot.census <- function(num_vars="AGEP", cat_vars="FER") {
  ggplot(test,
       aes(x = get(cat_vars), y = get(num_vars), weight = PWGTP)) +
    geom_boxplot() +
    labs(title = paste(num_vars, "by", cat_vars),
       x = cat_vars, y = num_vars)
}

plot.census("GASP","SEX")