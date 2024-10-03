library(dplyr)
library(tidyr)
library(lubridate)

data <- tibble(
  id = 258,
  time_range = "9:40 p.m. to 9:44 p.m."
)

data_separated <- data %>%
  separate_wider_delim(time_range, delim = " to ", names = c("start_time", "end_time"))

data_parsed <- data_separated %>%
  mutate(
    start_time = parse_date_time(start_time, orders = "I:M p"),
    end_time = parse_date_time(end_time, orders = "I:M p")
  )

data_midpoint <- data_parsed %>%
  mutate(
    midpoint = start_time + (end_time - start_time) / 2
  )

print(data_midpoint)