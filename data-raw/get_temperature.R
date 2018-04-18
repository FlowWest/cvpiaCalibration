library(tidyverse)
library(lubridate)
library(forecast)

# complete data sets----

# need imputation----
tuol_temp %>%
  group_by(year(date)) %>%
  summarise(n())


