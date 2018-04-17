library(tidyverse)
library(lubridate)

# complete data sets----
amer_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)
rbdd_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)


clear_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)

feather_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)

moke_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)

stan_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)

tuol_temp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)
