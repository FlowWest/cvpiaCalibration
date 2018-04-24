library(cvpiaFlow)
library(tidyverse)
library(lubridate)
library(forecast)
library(waterYearType)
library(devtools)

wys <- water_years %>%
  filter(location == 'Sacramento Valley') %>%
  mutate(WY = as.numeric(WY)) %>%
  select(WY, Yr_type) %>%
  bind_rows(tibble(WY = 2017,
                   Yr_type = factor('Wet', levels = c('Critical', 'Dry', 'Below Normal', 'Above Normal', 'Wet'))))

seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1


# RBDD	1999	2016----
rbdd_tot_div <- rbdd_prop_div %>%
  left_join(rbdd_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(rbdd_tot_div, aes(date, tot_div)) + geom_line()

use_data(rbdd_tot_div, overwrite = TRUE)

# No diversions

# Feather	1998	2016----
feat_tot_div <- feat_prop_div %>%
  left_join(feather_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap) %>%
  filter(!is.na(tot_div))

use_data(feat_tot_div, overwrite = TRUE)

ggplot(feat_tot_div, aes(date, tot_div)) + geom_line()

# Tuolumne	2007	2017 ----
tuol_tot_div <- tuol_prop_div %>%
  left_join(tuol_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(tuol_tot_div, aes(date, tot_div)) + geom_line()

use_data(tuol_tot_div, overwrite = TRUE)

# American	2013	2016----
amer_tot_div <- amer_prop_div %>%
  left_join(amer_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(amer_tot_div, aes(date, tot_div)) + geom_line()

use_data(amer_tot_div, overwrite = TRUE)

# Battle	1998	2016----
# NO diversions

# Clear	1998	2016----
# NO diversions

# Mok	1999	2015----
moke_tot_div <- moke_prop_div %>%
  left_join(moke_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(moke_tot_div, aes(date, tot_div)) + geom_line()

use_data(moke_tot_div, overwrite = TRUE)

# Stan	1998	2016----
stan_tot_div <- stan_prop_div %>%
  left_join(stan_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(stan_tot_div, aes(date, tot_div)) + geom_col()

use_data(stan_tot_div, overwrite = TRUE)

# Deer 1992 2010
deer_tot_div <- deer_prop_div %>%
  left_join(deer_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(deer_tot_div, aes(date, tot_div)) + geom_col()

use_data(deer_tot_div, overwrite = TRUE)
