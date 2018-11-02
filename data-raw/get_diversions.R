library(cvpiaFlow)
library(tidyverse)
library(lubridate)
library(readxl)
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


# Feather	1998	2016----
feat_tot_div <- feat_prop_div %>%
  left_join(feather_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap) %>%
  filter(!is.na(tot_div))

use_data(feat_tot_div, overwrite = TRUE)

ggplot(feat_tot_div, aes(date, tot_div)) + geom_line()



# American	2013	2016----
amer_tot_div <- amer_prop_div %>%
  left_join(amer_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(amer_tot_div, aes(date, tot_div)) + geom_line()

use_data(amer_tot_div, overwrite = TRUE)


# Clear	1998	2016----
# NO diversions

# Mok	1999	2015----
# CORRESPONDENCE *******
# Attached are diversion data for the Lower Mokelumne River. I just summed the
# acre feet and converted the acre foot per month to cfs
# (Diversions_AF*0.016564351). Can you please put this in the CVPIA calibration
# files? Also, are the diversion values in the calibration R package in cfs? So,
# to get proportion water diverted I would simply divide the total water
# diverted (in cfs) by the mean monthly flow, right?


# One question we had: Did the Lodi water diversions not start till 2013 or was this information just not recorded until 2013?

# I just read through a few reports and it looks like they completed
# construction of the surface water treatment plant in November 2012. The report
# states that they took 22 AF in November 2012 and 75 AF in December
# 2012…probably for testing. I’m not sure why those #’s didn’t show up in our
# database, but now you have them!

readxl::excel_sheets('data-raw/lower_mokelumne_diversions.xlsx')

lodi <- read_excel('data-raw/lower_mokelumne_diversions.xlsx', sheet = 'Lodi WTP (AF)', skip = 1)
wid <- read_excel('data-raw/lower_mokelumne_diversions.xlsx', sheet = 'WID canal (AF)')
rip <- read_excel('data-raw/lower_mokelumne_diversions.xlsx', sheet = 'Riparian diverters (AF)')

moke <- bind_rows(lodi, wid, rip)
glimpse(moke)

acre_ft_per_day_to_cfs <- function(af_per_day) {
  ft3_per_day <- af_per_day * 43560
  cfs <- ft3_per_day / 60 / 60 / 24
  return(cfs)
}

acre_ft_per_day_to_cms <- function(af_per_day) {
  m3_per_day <- af_per_day * 0.0142764101568
  cms <- m3_per_day / 60 / 60 / 24
  return(cms)
}


moke_tot_div <- moke %>%
  select(station = Station, date = `Reading Date`, monthly_acre_ft = Reading) %>%
  mutate(date = as.Date(date)) %>%
  bind_rows(tibble(
    station = 'Lodi WTP',
    date = c(ymd('2012-11-30'), ymd('2012-12-31')),
    monthly_acre_ft = c(22, 75)
  )) %>%
  spread(station, monthly_acre_ft) %>%
  rename(lodi = `Lodi WTP`, rip = `Riparian and Appropriative Diverters`, wid = `Woodbridge Irrigation District Canal`) %>%
  mutate(tot_div_acre_ft = ifelse(is.na(lodi), rip + wid, lodi + rip + wid),
         acre_ft_per_day = tot_div_acre_ft/days_in_month(date),
         cms = acre_ft_per_day_to_cms(acre_ft_per_day),
         screw_trap = 'MOKELUMNE') %>%
  select(date, tot_div_cms = cms, screw_trap)

ggplot(moke_tot_div, aes(date, tot_div_cms)) + geom_col()

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

# Not using anymore ------------------
# RBDD	1999	2016----
rbdd_tot_div <- rbdd_prop_div %>%
  left_join(rbdd_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(rbdd_tot_div, aes(date, tot_div)) + geom_line()

use_data(rbdd_tot_div, overwrite = TRUE)

# No diversions


# Tuolumne	2007	2017 ----
tuol_tot_div <- tuol_prop_div %>%
  left_join(tuol_flow) %>%
  mutate(tot_div = mean_flow_cfs * prop_div) %>%
  select(date, tot_div, screw_trap)

ggplot(tuol_tot_div, aes(date, tot_div)) + geom_line()

use_data(tuol_tot_div, overwrite = TRUE)
# Battle	1998	2016----
# NO diversions
