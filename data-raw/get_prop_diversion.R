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
# No diversions
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Upper Sacramento River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  filter(year(date) >= 1998) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

rbdd_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Upper Sacramento River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'RBDD') %>%
  filter(year(date) >= 1999) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

ggplot(rbdd_prop_div, aes(date, prop_div)) + geom_col()

use_data(rbdd_prop_div, overwrite = TRUE)

# Feather	1998	2016----
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Feather River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  filter(year(date) >= 1998) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

feat_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Feather River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'FEATHER') %>%
  filter(year(date) >= 1998) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

use_data(feat_prop_div)

# Tuolumne	2007	2017 ----
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Tuolumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2018-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  # filter(year(date) >= 2007) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

tuol_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Tuolumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2018-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'TUOLUMNE') %>%
  filter(year(date) >= 2007) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

use_data(tuol_prop_div)

# American	2013	2016----
dd <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `American River`) %>%
  mutate(WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         month = month(date)) %>%
  left_join(wys)

cor(dd$prop_div, as.numeric(dd$Yr_type), use = 'complete.obs')
cor(dd$prop_div, as.numeric(dd$month), use = 'complete.obs')

cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `American River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  # filter(year(date) >= 2013) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

amer_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `American River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'AMERICAN') %>%
  filter(year(date) >= 2013) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

use_data(amer_prop_div)

# Battle	1998	2016----
# NO diversions

# Clear	1998	2016----
# NO diversions

# Mok	1999	2015----
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Mokelumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  filter(between(year(date), 1999, 2015)) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

moke_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Mokelumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2016-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'MOKELUMNE') %>%
  filter(year(date) >= 1999) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

use_data(moke_prop_div, overwrite = TRUE)


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


# Stan	1998	2016----
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Stanislaus River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  filter(between(year(date), 1998, 2016)) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

stan_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Stanislaus River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'STANISLAUS') %>%
  filter(year(date) >= 1998) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

ggplot(stan_prop_div, aes(date, prop_div)) + geom_col()

use_data(stan_prop_div)

# Deer 1992 2010
cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Deer Creek`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2011-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div)) %>%
  filter(between(year(date), 1992, 2010)) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

deer_prop_div <- cvpiaFlow::proportion_diverted %>%
  select(date, prop_div = `Deer Creek`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2011-01-01'), by= 'month')-1,
                   prop_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(prop_div = ifelse(is.na(prop_div), median(prop_div, na.rm = TRUE), prop_div),
         screw_trap = 'DEER') %>%
  filter(year(date) >= 1992) %>%
  ungroup() %>%
  select(date, prop_div, screw_trap)

ggplot(deer_prop_div, aes(date, prop_div)) + geom_col()

use_data(deer_prop_div)
