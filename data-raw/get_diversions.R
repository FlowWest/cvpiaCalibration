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
cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Upper Sacramento River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(year(date) >= 1999) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Upper Sacramento River`) %>%
  group_by(month = month(date)) %>%
  summarise( min = min(tot_div, na.rm = TRUE), q25 = quantile(tot_div, .25, na.rm = TRUE), median = median(tot_div, na.rm = TRUE),
             mean = mean(tot_div, na.rm = TRUE), q75 = quantile(tot_div, .75, na.rm = TRUE), max = max(tot_div, na.rm = TRUE),
             sd = sd(tot_div, na.rm = TRUE), total = n())

# No diversions

# Feather	1998	2016----
cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Feather River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(year(date) >= 1998) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

feat_tot_div <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Feather River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(tot_div = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         screw_trap = 'FEATHER') %>%
  filter(year(date) >= 1998) %>%
  ungroup() %>%
  select(date, tot_div, screw_trap)

use_data(feat_tot_div)

cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Feather River`) %>%
  group_by(month = month(date)) %>%
  summarise( min = min(tot_div, na.rm = TRUE), q25 = quantile(tot_div, .25, na.rm = TRUE), median = median(tot_div, na.rm = TRUE),
             mean = mean(tot_div, na.rm = TRUE), q75 = quantile(tot_div, .75, na.rm = TRUE), max = max(tot_div, na.rm = TRUE),
             sd = sd(tot_div, na.rm = TRUE), total = n())

# Tuolumne	2007	2017 ----
cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Tuolumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2018-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(year(date) >= 2007) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

tuol_tot_div <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Tuolumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2018-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(tot_div = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         screw_trap = 'TUOLUMNE') %>%
  filter(year(date) >= 2007) %>%
  ungroup() %>%
  select(date, tot_div, screw_trap)

use_data(tuol_tot_div)

cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Tuolumne River`) %>%
  group_by(month = month(date)) %>%
  summarise( min = min(tot_div, na.rm = TRUE), q25 = quantile(tot_div, .25, na.rm = TRUE), median = median(tot_div, na.rm = TRUE),
             mean = mean(tot_div, na.rm = TRUE), q75 = quantile(tot_div, .75, na.rm = TRUE), max = max(tot_div, na.rm = TRUE),
             sd = sd(tot_div, na.rm = TRUE), total = n())

# American	2013	2016----
dd <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `American River`) %>%
  mutate(WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date)),
         month = month(date)) %>%
  left_join(wys)

cor(dd$tot_div, as.numeric(dd$Yr_type), use = 'complete.obs')
cor(dd$tot_div, as.numeric(dd$month), use = 'complete.obs')

cvpiaFlow::total_diverted %>%
  select(date, tot_div = `American River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(year(date) >= 2013) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

amer_tot_div <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `American River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(tot_div = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         screw_trap = 'AMERICAN') %>%
  filter(year(date) >= 2013) %>%
  ungroup() %>%
  select(date, tot_div, screw_trap)

use_data(amer_tot_div)

# Battle	1998	2016----
# NO diversions

# Clear	1998	2016----
# NO diversions

# Mok	1999	2015----
cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Mokelumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(between(year(date), 1999, 2015)) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

moke_tot_div <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Mokelumne River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2016-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(tot_div = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         screw_trap = 'MOKELUMNE') %>%
  filter(year(date) >= 1999) %>%
  ungroup() %>%
  select(date, tot_div, screw_trap)

use_data(moke_tot_div, overwrite = TRUE)

# Stan	1998	2016----
cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Stanislaus River`) %>%
  group_by(month = month(date)) %>%
  summarise( min = min(tot_div, na.rm = TRUE), q25 = quantile(tot_div, .25, na.rm = TRUE), median = median(tot_div, na.rm = TRUE),
             mean = mean(tot_div, na.rm = TRUE), q75 = quantile(tot_div, .75, na.rm = TRUE), max = max(tot_div, na.rm = TRUE),
             sd = sd(tot_div, na.rm = TRUE), total = n())

cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Stanislaus River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  group_by(month = month(date)) %>%
  mutate(filled = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         WY = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(filled2 = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div)) %>%
  filter(between(year(date), 1998, 2016)) %>%
  ggplot(aes(x = date, y = filled2, fill = Yr_type)) +
  geom_col() +
  geom_vline(xintercept = as.Date('2003-10-31'), size = 1) +
  scale_fill_brewer(palette = 'RdPu') +
  theme_dark()

stan_tot_div <- cvpiaFlow::total_diverted %>%
  select(date, tot_div = `Stanislaus River`) %>%
  bind_rows(tibble(date = seq(as.Date('2003-11-01'), as.Date('2017-01-01'), by= 'month')-1,
                   tot_div = NA)) %>%
  mutate(month = month(date), WY = ifelse(month %in% 10:12, year(date) + 1, year(date))) %>%
  left_join(wys) %>%
  group_by(month, Yr_type) %>%
  mutate(tot_div = ifelse(is.na(tot_div), median(tot_div, na.rm = TRUE), tot_div),
         screw_trap = 'STANISLAUS') %>%
  filter(year(date) >= 1998) %>%
  ungroup() %>%
  select(date, tot_div, screw_trap)

ggplot(stan_tot_div, aes(date, tot_div)) + geom_col()

use_data(stan_tot_div)

