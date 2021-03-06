library(tidyverse)
library(dataRetrieval)
library(CDECRetrieve)
library(lubridate)
library(stringr)
library(devtools)
library(forecast)


# helper functions ------------------------
# cdec flow = 20, temp = 25
# USGS flow = 00060, temp = 00010
usgs_clean_monthly_flow <- function(df, scrw_trp) {
  df %>%
    group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
    summarise(mean_flow_cfs = mean(X_00060_00003, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_flow_cfs, screw_trap)
}

usgs_clean_daily_flow <- function(df, scrw_trp) {
  df %>%
    select(date = Date, flow_cfs = X_00060_00003) %>%
    mutate(screw_trap = scrw_trp)
}


cdec_clean_monthly_flow <- function(df, scrw_trp) {
  df %>%
    group_by(year =  year(datetime), month = month(datetime), day = days_in_month(datetime)) %>%
    summarise(mean_flow_cfs = mean(parameter_value, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_flow_cfs, screw_trap)
}


cdec_clean_monthly_temp <- function(df, scrw_trp) {
  df %>%
    mutate(tempC = (parameter_value - 32) * 5/9) %>%
    group_by(year =  year(datetime), month = month(datetime), day = days_in_month(datetime)) %>%
    summarise(mean_temp_C = mean(tempC, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_temp_C, screw_trap)
}

cdec_clean_daily_temp <- function(df, scrw_trp) {
  df %>%
    group_by(date = as_date(datetime)) %>%
    summarise(mean_daily_tempC = mean((parameter_value - 32) * 5/9, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(screw_trap = scrw_trp)
}

cdec_clean_daily_flow <- function(df, scrw_trp) {
  df %>%
    group_by(date = as_date(datetime)) %>%
    summarise(flow_cfs = mean(parameter_value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(screw_trap = scrw_trp)
}

# Obtain flow and temperature data at screw trap locations-----

# Feather	1998	2016------
# CDEC FEATHER RIVER NEAR GRIDLEY - GRL
# flow 01/05/1999 to present
# temp 03/05/2003 to 06/01/2007
feather_flw <- CDECRetrieve::cdec_query(station = 'GRL', sensor_num = '20', dur_code = 'H',
                                         start_date = '1997-01-05', end_date = '2016-12-31')

feather_tmp <- CDECRetrieve::cdec_query(station = 'GRL', sensor_num = '25', dur_code = 'H',
                                         start_date = '2003-03-05', end_date = '2007-06-01')

ggplot(filter(feather_flw, parameter_value > 0), aes(x = datetime, y = parameter_value)) + geom_line()

feather_daily_flow <- cdec_clean_daily_flow(filter(feather_flw, parameter_value > 0), 'FEATHER')
feather_flow <- cdec_clean_monthly_flow(filter(feather_flw, parameter_value > 0), 'FEATHER') %>%
  filter(!is.na(date))

use_data(feather_flow, overwrite = TRUE)
use_data(feather_daily_flow, overwrite = TRUE)

ggplot(filter(feather_tmp, between(parameter_value, 40, 100)), aes(x = datetime, y = parameter_value)) + geom_line()

feather_daily_temp <- cdec_clean_daily_temp(filter(feather_tmp, between(parameter_value, 40, 100)), 'FEATHER') %>%
  filter(!is.na(date))

ggplot(feather_daily_temp, aes(date, mean_daily_tempC)) + geom_col()

use_data(feather_daily_temp, overwrite = TRUE)

fther_temp <- cdec_clean_monthly_temp(filter(feather_tmp, between(parameter_value, 40, 100)), 'FEATHER')
fther_temp %>%
  group_by(year(date)) %>%
  summarise(n())

fther_temp %>%
  filter(year(date) == 2003 | year(date) == 2006)
days_in_month(seq(as.Date('2006-10-01'), as.Date('2006-12-31'), by = 'month'))

missing_feather <- tibble(month = c(1:2, 10:12),
       year = c(rep(2003, 2), rep(2006, 3)),
       day = c(31, 28, 31, 30, 31 ),
       date = ymd(paste(year, month, day)),
       mean_temp_C = as.numeric(NA),
       screw_trap = 'FEATHER') %>%
  select(date, mean_temp_C, screw_trap)

fthr_tmp <- fther_temp %>%
  bind_rows(missing_feather) %>%
  arrange(date)

ts_fthr <- ts(fthr_tmp$mean_temp_C, start = c(2003, 1), end = c(2006, 12), frequency = 12)

na.interp(ts_fthr) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_fthr, series = 'Original')

feather_temp <- fthr_tmp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(ts_fthr)), mean_temp_C)) %>%
  filter(!is.na(date))

use_data(feather_temp, overwrite = TRUE)

# American	2013	2016----- (Adam asked for 2000-2014 spawning habitat)
# USGS 11446500 AMERICAN R A FAIR OAKS CA
# Temperature, water, degrees Celsius 	 1961-10-31 	 2018-04-17
# Discharge, cubic feet per second 	 1904-10-01 	 2018-04-16
amer_flw <- dataRetrieval::readNWISdv(siteNumbers = '11446500', parameterCd = '00060',
                                      startDate = '2000-01-01', endDate = '2016-12-31')
amer_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11446500', parameterCd = '00010',
                                      startDate = '2000-01-01', endDate = '2016-12-31', statCd = c('00001', '00002', '00008'))

amer_daily_flow <- usgs_clean_daily_flow(amer_flw, 'AMERICAN')
ggplot(amer_daily_flow, aes(date, flow_cfs)) + geom_line()
use_data(amer_daily_flow, overwrite = TRUE)

amer_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

amer_flow <- usgs_clean_monthly_flow(amer_flw, 'AMERICAN')
ggplot(amer_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(amer_flow, overwrite = TRUE)

amer_daily_temp <- amer_tmp %>%
  mutate(mean_daily_tempC = (X_00010_00001 + X_00010_00002)/2, screw_trap = 'AMERICAN') %>%
  select(date = Date, mean_daily_tempC, screw_trap)

ggplot(amer_daily_temp, aes(date, mean_daily_tempC)) + geom_line()

use_data(amer_daily_temp, overwrite = TRUE)

amer_temp <- amer_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  mutate(md = ifelse(is.nan(md), (mx + mn)/2, md), screw_trap = 'AMERICAN') %>%
  select(date, mean_temp_C = md, screw_trap)

ggplot(amer_temp, aes(date, mean_temp_C)) + geom_col()
use_data(amer_temp, overwrite = TRUE)

# Clear	1998	2016------
# USGS 11372000 CLEAR C NR IGO CA
# Temperature, water, degrees Celsius 	 1965-03-26 	 1979-01-28
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16
# CDEC CLEAR CREEK NEAR IGO - IGO
# flow 09/06/1996 to present
# temp 09/06/1996 to present
clear_flw <- CDECRetrieve::cdec_query(station = 'IGO', sensor_num = '20', dur_code = 'H',
                                        start_date = '1997-01-01', end_date = '2016-12-31')

clear_tmp <- CDECRetrieve::cdec_query(station = 'IGO', sensor_num = '25', dur_code = 'H',
                                        start_date = '1997-01-01', end_date = '2016-12-31')

clear_daily_flow <- cdec_clean_daily_flow(clear_flw, 'CLEAR') %>%
  filter(year(date) > 1996)
use_data(clear_daily_flow, overwrite = TRUE)
ggplot(clear_daily_temp, aes(date, mean_daily_tempC)) + geom_line()

clear_daily_temp <- cdec_clean_daily_temp(filter(clear_tmp, between(parameter_value, 32, 80), datetime >= as.Date('1997-01-01')), 'CLEAR')
use_data(clear_daily_temp, overwrite = TRUE)


clear_flw %>%
  filter(between(parameter_value, 0, 10000)) %>%
  ggplot(aes(datetime, parameter_value)) +
  geom_line()

clear_flow <- cdec_clean_monthly_flow(filter(clear_flw, between(parameter_value, 0, 10000), year(datetime) > 1996), 'CLEAR')
use_data(clear_flow, overwrite = TRUE)


clear_tmp %>%
  filter(between(parameter_value, 40, 80), datetime >= as.Date('1997-01-01')) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(tt = mean(parameter_value, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = tt)) +
  geom_col()

clr_tmp <- cdec_clean_monthly_temp(filter(clear_tmp, between(parameter_value, 40, 80), datetime >= as.Date('1997-01-01')), 'CLEAR')
ggplot(clr_tmp, aes(date, mean_temp_C)) + geom_line()

clr_tmp %>%
  summarise(start = min(date), end = max(date), n(), mnths = (year(end) - year(start) + 1)* 12)

clr_tmp %>%
  group_by(year(date)) %>%
  summarise(n())

clr_tmp %>%
  filter(year(date) == 1998)

clear_tmp <- clr_tmp %>%
  bind_rows(tibble(date = as.Date(c('1998-03-31', '1998-04-30')),
                   mean_temp_C = as.numeric(NA),
                   screw_trap = 'CLEAR')) %>%
  arrange(date)

ts_clr <- ts(clear_tmp$mean_temp_C, start = c(1997, 1), end = c(2016, 12), frequency = 12)

na.interp(ts_clr) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_clr, series = 'Original')

clear_tmp %>%
  group_by(year(date)) %>%
  summarise(n())

clear_temp <- clear_tmp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(ts_clr)), mean_temp_C))

use_data(clear_temp, overwrite = TRUE)


# Mok	1999	2015-----
# USGS 11323500 MOKELUMNE R BL CAMANCHE DAM CA
# Discharge, cubic feet per second 	 1904-10-01 	 2017-09-30
# water temp EBMUD Victor, CA
moke_flw <- dataRetrieval::readNWISdv(siteNumbers = '11323500', parameterCd = '00060',
                                      startDate = '1998-01-01', endDate = '2015-12-31')


moke_daily_flow <- usgs_clean_daily_flow(moke_flw, 'MOKELUMNE')
use_data(moke_daily_flow, overwrite = TRUE)
moke_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

moke_flow <- usgs_clean_monthly_flow(moke_flw, 'MOKELUMNE')
ggplot(moke_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(moke_flow, overwrite = TRUE)

victor <- read_csv('data-raw/Victor15min.csv')

moke_tmp <- victor %>%
  mutate(date = as.Date(Time, '%H:%M:%S %m/%d/%Y')) %>%
  group_by(year = year(date), month = month(date), day = days_in_month(date)) %>%
  summarise(mean_temp_C = mean(WaterTemperatureCelsius, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, day, sep = '-')), screw_trap = 'MOKELUMNE') %>%
  filter(between(year, 1998, 2015)) %>%
  select(date, mean_temp_C, screw_trap)

moke_daily_temp <- victor %>%
  group_by(date = as.Date(Time, '%H:%M:%S %m/%d/%Y')) %>%
  summarise(mean_daily_tempC = mean(WaterTemperatureCelsius, na.rm = TRUE)) %>%
  filter(between(year(date), 1998, 2015)) %>%
  ungroup() %>%
  mutate(screw_trap = 'MOKELUMNE') %>%
  select(date, mean_daily_tempC, screw_trap)

use_data(moke_daily_temp, overwrite = TRUE)

moke_tmp %>%
  filter(is.nan(mean_temp_C))

ts_mk <- ts(moke_tmp$mean_temp_C, start = c(1998, 1), end = c(2015, 12), frequency = 12)

na.interp(ts_mk) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_mk, series = 'Original')

moke_temp <- moke_tmp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(ts_mk)), mean_temp_C))

ggplot(moke_temp, aes(date, mean_temp_C)) + geom_col()
use_data(moke_temp, overwrite = TRUE)

# Stan	1998	2016-----
# USGS 11303000 STANISLAUS R A RIPON CA
# Temperature, water, degrees Celsius 	 1985-07-01 	 2018-04-17
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16
stan_flw <- dataRetrieval::readNWISdv(siteNumbers = '11303000', parameterCd = '00060',
                                      startDate = '1997-01-01', endDate = '2016-12-31')
stan_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11303000', parameterCd = '00010',
                                      startDate = '1997-01-01', endDate = '2016-12-31', statCd = c('00001', '00002', '00008'))
stan_flow %>% tail
stan_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()
stan_flow <- usgs_clean_monthly_flow(stan_flw, 'STANISLAUS')
ggplot(stan_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(stan_flow, overwrite = TRUE)

stan_daily_flow <- usgs_clean_daily_flow(stan_flw, 'STANISLAUS')
use_data(stan_daily_flow, overwrite = TRUE)

stan_daily_temp <- stan_tmp %>%
  mutate(mean_daily_tempC = (X_00010_00001 + X_00010_00002)/2, screw_trap = 'STANISLAUS') %>%
  select(date = Date, mean_daily_tempC, screw_trap)

use_data(stan_daily_temp, overwrite = TRUE)

stn_tmp <- stan_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  mutate(md = ifelse(is.nan(md), (mx + mn)/2, md), screw_trap = 'STANISLAUS') %>%
  select(date, mean_temp_C = md, screw_trap)

stn_tmp %>%
  group_by(year(date)) %>%
  summarise(n())

missing_stan <- tibble(date = as.Date(c('1997-04-30', '1997-05-31','1997-06-30',
                                        '2000-12-31', '2006-09-30')),
                       mean_temp_C = as.numeric(NA),
                       screw_trap = 'STANISLAUS')
st_tp <- stn_tmp %>%
  bind_rows(missing_stan) %>%
  arrange(date)

ts_stan <- ts(st_tp$mean_temp_C, start = c(1997, 1), end = c(2016, 12), frequency = 12)

na.interp(ts_stan) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_stan, series = 'Original')

stan_temp <- st_tp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(ts_stan)), mean_temp_C))

ggplot(stan_temp, aes(date, mean_temp_C)) + geom_col()
use_data(stan_temp, overwrite = TRUE)


# DATA FOR TRAPS WE AREN'T USING ### -----------------------------------------

# RBDD	1999	2016------
# Upper Sacramento not using because of location complications (too many tribs above)
# CDEC SACRAMENTO R AT RED BLUFF DIVERSION DAM - RDB
# temp 11/13/1989 to present
rbdd_tmp <- CDECRetrieve::cdec_query(station = 'RDB', sensor_num = '25', dur_code = 'H',
                                     start_date = '1998-01-01', end_date = '2016-12-31')

rbdd_tmp %>% glimpse
ggplot(rbdd_tmp, aes(x = year(datetime), y = parameter_value)) +
  geom_boxplot()

filter(rbdd_tmp, between(parameter_value, 40, 70)) %>%
  ggplot(aes(x = datetime, y = parameter_value)) + geom_line()

rbdd_daily_temp <- cdec_clean_daily_temp(filter(rbdd_tmp, between(parameter_value, 40, 70)), 'RBDD')
ggplot(rbdd_daily_temp, aes(date, mean_daily_tempC)) + geom_line()

rbdd_temp <- cdec_clean_monthly_temp(filter(rbdd_tmp, between(parameter_value, 40, 70), year(datetime) > 1997), 'RBDD')
ggplot(rbdd_temp, aes(date, mean_temp_C)) + geom_line()

# use_data(rbdd_temp, overwrite = TRUE)
# use_data(rbdd_daily_temp, overwrite = TRUE)

# USGS 11377100 SACRAMENTO R AB BEND BRIDGE NR RED BLUFF CA
# Discharge, cubic feet per second 	 1891-10-01 	 2018-04-16
rbdd_flw <- dataRetrieval::readNWISdv(siteNumbers = '11377100', parameterCd = '00060',
                                      startDate = '1998-01-01', endDate = '2016-12-31')

rbdd_flw %>%
  ggplot(aes(x=Date, y=X_00060_00003)) +
  geom_line()

rbdd_flow <- usgs_clean_monthly_flow(rbdd_flw, 'RBDD')

rbdd_daily_flow <- usgs_clean_daily_flow(rbdd_flw, 'RBDD')

ggplot(rbdd_daily_flow, aes(date, flow_cfs)) + geom_line()

# use_data(rbdd_daily_flow)
# use_data(rbdd_flow)


# Tuolumne	2007	2017-----
# Not using trap because too few captures
# USGS 11290000 TUOLUMNE R A MODESTO CA
# Temperature, water, degrees Celsius 	 1965-07-21 	 2018-04-17
# Discharge, cubic feet per second 	 1895-01-01 	 2018-04-16
tuol_flw <- dataRetrieval::readNWISdv(siteNumbers = '11290000', parameterCd = '00060',
                                      startDate = '2006-01-01', endDate = '2017-12-31')
tuol_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11290000', parameterCd = '00010',
                                      startDate = '2006-01-01', endDate = '2017-12-31', statCd = c('00001', '00002', '00008'))

tuol_flow <- usgs_clean_monthly_flow(tuol_flw, 'TUOLUMNE')
ggplot(tuol_flow, aes(date, mean_flow_cfs)) + geom_line()
use_data(tuol_flow, overwrite = TRUE)

tuol_daily_flow <- usgs_clean_daily_flow(tuol_flw, 'TUOLUMNE')
# use_data(tuol_daily_flow, overwrite = TRUE)


tuol_daily_temp <- tuol_tmp %>%
  mutate(mean_daily_tempC = (X_00010_00001 + X_00010_00002)/2, screw_trap = 'TUOLUMNE') %>%
  select(date = Date, mean_daily_tempC, screw_trap)

# use_data(tuol_daily_temp, overwrite = TRUE)

tuol_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  gather(stat, temp, -date) %>%
  ggplot(aes(x=date, y=temp, color = stat)) +
  geom_line()

tu_tmp <- tuol_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  mutate(md = ifelse(is.nan(md), (mx + mn)/2, md), screw_trap = 'TUOLUMNE') %>%
  select(date, mean_temp_C = md, screw_trap)

tu_tmp %>%
  group_by(year(date)) %>%
  summarise(n())

tu_tmp %>%
  filter(year(date) == 2010)

missing_tuol <- tibble(date = c(as.Date('2010-05-31'), as.Date('2010-11-30'), as.Date('2010-12-31')),
                       mean_temp_C = as.numeric(NA),
                       screw_trap = 'TUOLUMNE')
tu_tmp <- tu_tmp %>%
  bind_rows(missing_tuol) %>%
  arrange(date)

ts_tuol <- ts(tu_tmp$mean_temp_C, start = c(2007, 1), end = c(2017, 12), frequency = 12)

na.interp(ts_tuol) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_tuol, series = 'Original')

tuol_temp <- tu_tmp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(ts_tuol)), mean_temp_C))

ggplot(tuol_temp, aes(date, mean_temp_C)) + geom_col()

# use_data(tuol_temp, overwrite = TRUE)


# Battle	1998	2016------
# not using trap because no diversion data
# USGS 11376550 BATTLE C BL COLEMAN FISH HATCHERY NR COTTONWOOD CA
# Discharge, cubic feet per second 	 1961-10-01 	 2018-04-16
battle_flw <- dataRetrieval::readNWISdv(siteNumbers = '11376550', parameterCd = '00060',
                                        startDate = '1998-01-01', endDate = '2016-12-31')

battle_daily_flow <- usgs_clean_daily_flow(battle_flw, 'BATTLE')
# use_data(battle_daily_flow)

battle_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

battle_flow <- usgs_clean_monthly_flow(battle_flw, 'BATTLE')
ggplot(battle_flow, aes(date, mean_flow_cfs)) + geom_col()
# use_data(battle_flow)


# mike wright 5q data
cl_dates <- read_csv('data-raw/calLite_calSim_date_mapping.csv')

battle_tmp <- read_csv('data-raw/tempmaster.csv', skip = 1) %>%
  select(`5q_date`, `Battle Creek`) %>%
  mutate(day_month = str_sub(`5q_date`, 1, 6),
         year = str_sub(`5q_date`, 8, 9),
         year = str_c('20', year),
         date = dmy(paste(day_month, year))) %>%
  select(-day_month, -year, -`5q_date`) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_temp_F = mean(`Battle Creek`, na.rm = TRUE),
            mean_temp_C = (mean_temp_F - 32) * (5/9)) %>%
  ungroup() %>%
  mutate(cl_date = ymd(paste(year, month, 1)),
         screw_trap = 'BATTLE') %>%
  left_join(cl_dates) %>%
  select(date = cs_date, mean_temp_C, screw_trap) %>%
  filter(date >= as.Date('1998-01-01'))

ggplot(battle_tmp, aes(date, mean_temp_C)) + geom_col()

battle_tmp %>% tail

battle_missing <- tibble(
  date_seq = seq(as.Date('2003-10-01'), as.Date('2016-12-31'), by = 'month'),
  day = days_in_month(date_seq),
  date = ymd(paste(year(date_seq), month(date_seq), day)),
  mean_temp_C = as.numeric(NA),
  screw_trap = 'BATTLE') %>%
  select(date, mean_temp_C, screw_trap)

bt_tmp <- battle_tmp %>%
  bind_rows(battle_missing) %>%
  arrange(date)

bt_ts <- ts(bt_tmp$mean_temp_C, start = c(1998, 1), end = c(2016, 12), frequency = 12)

na.interp(bt_ts) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(bt_ts, series = 'Original')

battle_temp <- bt_tmp %>%
  mutate(mean_temp_C = ifelse(is.na(mean_temp_C), as.numeric(na.interp(bt_ts)), mean_temp_C))

ggplot(battle_temp, aes(date, mean_temp_C)) + geom_col()
# use_data(battle_temp)


# Deer 1992 2010 ---------
# not using trap because no trail efficiency data
# USGS 11383500 DEER C NR VINA CA
# Temperature, water, degrees Celsius 	 1998-10-05 	 2018-04-24
# Discharge, cubic feet per second 	 1911-10-01 	 2018-04-23
deer_flw <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00060',
                                      startDate = '1992-01-01', endDate = '2010-12-31')
deer_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00010',
                                      startDate = '1992-01-01', endDate = '2010-12-31', statCd = c('00001', '00002'))

deer_flw %>%
  filter(X_00060_00003 < 10000) %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()
deer_flow <- usgs_clean_monthly_flow(filter(deer_flw, X_00060_00003 < 10000), 'DEER')
ggplot(deer_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(deer_flow, overwrite = TRUE)

deer_daily_flow <- usgs_clean_daily_flow(filter(deer_flw, X_00060_00003 < 10000), 'DEER')
# use_data(deer_daily_flow)

deer_daily_temp <- deer_tmp %>%
  mutate(mean_daily_tempC = (X_00010_00001 + X_00010_00002)/2, screw_trap = 'DEER') %>%
  select(date = Date, mean_daily_tempC, screw_trap)

# use_data(deer_daily_temp)

deer_temp <- deer_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  # select(-year, -month, -day) %>%
  # gather(stat, temp, -date) %>%
  # ggplot(aes(x = date, y = temp, color = stat)) + geom_line()
  select(-year, -month, -day) %>%
  mutate(md = (mx + mn)/2, screw_trap = 'DEER') %>%
  select(date, mean_temp_C = md, screw_trap)

# use_data(deer_temp,overwrite = TRUE)


# Chipps Trawl 	2008	2011 *********** ----------
# (corrected assignments)
# Proposed "Good" fish monitoring data	2008	2018 OR 	2003	2018 ******

