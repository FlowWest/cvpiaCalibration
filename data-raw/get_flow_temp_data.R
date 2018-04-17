library(tidyverse)
library(dataRetrieval)
library(CDECRetrieve)
library(lubridate)

# cdec flow = 20, temp = 25
# USGS flow = 00060, temp = 00010
usgs_clean_flow <- function(df, scrw_trp) {
  df %>%
    group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
    summarise(mean_flow_cfs = mean(X_00060_00003, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_flow_cfs, screw_trap)
}

cdec_clean_flow <- function(df, scrw_trp) {
  df %>%
    group_by(year =  year(datetime), month = month(datetime), day = days_in_month(datetime)) %>%
    summarise(mean_flow_cfs = mean(parameter_value, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_flow_cfs, screw_trap)
}

cdec_clean_temp <- function(df, scrw_trp) {
  df %>%
    mutate(tempC = (parameter_value - 32) * 5/9) %>%
    group_by(year =  year(datetime), month = month(datetime), day = days_in_month(datetime)) %>%
    summarise(mean_temp_C = mean(tempC, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, month, day)), screw_trap = scrw_trp) %>%
    ungroup() %>%
    select(date, mean_temp_C, screw_trap)
}

# Obtain flow and temperature data at screw trap locations-----

# RBDD	1999	2016------
# CDEC SACRAMENTO R AT RED BLUFF DIVERSION DAM - RDB
# temp 11/13/1989 to present
rbdd_tmp <- CDECRetrieve::cdec_query(station = 'RDB', sensor_num = '25', dur_code = 'H',
                                      start_date = '1999-01-01', end_date = '2016-12-31')
rbdd_tmp %>%
  filter(parameter_value > 32) %>%
  ggplot(aes(x = datetime, y = parameter_value)) +
  geom_line()

rbdd_temp <- cdec_clean_temp(filter(rbdd_tmp, parameter_value > 32, year(datetime) > 1998), 'RBDD')

use_data(rbdd_temp, overwrite = TRUE)

# USGS 11377100 SACRAMENTO R AB BEND BRIDGE NR RED BLUFF CA
# Discharge, cubic feet per second 	 1891-10-01 	 2018-04-16
rbdd_flw <- dataRetrieval::readNWISdv(siteNumbers = '11377100', parameterCd = '00060',
                                       startDate = '1999-01-01', endDate = '2016-12-31')

rbdd_flw %>%
  ggplot(aes(x=Date, y=X_00060_00003)) +
  geom_line()

rbdd_flow <- usgs_clean_flow(rbdd_flw, 'RBDD')

use_data(rbdd_flow)

# Feather	1998	2016------
# CDEC FEATHER RIVER NEAR GRIDLEY - GRL
# flow 01/05/1999 to present
# temp 03/05/2003 to 06/01/2007
feather_flw <- CDECRetrieve::cdec_query(station = 'GRL', sensor_num = '20', dur_code = 'H',
                                         start_date = '1999-01-05', end_date = '2016-12-31')

feather_tmp <- CDECRetrieve::cdec_query(station = 'GRL', sensor_num = '25', dur_code = 'H',
                                         start_date = '2003-03-05', end_date = '2007-06-01')

feather_flw %>%
  filter(parameter_value > 0) %>%
  # filter(between(parameter_value, 40, 100)) %>%
  ggplot(aes(x = datetime, y = parameter_value)) +
  geom_line()

feather_flow <- cdec_clean_flow(feather_flw, 'FEATHER')
use_data(feather_flow)

feather_tmp %>%
  filter(between(parameter_value, 40, 100)) %>%
  ggplot(aes(x = datetime, y = parameter_value)) +
  geom_line()

feather_temp <- cdec_clean_temp(filter(feather_tmp, between(parameter_value, 40, 100)), 'FEATHER')
use_data(feather_temp)

# Tuolumne	2007	2017-----
# USGS 11290000 TUOLUMNE R A MODESTO CA
# Temperature, water, degrees Celsius 	 1965-07-21 	 2018-04-17
# Discharge, cubic feet per second 	 1895-01-01 	 2018-04-16
tuol_flw <- dataRetrieval::readNWISdv(siteNumbers = '11290000', parameterCd = '00060',
                                      startDate = '2007-01-01', endDate = '2017-12-31')
tuol_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11290000', parameterCd = '00010',
                                      startDate = '2007-01-01', endDate = '2017-12-31', statCd = c('00001', '00002', '00008'))

tuol_flow <- usgs_clean_flow(tuol_flw, 'TUOLUMNE')
ggplot(tuol_flow, aes(date, mean_flow_cfs)) + geom_line()
use_data(tuol_flow)

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

tuol_temp <- tuol_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  mutate(md = ifelse(is.nan(md), (mx + mn)/2, md), screw_trap = 'TUOLUMNE') %>%
  select(date, mean_temp_C = md, screw_trap)

use_data(tuol_temp)

# American	2013	2016-----
# USGS 11446500 AMERICAN R A FAIR OAKS CA
# Temperature, water, degrees Celsius 	 1961-10-31 	 2018-04-17
# Discharge, cubic feet per second 	 1904-10-01 	 2018-04-16
amer_flw <- dataRetrieval::readNWISdv(siteNumbers = '11446500', parameterCd = '00060',
                                      startDate = '2013-01-01', endDate = '2016-12-31')
amer_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11446500', parameterCd = '00010',
                                      startDate = '2013-01-01', endDate = '2016-12-31', statCd = c('00001', '00002', '00008'))

amer_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

amer_flow <- usgs_clean_flow(amer_flw, 'AMERICAN')
ggplot(amer_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(amer_flow)

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
use_data(amer_temp)

# Battle	1998	2016------
# USGS 11376550 BATTLE C BL COLEMAN FISH HATCHERY NR COTTONWOOD CA
# Discharge, cubic feet per second 	 1961-10-01 	 2018-04-16
battle_flw <- dataRetrieval::readNWISdv(siteNumbers = '11376550', parameterCd = '00060',
                                        startDate = '1998-01-01', endDate = '2016-12-31')

battle_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

battle_flow <- usgs_clean_flow(battle_flw, 'BATTLE')
ggplot(battle_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(battle_flow)

# Clear	1998	2016------
# USGS 11372000 CLEAR C NR IGO CA
# Temperature, water, degrees Celsius 	 1965-03-26 	 1979-01-28
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16
# CDEC CLEAR CREEK NEAR IGO - IGO
# flow 09/06/1996 to present
# temp 09/06/1996 to present
clear_flw <- CDECRetrieve::cdec_query(station = 'IGO', sensor_num = '20', dur_code = 'H',
                                        start_date = '1998-01-01', end_date = '2016-12-31')

clear_tmp <- CDECRetrieve::cdec_query(station = 'IGO', sensor_num = '25', dur_code = 'H',
                                        start_date = '1998-01-01', end_date = '2016-12-31')

clear_flw %>%
  filter(between(parameter_value, 0, 10000)) %>%
  ggplot(aes(datetime, parameter_value)) +
  geom_line()

clear_flow <- cdec_clean_flow(filter(clear_flw, between(parameter_value, 0, 10000), year(datetime) > 1997), 'CLEAR')
use_data(clear_flow, overwrite = TRUE)

clear_tmp %>%
  filter(between(parameter_value, 40, 80), datetime >= as.Date('1998-06-01')) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(tt = mean(parameter_value, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = tt)) +
  geom_col()

clear_temp <- cdec_clean_temp(filter(clear_tmp, between(parameter_value, 40, 80), datetime >= as.Date('1998-06-01')), 'CLEAR')
ggplot(clear_temp, aes(date, mean_temp_C)) + geom_line()
use_data(clear_temp)

# Mok	1999	2015-----
# USGS 11323500 MOKELUMNE R BL CAMANCHE DAM CA
# Discharge, cubic feet per second 	 1904-10-01 	 2017-09-30
# water temp EBMUD Victor, CA
moke_flw <- dataRetrieval::readNWISdv(siteNumbers = '11323500', parameterCd = '00060',
                                      startDate = '1999-01-01', endDate = '2015-12-31')

moke_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()

moke_flow <- usgs_clean_flow(moke_flw, 'MOKELUMNE')
ggplot(moke_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(moke_flow)

victor <- read_csv('data-raw/Victor15min.csv')

moke_temp <- victor %>%
  mutate(date = as.Date(Time, '%H:%M:%S %m/%d/%Y')) %>%
  group_by(year = year(date), month = month(date), day = days_in_month(date)) %>%
  summarise(mean_temp_C = mean(WaterTemperatureCelsius, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, day, sep = '-')), screw_trap = 'MOKELUMNE') %>%
  filter(year > 1998) %>%
  select(date, mean_temp_C, screw_trap)

ggplot(moke_temp, aes(date, mean_temp_C)) + geom_line()
use_data(moke_temp, overwrite = TRUE)

# Stan	1998	2016-----
# USGS 11303000 STANISLAUS R A RIPON CA
# Temperature, water, degrees Celsius 	 1985-07-01 	 2018-04-17
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16
stan_flw <- dataRetrieval::readNWISdv(siteNumbers = '11303000', parameterCd = '00060',
                                      startDate = '1998-01-01', endDate = '2016-12-31')
stan_tmp <- dataRetrieval::readNWISdv(siteNumbers = '11303000', parameterCd = '00010',
                                      startDate = '1998-01-01', endDate = '2016-12-31', statCd = c('00001', '00002', '00008'))
stan_flow %>% tail
stan_flw %>%
  ggplot(aes(Date, X_00060_00003)) +
  geom_line()
stan_flow <- usgs_clean_flow(stan_flw, 'STANISLAUS')
ggplot(stan_flow, aes(date, mean_flow_cfs)) + geom_col()
use_data(stan_flow, overwrite = TRUE)

stan_temp <- stan_tmp %>%
  select(Date, max_temp = X_00010_00001, min_temp = X_00010_00002, med_temp = X_00010_00008) %>%
  group_by(year = year(Date), month = month(Date), day = days_in_month(Date)) %>%
  summarise(mx = mean(max_temp, na.rm = TRUE), mn = mean(min_temp, na.rm = TRUE), md = mean(med_temp, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  ungroup() %>%
  select(-year, -month, -day) %>%
  mutate(md = ifelse(is.nan(md), (mx + mn)/2, md), screw_trap = 'STANISLAUS') %>%
  select(date, mean_temp_C = md, screw_trap)

ggplot(stan_temp, aes(date, mean_temp_C)) + geom_col()
use_data(stan_temp)

# Chipps Trawl 	2008	2011 ***********
# (corrected assignments)
# Proposed "Good" fish monitoring data	2008	2018 OR 	2003	2018 ******