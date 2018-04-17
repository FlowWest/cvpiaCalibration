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

# SCREW TRAP ***********
#
# RBDD	1999	2016
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

feather_temp %>%
  ggplot(aes(x = date, y = mean_temp_C, fill = as.factor(month(date)))) +
  geom_col()

use_data(feather_temp)

# Tuolumne	2007	2017-----
# USGS 11290000 TUOLUMNE R A MODESTO CA
# Temperature, water, degrees Celsius 	 1965-07-21 	 2018-04-17
# Discharge, cubic feet per second 	 1895-01-01 	 2018-04-16

# American	2013	2016-----
# USGS 11446500 AMERICAN R A FAIR OAKS CA
# Temperature, water, degrees Celsius 	 1961-10-31 	 2018-04-17
# Discharge, cubic feet per second 	 1904-10-01 	 2018-04-16

# Battle	1998	2016------
# USGS 11376550 BATTLE C BL COLEMAN FISH HATCHERY NR COTTONWOOD CA
# Discharge, cubic feet per second 	 1961-10-01 	 2018-04-16

# Clear	1998	2016------
# USGS 11372000 CLEAR C NR IGO CA
# Temperature, water, degrees Celsius 	 1965-03-26 	 1979-01-28
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16
# CDEC CLEAR CREEK NEAR IGO - IGO
# flow 09/06/1996 to present
# temp 09/06/1996 to present

# Mok	1999	2015-----
# USGS 11323500 MOKELUMNE R BL CAMANCHE DAM CA
# Discharge, cubic feet per second 	 1904-10-01 	 2017-09-30
# water temp EBMUD Victor, CA

# Stan	1998	2016-----
# USGS 11303000 STANISLAUS R A RIPON CA
# Temperature, water, degrees Celsius 	 1985-07-01 	 2018-04-17
# Discharge, cubic feet per second 	 1940-10-01 	 2018-04-16


# Chipps Trawl 	2008	2011 ***********
# (corrected assignments)
# Proposed "Good" fish monitoring data	2008	2018 OR 	2003	2018 ******
