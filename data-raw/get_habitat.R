library(tidyverse)
library(devtools)
library(lubridate)

rst_prop_length <- read_csv('data-raw/rst_prop_length.csv')

get_habitats <- function(df, species, watershed, rst_prop_length) {
  flows <- df$mean_flow_cfs
  dates <- df$date

  # scaling habitat area to above the screw trap
  spawn_scale <- rst_prop_length %>%
    filter(river == watershed, habitat == 'spawning') %>%
    pull(prop_length)

  rear_scale <- rst_prop_length %>%
    filter(river == watershed, habitat == 'rearing') %>%
    pull(prop_length)

  spawn <- cvpiaHabitat::set_spawning_habitat(watershed,
                                              species = species,
                                              flow = flows) * spawn_scale

  fry <- cvpiaHabitat::set_instream_habitat(watershed,
                                            species = species,
                                            life_stage = 'fry',
                                            flow = flows) * rear_scale

  juv <- cvpiaHabitat::set_instream_habitat(watershed,
                                            species = species,
                                            life_stage = 'juv',
                                            flow = flows) * rear_scale

  fp <- cvpiaHabitat::acres_to_square_meters(
    cvpiaHabitat::set_floodplain_habitat(watershed, species, flows)) * rear_scale

  habitat <- data.frame(date = dates,
                        spawn = spawn,
                        fry = fry,
                        juv = juv,
                        fp = fp,
                        screw_trap = watershed, stringsAsFactors = FALSE)

  return(habitat)
}


get_all_habitats <- function(species) {
  # RBDD	1999	2016
  # rbdd_habitat <- get_rbdd_habitat(species)
  # Feather	1998	2016
  feather_habitat <- get_habitats(cvpiaCalibration::feather_flow, species, 'Feather River', rst_prop_length)
  # Tuolumne	2007	2017
  # tuolumne_habitat <- get_habitats(cvpiaCalibration::tuol_flow, species, 'Tuolumne River')
  # American	2013	2016
  american_habitat <- get_habitats(cvpiaCalibration::amer_flow, species, 'American River', rst_prop_length)
  # Battle	1998	2016
  # battle_habitat <- get_habitats(cvpiaCalibration::battle_flow, species, 'Battle Creek')
  # Clear	1998	2016
  clear_habitat <- get_habitats(cvpiaCalibration::clear_flow, species, 'Clear Creek', rst_prop_length)
  # Mok	1999	2015
  mokelumne_habitat <- get_habitats(cvpiaCalibration::moke_flow, species, 'Mokelumne River', rst_prop_length)
  # Stan	1998	2016
  stanislaus_habitat <- get_habitats(cvpiaCalibration::stan_flow, species, 'Stanislaus River', rst_prop_length)
  # Deer 1992 2010
  # deer_habitat <- get_habitats(cvpiaCalibration::deer_flow, species, 'Deer Creek')

  all_habitats <- bind_rows(clear_habitat, american_habitat, feather_habitat,
                            mokelumne_habitat, stanislaus_habitat)

  return(all_habitats)
}

fall_habitats <- get_all_habitats('fr')

use_data(fall_habitats, overwrite = TRUE)

# confirm that dates align and flow record is complete
fall_habitats %>%
  group_by(screw_trap) %>%
  summarise(start = min(date), end = max(date), n(), months = (year(end) - year(start) + 1) * 12)

spring_habitats <- get_all_habitats('sr')
use_data(spring_habitats, overwrite = TRUE)

steelhead_habitats <- get_all_habitats('st')
use_data(steelhead_habitats, overwrite = TRUE)

# No longer needed-------------------
get_rbdd_habitat <- function(species) {
  mnths <- month(cvpiaCalibration::rbdd_flow$date)
  flws <- cvpiaCalibration::rbdd_flow$mean_flow_cfs
  rbdd_spawn <- purrr::map2_dbl(mnths, flws, function(month, flow) {
    cvpiaHabitat::set_spawning_habitat('Upper Sacramento River',
                                       species = species,
                                       flow = flow, month = month)
  })

  rbdd_instream_fry <- purrr::map_dbl(flws, function(flow) {
    cvpiaHabitat::set_instream_habitat('Upper Sacramento River',
                                       species = species,
                                       life_stage = 'fry',
                                       flow = flow)
  })

  rbdd_instream_juv <- purrr::map_dbl(flws, function(flow) {
    cvpiaHabitat::set_instream_habitat('Upper Sacramento River',
                                       species = species,
                                       life_stage = 'juv',
                                       flow = flow)
  })

  rbdd_floodplain <- cvpiaHabitat::acres_to_square_meters(
    cvpiaHabitat::set_floodplain_habitat('Upper Sacramento River', species, flws))

  rbdd_habitat <- data.frame(date = cvpiaCalibration::rbdd_flow$date,
                             spawn = rbdd_spawn,
                             fry = rbdd_instream_fry,
                             juv = rbdd_instream_juv,
                             fp = rbdd_floodplain,
                             screw_trap = 'RBDD', stringsAsFactors = FALSE)
  return(rbdd_habitat)
}
