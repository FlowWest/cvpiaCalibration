library(readxl)
library(tidyverse)
library(cvpiaData)
library(testthat)
library(stringr)
library(devtools)

synth_wy <- read_excel('data-raw/water_year_synthetic_series.xlsx') %>%
  select(wy = sim.year, stand_in_wy = syn.yr.WY) %>%
  arrange(wy) %>%
  filter(!is.na(wy))%>%
  left_join(tibble(model_year_index = 1:20, stand_in_wy = 1980:1999))

View(synth_wy)
glimpse(synth_wy)

synth_n_12_20 <- function(n, input_array, model_wy = synth_wy$model_year_index) {
  output <- array(NA, dim = c(n, 12, 20))
  synth_year = 1
  for (year in model_wy) {
    output[ , , synth_year] = input_array[, , year]
    synth_year = synth_year + 1
  }
  return(output)
}
synth_12_20_n <- function(n, input_array, model_wy = synth_wy$model_year_index) {
  output = array(NA, dim = c(12, 20, n))
  synth_year = 1
  for (year in model_wy) {
    output[ , synth_year, ] = input_array[, year, ]
    synth_year = synth_year + 1
  }
  return(output)
}
# for gate top, 1979-1999, need to use 1997 as 1979 for new 1998-2017 period
synth_12_21_n <- function(n, input_array, model_wy = c(18, synth_wy$model_year_index)) {
  output = array(NA, dim = c(12, 21, n))
  synth_year = 1
  for (year in model_wy) {
    output[ , synth_year, ] = input_array[, year, ]
    synth_year = synth_year + 1
  }
  return(output)
}

set_synth_years <- function(species) {
  run <- cvpiaData::load_baseline_data(species)

  p.tempMC20 <- run$p.tempMC20
  p.diver <- synth_n_12_20(31, run$p.diver)
  t.diver <- synth_n_12_20(31, run$t.diver)
  dlt.divers <- synth_12_20_n(2, run$dlt.divers)
  dlt.divers.tot <- synth_12_20_n(2, run$dlt.divers.tot)
  juv.tmp <- synth_n_12_20(31, run$juv.tmp)
  juv.tmp.dlt <- synth_12_20_n(2, run$juv.tmp.dlt)
  Dlt.inf <- synth_12_20_n(2, run$Dlt.inf)
  DLThab <- synth_12_20_n(2, run$DLThab)
  prop.Q.bypasses <- synth_12_20_n(6, run$prop.Q.bypasses)
  IChab.bypass <- synth_n_12_20(6, run$IChab.bypass)
  floodp.bypass <- synth_n_12_20(6, run$floodp.bypass)
  gate.top <- synth_12_21_n(2, run$gate.top)
  fp.weeks <- synth_n_12_20(31, run$fp.weeks)
  DegDay <- synth_n_12_20(31, run$DegDay)

  retQ <- tibble(
    watershed = run$retQ$watershed,
    `1998` = run$retQ$`1998`,
    `1999` = run$retQ$`1997`,
    `2000` = run$retQ$`1993`,
    `2001` = run$retQ$`1981`,
    `2002` = run$retQ$`1989`,
    `2003` = run$retQ$`1993`,
    `2004` = run$retQ$`1993`,
    `2005` = run$retQ$`1993`,
    `2006` = run$retQ$`1998`,
    `2007` = run$retQ$`1994`,
    `2008` = run$retQ$`1988`,
    `2009` = run$retQ$`1994`,
    `2010` = run$retQ$`1985`,
    `2011` = run$retQ$`1997`,
    `2012` = run$retQ$`1985`,
    `2013` = run$retQ$`1994`,
    `2014` = run$retQ$`1992`,
    `2015` = run$retQ$`1992`,
    `2016` = run$retQ$`1989`,
    `2017` = run$retQ$`1998`
  )

  upSacQ <- tibble(
    `1998` = run$upSacQ$`1998`,
    `1999` = run$upSacQ$`1990`,
    `2000` = run$upSacQ$`1993`,
    `2001` = run$upSacQ$`1981`,
    `2002` = run$upSacQ$`1989`,
    `2003` = run$upSacQ$`1993`,
    `2004` = run$upSacQ$`1993`,
    `2005` = run$upSacQ$`1993`,
    `2006` = run$upSacQ$`1998`,
    `2007` = run$upSacQ$`1994`,
    `2008` = run$upSacQ$`1988`,
    `2009` = run$upSacQ$`1994`,
    `2010` = run$upSacQ$`1985`,
    `2011` = run$upSacQ$`1997`,
    `2012` = run$upSacQ$`1985`,
    `2013` = run$upSacQ$`1994`,
    `2014` = run$upSacQ$`1992`,
    `2015` = run$upSacQ$`1992`,
    `2016` = run$upSacQ$`1989`,
    `2017` = run$upSacQ$`1998`
  )

  freeportQ <- tibble(
    `1998` = run$freeportQ$`1998`,
    `1999` = run$freeportQ$`1990`,
    `2000` = run$freeportQ$`1993`,
    `2001` = run$freeportQ$`1981`,
    `2002` = run$freeportQ$`1989`,
    `2003` = run$freeportQ$`1993`,
    `2004` = run$freeportQ$`1993`,
    `2005` = run$freeportQ$`1993`,
    `2006` = run$freeportQ$`1998`,
    `2007` = run$freeportQ$`1994`,
    `2008` = run$freeportQ$`1988`,
    `2009` = run$freeportQ$`1994`,
    `2010` = run$freeportQ$`1985`,
    `2011` = run$freeportQ$`1997`,
    `2012` = run$freeportQ$`1985`,
    `2013` = run$freeportQ$`1994`,
    `2014` = run$freeportQ$`1992`,
    `2015` = run$freeportQ$`1992`,
    `2016` = run$freeportQ$`1989`,
    `2017` = run$freeportQ$`1998`
  )

  dlt.gates <- run$dlt.gates
  egg.tmp.eff <- run$egg.tmp.eff
  Dlt.inp <- run$Dlt.inp
  prop.pulse <- run$prop.pulse
  medQ <- run$medQ
  inps <- run$inps
  IChab.spawn <- synth_n_12_20(31, run$IChab.spawn)
  IChab.fry <- synth_n_12_20(31, run$IChab.fry)
  IChab.juv <- synth_n_12_20(31, run$IChab.juv)
  floodP <- synth_n_12_20(31, run$floodP)

  all_inputs <- list(p.tempMC20 = p.tempMC20,
                     p.diver = p.diver,
                     t.diver = t.diver,
                     dlt.divers = dlt.divers,
                     dlt.divers.tot = dlt.divers.tot,
                     juv.tmp = juv.tmp,
                     juv.tmp.dlt = juv.tmp.dlt,
                     Dlt.inf = Dlt.inf,
                     DLThab = DLThab,
                     prop.Q.bypasses = prop.Q.bypasses,
                     IChab.bypass = IChab.bypass,
                     floodp.bypass = floodp.bypass,
                     fp.weeks = fp.weeks,
                     gate.top = gate.top,
                     DegDay = DegDay,
                     retQ = retQ,
                     upSacQ = upSacQ,
                     freeportQ = freeportQ,
                     dlt.gates = dlt.gates,
                     egg.tmp.eff = egg.tmp.eff,
                     Dlt.inp = Dlt.inp,
                     prop.pulse = prop.pulse,
                     medQ = medQ,
                     inps = inps,
                     IChab.spawn = IChab.spawn,
                     IChab.fry = IChab.fry,
                     IChab.juv = IChab.juv,
                     floodP = floodP)

  switch(species,
         'spring' = {
           all_inputs$SR.pools <- run$SR.pools
           all_inputs$has.SR <- run$has.SR
         },
         'steelhead' = {
           all_inputs$ST.pools <- run$ST.pools
           all_inputs$IChab.adult <- run$IChab.adult
         })

  return(all_inputs)

}

# create calibration model inputs
fall_inputs <- set_synth_years('fall')
use_data(fall_inputs, overwrite = TRUE)

winter_inputs <- set_synth_years('winter')
use_data(winter_inputs, overwrite = TRUE)

spring_inputs <- set_synth_years('spring')
use_data(spring_inputs, overwrite = TRUE)

steelhead_inputs <- set_synth_years('steelhead')
use_data(steelhead_inputs, overwrite = TRUE)

# tests------------------
# expect p.diver of synthetic year 2003 equal to 1993 (year 14 of simulation)
expect_equal(set_synth_years('fall')$p.diver[, , 6], cvpiaData::load_baseline_data('fall')$p.diver[, , 14])
expect_equal(set_synth_years('spring')$p.diver[, , 6], cvpiaData::load_baseline_data('spring')$p.diver[, , 14])

# expect DLThab of synthetic year 2012 equal to 1985 (year 6 of simulation)
expect_equal(set_synth_years('fall')$DLThab[, 15,], cvpiaData::load_baseline_data('fall')$DLThab[, 6,])
expect_equal(set_synth_years('steelhead')$DLThab[, 15,], cvpiaData::load_baseline_data('steelhead')$DLThab[, 6,])

# expect gate.top of synthetic year 2016 equal to 1989 (year 10 of simulation)
expect_equal(set_synth_years('fall')$gate.top[, 20,], cvpiaData::load_baseline_data('fall')$gate.top[, 10,])
expect_equal(set_synth_years('steelhead')$gate.top[, 20,], cvpiaData::load_baseline_data('steelhead')$gate.top[, 10,])
