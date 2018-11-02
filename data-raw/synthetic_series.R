library(readxl)
library(tidyverse)
library(cvpiaData)
library(testthat)
library(stringr)

synth_wy <- read_excel('data-raw/water_year_synthetic_series.xlsx') %>%
  select(wy = sim.year, stand_in_wy = syn.yr.WY) %>%
  filter(!is.na(wy)) %>%
  left_join(
    data.frame(model_year_index = 1:20, stand_in_wy = 1980:1999)
  )

View(synth_wy)
glimpse(synth_wy)
wy <- synth_wy$wy
names(wy) <- as.character(synth_wy$stand_in_wy )
synth_wy$model_year_index
run <- cvpiaData::load_baseline_data('fall')
dim(run$p.diver)
run$p.diver[, , 1]

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

purrr::map(run, dim)

p.diver <- synth_n_12_20(31, run$p.diver)
t.diver <- synth_n_12_20(31, run$t.diver)
dlt.divers <- synth_12_20_n(2, run$dlt.divers)
dlt.divers.tot <- synth_12_20_n(2, run$dlt.divers.tot)
juv.tmp <- synth_n_12_20(31, run$juv.tmp)
juv.tmp.dlt <- synth_12_20_n(2, run$juv.tmp.dlt)
Dlt.inf <- synth_12_20_n(2, run$Dlt.inf)
DLThab <- synth_12_20_n(2, run$DLThab)
prop.Q.bypass <- synth_12_20_n(6, run$prop.Q.bypasses)
IChab.bypass <- synth_n_12_20(6, run$IChab.bypass)
floodp.bypass <- synth_n_12_20(6, run$floodp.bypass)
Crun$gate.top %>% dim
fp.weeks <- synth_n_12_20(31, run$fp.weeks)
DegDay <- synth_n_12_20(31, run$DegDay)

retQ <- tibble(
  watershed = run$retQ$watershed,
  `1998` = run$retQ$`1998`,
  `1999` = run$retQ$`1993`, # confirm with adam what year should 1999 be
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
# run$dlt.gates
# run$egg.tmp.eff
# run$Dlt.inp
# run$prop.pulse
# run$medQ
# run$inps
IChab.spawn <- synth_n_12_20(31, run$IChab.spawn)
IChab.fry <- synth_n_12_20(31, run$IChab.fry)
IChab.juv <- synth_n_12_20(31, run$IChab.juv)
floodP <- synth_n_12_20(31, run$floodP)

