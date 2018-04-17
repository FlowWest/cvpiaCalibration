#' Fall Habitats
#' @description
#' The spawning, instream rearing, and floodplain rearing habitat values
#' at screw trap locations for model calibration.
#'
#' @format dataframe with 1500 rows and 6 variables
#'
#' \describe{
#'   \item{date}{last day of month}
#'   \item{spawn}{spawning habitat in square meters}
#'   \item{fry}{fry instream rearing habitat in square meters}
#'   \item{juv}{juvenile instream rearing habitat in square meters}
#'   \item{fp}{floodplain rearing habitat in square meters}
#'   \item{screw_trap}{screw trap location}
#' }
#'
#' @details
#'
#' Available screw trap data start and end years:
#'
#' \tabular{lll}{
#'   \strong{Location} \tab \strong{start} \tab \strong{end}\cr
#'   RBDD \tab 1999  \tab 2016\cr
#'   Feather River\tab	1998 \tab	2016\cr
#'   Tuolumne River\tab	2007 \tab	2017\cr
#'   American River\tab	2013 \tab	2016\cr
#'   Battle Creek\tab	1998 \tab	2016\cr
#'   Clear Creek\tab	1998 \tab	2016\cr
#'   Mokelumne River\tab	1999 \tab	2015\cr
#'   Stanislaus River \tab	1998 \tab	2016
#' }
#'
#' \strong{NOTE:}
#' \enumerate{
#'   \item Feather River does not have habitat values for 1998 because flow data is unavailable.
#'   \item Only floodplain values at RBDD are suitable, for all other regions apply
#'   a suitability factor (suggested value = .27).
#' }
#'
#'
"fall_habitats"

#' Spring Habitats
#' @description
#' The spawning, instream rearing, and floodplain rearing habitat values
#' at screw trap locations for model calibration.
#'
#' @format dataframe with 1500 rows and 6 variables
#'
#' \describe{
#'   \item{date}{last day of month}
#'   \item{spawn}{spawning habitat in square meters}
#'   \item{fry}{fry instream rearing habitat in square meters}
#'   \item{juv}{juvenile instream rearing habitat in square meters}
#'   \item{fp}{floodplain rearing habitat in square meters}
#'   \item{screw_trap}{screw trap location}
#' }
#'
#' @details
#'
#' Available screw trap data start and end years:
#'
#' \tabular{lll}{
#'   \strong{Location} \tab \strong{start} \tab \strong{end}\cr
#'   RBDD \tab 1999  \tab 2016\cr
#'   Feather River\tab	1998 \tab	2016\cr
#'   Tuolumne River\tab	2007 \tab	2017\cr
#'   American River\tab	2013 \tab	2016\cr
#'   Battle Creek\tab	1998 \tab	2016\cr
#'   Clear Creek\tab	1998 \tab	2016\cr
#'   Mokelumne River\tab	1999 \tab	2015\cr
#'   Stanislaus River \tab	1998 \tab	2016
#' }
#'
#' \strong{NOTE:}
#' \enumerate{
#'   \item Feather River does not have habitat values for 1998 because flow data is unavailable.
#'   \item Only floodplain values at RBDD are suitable, for all other regions apply
#'   a suitability factor (suggested value = .27).
#'   \item American River has no spring run.
#' }
#'
#'
"spring_habitats"

#' Steelhead Habitats
#' @description
#' The spawning, instream rearing, and floodplain rearing habitat values
#' at screw trap locations for model calibration.
#'
#' @format dataframe with 1500 rows and 6 variables
#'
#' \describe{
#'   \item{date}{last day of month}
#'   \item{spawn}{spawning habitat in square meters}
#'   \item{fry}{fry instream rearing habitat in square meters}
#'   \item{juv}{juvenile instream rearing habitat in square meters}
#'   \item{fp}{floodplain rearing habitat in square meters}
#'   \item{screw_trap}{screw trap location}
#' }
#'
#' @details
#'
#' Available screw trap data start and end years:
#'
#' \tabular{lll}{
#'   \strong{Location} \tab \strong{start} \tab \strong{end}\cr
#'   RBDD \tab 1999  \tab 2016\cr
#'   Feather River\tab	1998 \tab	2016\cr
#'   Tuolumne River\tab	2007 \tab	2017\cr
#'   American River\tab	2013 \tab	2016\cr
#'   Battle Creek\tab	1998 \tab	2016\cr
#'   Clear Creek\tab	1998 \tab	2016\cr
#'   Mokelumne River\tab	1999 \tab	2015\cr
#'   Stanislaus River \tab	1998 \tab	2016
#' }
#'
#' \strong{NOTE:}
#' \enumerate{
#'   \item Feather River does not have habitat values for 1998 because flow data is unavailable.
#'   \item Only floodplain values at RBDD are suitable, for all other regions apply
#'   a suitability factor (suggested value = .27).
#' }
#'
#'
"steelhead_habitats"
