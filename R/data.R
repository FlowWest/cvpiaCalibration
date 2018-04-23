#' Spawning and Rearing Habitats
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
#' @name habitats
#' @aliases NULL
NULL

#' @rdname habitats
"fall_habitats"

#' @rdname habitats
"spring_habitats"

#' @rdname habitats
"steelhead_habitats"

#' Proportion and Total Diverted
#'
#' Monthly propotion and total diverted values for the duration of screw trap observations.
#'
#' @details
#' Available screw trap data start and end years:
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
#' @section Proportion Diverted:
#' Values pre September 2003 are CALSIM model output and values after are the median
#' CALSIM value grouped by water year type and month.
#'
#' For more details on CALSIM see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{proportion_diverted}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/proportion_diverted.html}{link} if in a web browser
#' }
#'
#' NOTE: RBDD is represented using the Upper Sacramento River DSM model node.
#' There are no modeled diversions for Battle and Clear Creek.
#'
#' @section Total Diverted:
#' Values were estimated by multiplying the measured flow data and the modeled
#' proportion diverted values.
#'
#' @name diversions
#' @aliases NULL
NULL

#' @rdname diversions
"rbdd_prop_div"

#' @rdname diversions
"feat_prop_div"

#' @rdname diversions
"tuol_prop_div"

#' @rdname diversions
"amer_prop_div"

#' @rdname diversions
"moke_prop_div"

#' @rdname diversions
"stan_prop_div"

#' @rdname diversions
"rbdd_tot_div"

#' @rdname diversions
"feat_tot_div"

#' @rdname diversions
"tuol_tot_div"

#' @rdname diversions
"amer_tot_div"

#' @rdname diversions
"moke_tot_div"

#' @rdname diversions
"stan_tot_div"
