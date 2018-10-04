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
#' \tabular{ll}{
#'   \strong{Location} \tab \strong{start-end}\cr
#'   Feather River\tab	1998 - 2016\cr
#'   American River\tab	2013 - 2016\cr
#'   Clear Creek\tab	1998 - 2016\cr
#'   Mokelumne River\tab	1999 - 2015\cr
#'   Stanislaus River \tab	1998 - 2016
#' }
#'
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
#' \tabular{ll}{
#'   \strong{Location} \tab \strong{start-end}\cr
#'   Feather River\tab	1998 - 2016\cr
#'   American River\tab	2013 - 2016\cr
#'   Clear Creek\tab	1998 - 2016\cr
#'   Mokelumne River\tab	1999 - 2015\cr
#'   Stanislaus River \tab	1998 - 2016
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
#' \strong{NOTE:} No major diversions to track for Clear Creek.
#'
#' Mokelumne data is measured provided by EDMUD
#'
#' @section Total Diverted:
#' Values were estimated by multiplying the measured flow data and the modeled
#' proportion diverted values.
#'
#' \strong{NOTE:} Mokelumne data is measured provided by EDMUD
#'
#' @name diversions
#' @aliases NULL
NULL

#' @rdname diversions
"feat_prop_div"

#' @rdname diversions
"amer_prop_div"

#' @rdname diversions
"moke_prop_div"

#' @rdname diversions
"stan_prop_div"

#' @rdname diversions
"feat_tot_div"

#' @rdname diversions
"amer_tot_div"

#' @rdname diversions
"moke_tot_div"

#' @rdname diversions
"stan_tot_div"


#' Gaged Flow and Temperature
#'
#' Measured daily and monthly mean flow (cfs) and temperature (°C) from USGS and CDEC gages
#' for the duration of screw trap observations.
#'
#' @details
#' Available screw trap data start and end years:
#' \tabular{ll}{
#'   \strong{Location} \tab \strong{start-end}\cr
#'   Feather River\tab	1998 - 2016\cr
#'   American River\tab	2013 - 2016\cr
#'   Clear Creek\tab	1998 - 2016\cr
#'   Mokelumne River\tab	1999 - 2015\cr
#'   Stanislaus River \tab	1998 - 2016
#' }
#'
#' @section Flow and Temperature Gage IDs:
#'
#' \strong{Flow Gages:}
#' \itemize{
#'   \item Feather River - CDEC FEATHER RIVER NEAR GRIDLEY (GRL)
#'   \item American River - USGS AMERICAN R A FAIR OAKS CA (11446500)
#'   \item Clear Creek - CDEC CLEAR CREEK NEAR IGO (IGO)
#'   \item Mokelumne River - USGS MOKELUMNE R BL CAMANCHE DAM CA (11323500)
#'   \item Stanislaus River - USGS STANISLAUS R A RIPON CA (11303000)
#' }
#'
#' \strong{Temperature Gages:}
#' \itemize{
#'   \item Feather River - CDEC FEATHER RIVER NEAR GRIDLEY (GRL)
#'   \item American River - USGS AMERICAN R A FAIR OAKS CA (11446500)
#'   \item Clear Creek  - CDEC CLEAR CREEK NEAR IGO (IGO)
#'   \item Mokelumne River - EDMUD provided data near Victor CA
#'   \item Stanislaus River - USGS STANISLAUS R A RIPON CA (11303000)
#' }
#'
#' \strong{NOTE:} Feather temperature data available 2003 - 2016
#'
#' @section Daily Data:
#' Daily data for flow and temperature are also available: \code{[watershed]_daily_flow}
#' or  \code{[watershed]_daily_temp}
#'
#' @name flow_temperature
#' @aliases NULL
NULL

#' @rdname flow_temperature
"feather_flow"

#' @rdname flow_temperature
"tuol_flow"

#' @rdname flow_temperature
"amer_flow"

#' @rdname flow_temperature
"moke_flow"

#' @rdname flow_temperature
"stan_flow"

#' @rdname flow_temperature
"clear_flow"

#' @rdname flow_temperature
"feather_temp"

#' @rdname flow_temperature
"amer_temp"

#' @rdname flow_temperature
"moke_temp"

#' @rdname flow_temperature
"stan_temp"

#' @rdname flow_temperature
"clear_temp"

#' @rdname flow_temperature
"feather_daily_flow"

#' @rdname flow_temperature
"amer_daily_flow"

#' @rdname flow_temperature
"moke_daily_flow"

#' @rdname flow_temperature
"stan_daily_flow"

#' @rdname flow_temperature
"clear_daily_flow"

#' @rdname flow_temperature
"feather_daily_temp"

#' @rdname flow_temperature
"amer_daily_temp"

#' @rdname flow_temperature
"moke_daily_temp"

#' @rdname flow_temperature
"stan_daily_temp"

#' @rdname flow_temperature
"clear_daily_temp"

