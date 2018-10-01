#' A simulated data set
#'
#' A simulated cohort with 9 confounders (X1 to X9), generated with the algorithm described in Hajage *et al.* (2018) \doi{10.1002/bimj.201700330}.
#'
#' @docType data
#'
#' @usage data(hrData)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#' @references
#’   Hajage David, Chauvet Guillaume, Belin Lisa, Lafourcade Alexandre, Tubach Florence, De Rycke Yann.
#'   Closed-form variance estimator for weighted propensity score estimators with survival outcome.
#'   Submitted to Statistics in Medicine (2017).
#'
#' @examples
#' data(hrData, package = "hrIPW")
#' hrIPW(hrData, time = "time", status = "status", exposure = "Trt",
#'       variables = paste("X", 1:9, sep = ""), wtype = "ATE-stab")
"hrData"
