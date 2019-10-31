#' @title oil
#' @description oil data
#' @format A data frame with 53 rows and 12 variables:
#' \describe{
#'   \item{\code{X1}}{double Index}
#'   \item{\code{dur}}{double duration of the appraisal lag in months (time span between discovery of an oil field and beginning of development, i.e. approval of annex 😎.}
#'   \item{\code{size}}{double size of recoverable reserves in millions of barrels}
#'   \item{\code{waterd}}{double depth of the sea in metres}
#'   \item{\code{gasres}}{double size of recoverable gas reserves in billions of cubic feet}
#'   \item{\code{operator}}{double equity market value (in 1991 million pounds) of the company operating the oil field}
#'   \item{\code{p}}{double real after–tax oil price measured at time of annex B approval}
#'   \item{\code{vardp}}{double volatility of the real oil price process measured as the squared recursive standard errors of the regression of pt-pt-1 on a constant}
#'   \item{\code{p97}}{double adaptive expectations (with parameter theta=0.97) for the real after–tax oil prices formed at the time of annex B approval}
#'   \item{\code{varp97}}{double volatility of the adaptive expectations (with parameter theta=0.97) for real after tax oil prices measured as the squared recursive standard errors of the regression of pt on pte(theta)}
#'   \item{\code{p98}}{double adaptive expectations (with parameter theta=0.98) for the real after–tax oil prices formed at the time of annex B approval}
#'   \item{\code{varp98}}{double volatility of the adaptive expectations (with parameter theta=0.98) for real after tax oil prices measured as the squared recursive standard errors of the regression of pt on pte(theta)}
#'}
#' @details DETAILS
"oil"
