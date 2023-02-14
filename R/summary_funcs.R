
#' Coefficient of Variation
#'
#' @param x numeric vector
#' @param na.rm logical indicating whether NA values should be excluded from computation
#'
#' @return Coefficient of variation as a percentage of the arithmetic mean of `x`
#' @export
#'
#' @examples # simulate vector of gamma-distributed slaking values
#' stab10_sim <- 1 / (rgamma(3, 2, 1) + 1)
#'
#' cv(stab10_sim)
#'
cv <- function(x, na.rm = T) {
100*sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}

#' Geometric Mean
#'
#' @param x numeric vector
#' @param na.rm logical indicating whether NA values should be excluded from computation
#'
#' @return Geometric mean of `x`
#' @export
#'
#' @examples # simulate vector of gamma-distributed slaking values
#' stab10_sim <- 1 / (rgamma(3, 2, 1) + 1)
#'
#' # geometric mean
#' mean_geom(stab10_sim)
#'
#' # arithmetic mean
#' mean(stab10_sim)
#'
mean_geom <- function(x, na.rm = T) {
exp(mean(log(x), na.rm = na.rm))
}

