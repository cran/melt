#' Calibrate critical value and p-value
#'
#' Calibrate critical value and p-value in [elt()].
#'
#' @param alpha A single numeric.
#' @param statistic A single numeric.
#' @param calibrate A single character.
#' @param p A single integer.
#' @param par A numeric vector.
#' @param object An object that inherit from class \linkS4class{EL}.
#' @param control An object of class of \linkS4class{ControlEL}.
#' @return A numeric vector of length two for the critical value and the
#'   p-value.
#' @noRd
#' @importFrom stats pf qf quantile
calibrate <- function(calibrate, alpha, statistic, p, par, object, control) {
  switch(calibrate,
    "chisq" = {
      c(
        cv = qchisq(p = 1 - alpha, df = p),
        pval = pchisq(q = statistic, df = p, lower.tail = FALSE)
      )
    },
    "boot" = {
      compute_bootstrap_calibration(
        alpha, statistic, control@b, control@seed, control@nthreads,
        getMethodEL(object), getDataMatrix(object), par, control@maxit_l,
        control@tol_l, control@th, getWeights(object)
      )
    },
    "f" = {
      n <- nrow(getDataMatrix(object))
      c(
        cv = qf(p = 1 - alpha, df1 = p, df2 = n - p) * (p * (n - 1)) / (n - p),
        pval = pf(
          q = statistic * (n - p) / (p * (n - 1)), df1 = p,
          df2 = n - p, lower.tail = FALSE
        )
      )
    }
  )
}
