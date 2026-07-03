# Generic risk-reduction metric functions, following Tsiboe et al. (2025),
# "Risk reduction impacts of crop insurance in the United States". These are the
# canonical, composable building blocks; the scoring engine calls them. They are
# vectorized and outcome-agnostic (the inputs can be revenue, profit, or any
# custom outcome's moments).

#' Income Transfer Score (ITS)
#'
#' Ratio of the expected outcome with the safety net to the expected outcome
#' without it. `ITS > 1` indicates the configuration raises the mean outcome.
#' (Eq. 2 in Tsiboe et al. 2025.)
#'
#' @param treated_mean,baseline_mean numeric vectors of mean outcomes with and
#'   without the safety net.
#' @param clamp optional length-2 numeric `c(lower, upper)` to winsorize the
#'   ratio into (guards against a near-zero baseline mean blowing the ratio up).
#'   `NULL` leaves the raw ratio. Defaults to `c(0.10, 2)`.
#' @return numeric vector (non-finite ratios returned as `NA`; finite values
#'   clamped to `clamp` when supplied).
#' @export
income_transfer_score <- function(treated_mean, baseline_mean, clamp = c(0.10, 2)) {
  out <- treated_mean / baseline_mean
  out[!is.finite(out)] <- NA_real_
  if (!is.null(clamp)) {
    stopifnot(length(clamp) == 2L, clamp[1] <= clamp[2])
    out <- pmin(pmax(out, clamp[1]), clamp[2])
  }
  out
}

#' Variability Reduction Score (VRS)
#'
#' Ratio of an outcome-variability measure with the safety net to the measure
#' without it. `VRS < 1` indicates the configuration reduces variability. Works
#' for ANY variability input (CV, downside semivariance, loss-conditional
#' severity, ...), giving the headline and the robustness flavors.
#'
#' @param treated_var,baseline_var numeric vectors of the variability measure
#'   with and without the safety net.
#' @param clamp optional length-2 numeric `c(lower, upper)` to winsorize the
#'   ratio into (guards against a near-zero baseline variability blowing the
#'   ratio up). `NULL` leaves the raw ratio. Defaults to `c(0.10, 2)`.
#' @return numeric vector (non-finite ratios returned as `NA`; finite values
#'   clamped to `clamp` when supplied).
#' @export
variability_reduction_score <- function(treated_var, baseline_var, clamp = c(0.10, 2)) {
  out <- treated_var / baseline_var
  out[!is.finite(out)] <- NA_real_
  if (!is.null(clamp)) {
    stopifnot(length(clamp) == 2L, clamp[1] <= clamp[2])
    out <- pmin(pmax(out, clamp[1]), clamp[2])
  }
  out
}

#' Risk Reduction Efficiency Ratio (RRER) -- Equation (3)
#'
#' \deqn{RRER = \mathbb{1}(VRS < 1)\,\mathbb{1}(ITS > 1)\,\frac{1 - VRS}{ITS - 1}}
#'
#' The proportional variability reduction deflated by the proportional income
#' (outcome) gain required to achieve it. It is `0` unless the configuration both
#' lifts the mean and cuts variability (`VRS < 1`); otherwise it ranges over
#' `[0, Inf)`, with larger values = more efficient risk mitigation. Exactly
#' Equation (3) of Tsiboe et al. (2025).
#'
#' Because the denominator `ITS - 1` vanishes as `ITS -> 1`, a vanishingly small
#' mean gain can make the raw ratio explode. `deadband` guards this by requiring a
#' minimum proportional gain (`ITS - 1 >= deadband`) before the ratio is taken;
#' below it the score is `0`. This also bounds the result: since `VRS >= 0`, the
#' maximum is `1 / deadband` (and `(1 - vrs_floor) / deadband` when VRS is
#' floored). An optional `cap` applies a further hard ceiling.
#'
#' @param its Income Transfer Score (from `income_transfer_score()`).
#' @param vrs Variability Reduction Score (from `variability_reduction_score()`).
#' @param deadband minimum proportional mean gain `ITS - 1` required for a nonzero
#'   score; below it the efficiency is `0`. Guards the `ITS -> 1` singularity and
#'   bounds the score at `1 / deadband`. Default `0.05`; use `0` for the raw
#'   Equation (3) behaviour.
#' @param cap optional hard upper bound applied after the ratio (`NULL` = none).
#' @return numeric vector in `[0, 1/deadband]` (or `[0, cap]`).
#' @examples
#' risk_reduction_efficiency(its = 1.05, vrs = 0.80)   # both conditions met
#' risk_reduction_efficiency(its = 0.98, vrs = 0.80)   # mean not lifted -> 0
#' risk_reduction_efficiency(its = 1.001, vrs = 0.50)  # gain below deadband -> 0
#' @export
risk_reduction_efficiency <- function(its, vrs, deadband = 0.05, cap = NULL) {
  ind <- (vrs < 1) & is.finite(vrs) & is.finite(its) & ((its - 1) >= deadband)
  out <- ifelse(ind, (1 - vrs) / (its - 1), 0)
  out[!is.finite(out)] <- 0
  if (!is.null(cap)) out <- pmin(out, cap)
  out
}
