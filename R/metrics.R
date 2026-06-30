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
#' @return numeric vector (non-finite ratios returned as `NA`).
#' @export
income_transfer_score <- function(treated_mean, baseline_mean) {
  out <- treated_mean / baseline_mean
  out[!is.finite(out)] <- NA_real_
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
#' @return numeric vector (non-finite ratios returned as `NA`).
#' @export
variability_reduction_score <- function(treated_var, baseline_var) {
  out <- treated_var / baseline_var
  out[!is.finite(out)] <- NA_real_
  out
}

#' Risk Reduction Efficiency Ratio (RRER) -- Equation (3)
#'
#' \deqn{RRER = \mathbb{1}(VRS < 1)\,\mathbb{1}(ITS > 1)\,\frac{1 - VRS}{ITS - 1}}
#'
#' The proportional variability reduction deflated by the proportional income
#' (outcome) gain required to achieve it. It is `0` unless the configuration both
#' lifts the mean (`ITS > 1`) and cuts variability (`VRS < 1`); otherwise it
#' ranges over `[0, Inf)`, with larger values = more efficient risk mitigation.
#' Exactly Equation (3) of Tsiboe et al. (2025).
#'
#' @param its Income Transfer Score (from `income_transfer_score()`).
#' @param vrs Variability Reduction Score (from `variability_reduction_score()`).
#' @return numeric vector in `[0, Inf)`.
#' @examples
#' risk_reduction_efficiency(its = 1.05, vrs = 0.80)   # both conditions met
#' risk_reduction_efficiency(its = 0.98, vrs = 0.80)   # mean not lifted -> 0
#' @export
risk_reduction_efficiency <- function(its, vrs) {
  ind <- (vrs < 1) & (its > 1) & is.finite(vrs) & is.finite(its) & (its != 1)
  out <- ifelse(ind, (1 - vrs) / (its - 1), 0)
  out[!is.finite(out)] <- 0
  out
}
