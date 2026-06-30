#' rfsnEfficiency: risk-reduction efficacy and efficiency of risk-management configurations
#'
#' A general, program-agnostic toolkit. It separates an **efficacy** layer (does
#' a configuration lift the mean outcome and cut its variability?) from an
#' **efficiency** layer (how much risk reduction per unit of outcome gained?),
#' all computed on a configurable outcome (revenue, profit, margin, or a custom /
#' cost-side measure) relative to a no-program baseline, and compared across
#' regimes or scenarios.
#'
#' Core entry points: `build_agent_outcomes()` and `build_outcome()` (construct
#' the outcome from end-of-season draws and program parameters),
#' `income_transfer_score()` / `variability_reduction_score()` /
#' `risk_reduction_efficiency()` (the generic ITS/VRS/RRER functions),
#' `compute_efficiency_scores()`, `regime_deltas()`, and
#' `run_efficiency_analysis()`.
#'
#' @import data.table
#' @importFrom stats sd quantile weighted.mean na.omit as.formula rnorm
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

# NSE global variables are registered in .onLoad() (see zzz.R) from
# PACKAGE_GLOBALVARIABLES.
