# Risk-reduction efficacy + efficiency scores, computed on a generic outcome
# (outcome_baseline = no safety net; outcome_treated = with safety net), relative
# to the no-safety-net baseline. Names follow the intuitive convention:
#   *_base / *_sn  : moments of the baseline vs safety-net series
#   *_index        : ratio to no-safety-net (1.0 = no effect)
#   mean_index     : efficacy (mean lift)     | risk_index : efficacy (variability cut)
#   efficiency     : risk reduction per unit of outcome gained (RRER)

#' Compute efficacy and efficiency scores from a generic outcome
#'
#' @param data data.frame/data.table with grouping columns and the two outcome
#'   series (`outcome_baseline`, `outcome_treated`), one row per draw/year within
#'   each group.
#' @param by character vector of grouping columns (the unit the scores describe,
#'   e.g., agent identifiers x combination).
#' @param outcome_baseline,outcome_treated column names of the no-safety-net and
#'   with-safety-net outcome series.
#' @return data.table, one row per `by` group, with moments (`*_base`/`*_sn`),
#'   relative indices (`*_index`), headline scores (`mean_index`, `risk_index`,
#'   `efficiency`), flags, percent transforms, and the downside/shortfall flavors.
#' @export
compute_efficiency_scores <- function(data, by,
                                      outcome_baseline = "outcome_baseline",
                                      outcome_treated  = "outcome_treated") {
  data.table::setDT(data)
  stopifnot(all(c(by, outcome_baseline, outcome_treated) %in% names(data)))
  safe_div <- function(n, d) { r <- n / d; r[!is.finite(r) | d == 0] <- NA_real_; r }

  series_stats <- function(val) {
    d <- data[, c(by, val), with = FALSE]
    data.table::setnames(d, val, "x")
    m <- d[, .(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE)), by = by]
    d <- merge(d, m, by = by)
    d[, res := x - mean]
    d[, lres2 := data.table::fifelse(res > 0, NA_real_, res^2)]
    d[, lossI := data.table::fifelse(res < 0, 1, 0)]
    s <- d[, .(lapv = mean(lres2, na.rm = TRUE), ploss = mean(lossI, na.rm = TRUE)), by = by]
    out <- merge(m, s, by = by)
    out[, var   := sd^2]
    out[, cv    := safe_div(sd, mean)]
    out[, lrpv  := safe_div(lapv, ploss)]
    out[, nlapv := safe_div(lapv, mean)]
    out[, nlrpv := safe_div(lrpv, mean)]
    out
  }

  stat_cols <- c("mean", "sd", "var", "cv", "lapv", "lrpv", "nlapv", "nlrpv", "ploss")
  b <- series_stats(outcome_baseline); data.table::setnames(b, stat_cols, paste0(stat_cols, "_base"))
  t <- series_stats(outcome_treated);  data.table::setnames(t, stat_cols, paste0(stat_cols, "_sn"))
  r <- merge(b, t, by = by)

  # Relative indices (treated / baseline; 1 = no safety net).
  # mean_index = ITS (Eq. 2); the variability indices are VRS flavors.
  r[, mean_index        := income_transfer_score(mean_sn, mean_base)]
  r[, sd_index          := safe_div(sd_sn,  sd_base)]
  r[, var_index         := safe_div(var_sn, var_base)]
  r[, cv_index          := variability_reduction_score(cv_sn,    cv_base)]
  r[, downside_index    := variability_reduction_score(lapv_sn,  lapv_base)]
  r[, shortfall_index   := variability_reduction_score(lrpv_sn,  lrpv_base)]
  r[, downside_index_n  := variability_reduction_score(nlapv_sn, nlapv_base)]
  r[, shortfall_index_n := variability_reduction_score(nlrpv_sn, nlrpv_base)]

  # Headline efficacy (variability) flavors
  r[, risk_index           := cv_index]
  r[, risk_index_downside  := downside_index_n]
  r[, risk_index_shortfall := shortfall_index_n]

  # Flags
  r[, mean_gain_flag                := mean_index > 1]
  r[, risk_reduction_flag           := cv_index < 1]
  r[, risk_reduction_flag_downside  := downside_index_n < 1]
  r[, risk_reduction_flag_shortfall := shortfall_index_n < 1]
  r[, reduces_risk                  := mean_gain_flag & risk_reduction_flag]

  # Percent transforms
  r[, mean_gain_pct                := 100 * (mean_index - 1)]
  r[, risk_reduction_pct           := -100 * (cv_index - 1)]
  r[, risk_reduction_pct_downside  := -100 * (downside_index_n - 1)]
  r[, risk_reduction_pct_shortfall := -100 * (shortfall_index_n - 1)]

  # Efficiency (RRER, Eq. 3): risk reduction per unit of outcome gained. The
  # generic risk_reduction_efficiency() applies both indicator conditions, so it
  # is 0 unless the configuration lifts the mean and cuts the relevant variability.
  r[, efficiency           := risk_reduction_efficiency(mean_index, cv_index)]
  r[, efficiency_downside  := risk_reduction_efficiency(mean_index, downside_index_n)]
  r[, efficiency_shortfall := risk_reduction_efficiency(mean_index, shortfall_index_n)]
  r[]
}

#' Crosswalk: intuitive names <-> legacy / source-pipeline column names
#'
#' Maps the package's intuitive score names to the terse column names used by
#' earlier implementations of the same metrics (e.g., `its`, `Relcv`, `sner1`).
#' @return data.frame(`score`, `legacy`, `meaning`).
#' @export
efficiency_score_crosswalk <- function() {
  data.frame(
    score = c("mean_index", "risk_index", "risk_index_shortfall", "risk_index_downside",
              "efficiency", "efficiency_shortfall", "efficiency_downside",
              "mean_gain_flag", "risk_reduction_flag", "reduces_risk",
              "mean_gain_pct", "risk_reduction_pct",
              "cv_index", "sd_index", "var_index",
              "downside_index", "shortfall_index", "downside_index_n", "shortfall_index_n",
              "premium_rate", "producer_premium_rate", "subsidy_share", "loss_cost_ratio"),
    legacy = c("its (Relmean)", "rrs1 (Relcv)", "rrs2 (Relnlrpv)", "rrs3 (Relnlapv)",
               "sner1", "sner2", "sner3",
               "Iits", "Irrs1", "Iits & Irrs1",
               "itp", "rrp1",
               "Relcv", "Relsd", "Relvar",
               "Rellapv", "Rellrpv", "Relnlapv", "Relnlrpv",
               "Simrate", "SimrateP", "Simsuby", "Simlcr"),
    meaning = c("mean outcome index (>1 lifts mean)", "variability index (<1 cuts risk)",
                "loss-conditional variability index", "downside semivariance index",
                "risk reduction per unit outcome gain", "efficiency (shortfall base)",
                "efficiency (downside base)", "mean lifted", "variability cut",
                "genuinely risk-reducing", "% mean gain", "% risk reduction",
                "CV index", "SD index", "variance index",
                "downside semivariance index", "loss-conditional index",
                "normalized downside index", "normalized shortfall index",
                "premium rate", "producer-paid premium rate", "subsidy share",
                "loss-cost ratio"),
    stringsAsFactors = FALSE)
}

#' Regime deltas (alt - ref) for every numeric score
#'
#' @param scores data.table from `compute_efficiency_scores()` stacked over
#'   regimes (must include `regime_col`).
#' @param by grouping columns identifying a comparable unit across regimes.
#' @param metrics numeric score columns to difference (default: all numeric).
#' @param regime_col regime column name (default "regime").
#' @param ref,alt reference and alternative regime labels. If `NULL` (default),
#'   the first two (sorted) regime values are used.
#' @return data.table with `by`, `metric`, the two regime columns, and `delta`.
#' @export
regime_deltas <- function(scores, by, metrics = NULL, regime_col = "regime",
                          ref = NULL, alt = NULL) {
  data.table::setDT(scores)
  if (is.null(metrics)) {
    num <- names(scores)[vapply(scores, is.numeric, logical(1))]
    metrics <- setdiff(num, c(by, regime_col))
  }
  if (is.null(ref) || is.null(alt)) {
    rs <- sort(unique(as.character(scores[[regime_col]])))
    if (length(rs) >= 2) { ref <- rs[1]; alt <- rs[2] }
  }
  long <- data.table::melt(scores, id.vars = c(by, regime_col),
                           measure.vars = metrics, variable.name = "metric",
                           value.name = "value")
  w <- data.table::dcast(
    long, stats::as.formula(paste(paste(c(by, "metric"), collapse = " + "), "~", regime_col)),
    value.var = "value")
  if (!is.null(ref) && !is.null(alt) && all(c(ref, alt) %in% names(w))) {
    w[["delta"]] <- w[[alt]] - w[[ref]]
  }
  w[]
}
