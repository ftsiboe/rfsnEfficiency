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

#' Canonical column names emitted by compute_efficiency_scores()
#'
#' Returns the score / indicator column names produced by
#' \code{\link{compute_efficiency_scores}}, grouped by how they should be
#' summarised: continuous \code{scores} (indices, efficiency, percent
#' transforms), the baseline/safety-net \code{moments}, and the logical
#' \code{flags}. Used as the default column set by \code{\link{summarise_scores}}
#' and available for callers that want to summarise or difference the same set.
#'
#' @return named list with character vectors \code{scores}, \code{moments}, and
#'   \code{flags}.
#' @export
efficiency_score_columns <- function() {
  list(
    scores = c("mean_index", "sd_index", "var_index", "cv_index",
               "downside_index", "shortfall_index", "downside_index_n", "shortfall_index_n",
               "risk_index", "risk_index_downside", "risk_index_shortfall",
               "efficiency", "efficiency_downside", "efficiency_shortfall",
               "mean_gain_pct", "risk_reduction_pct",
               "risk_reduction_pct_downside", "risk_reduction_pct_shortfall"),
    moments = as.vector(t(outer(
      c("mean", "sd", "var", "cv", "lapv", "lrpv", "nlapv", "nlrpv", "ploss"),
      c("base", "sn"), paste, sep = "_"))),
    flags = c("mean_gain_flag", "risk_reduction_flag",
              "risk_reduction_flag_downside", "risk_reduction_flag_shortfall",
              "reduces_risk")
  )
}

#' Summarise producer-level scores into robust group summaries
#'
#' Collapses the per-producer score table from \code{compute_efficiency_scores()}
#' to one row per \code{by} group. By default it summarises \strong{every} score,
#' indicator, percent transform, and moment the scorer produces: each continuous
#' metric \code{m} yields a robust (trimmed) mean kept as \code{m} plus its median
#' \code{med_m}, and each logical flag \code{f} yields the share (percent) of the
#' group flagged, \code{pct_f}. A few producers with near-zero baseline revenue
#' produce extreme indices, so the means are trimmed (see \code{\link{trim_mean}})
#' and the medians reported alongside as a robustness check. Every summary accepts
#' an optional \code{weight} column, so results can be expressed per acre, per
#' dollar of liability, or per simulation weight rather than per producer.
#'
#' Which columns are summarised is discovered from
#' \code{\link{efficiency_score_columns}} intersected with the columns actually
#' present, so non-score columns (identifiers, group codes, the weight) are
#' ignored automatically. Pass \code{metrics}/\code{flags} to override.
#'
#' @param dt data.table of per-producer scores (from
#'   \code{compute_efficiency_scores()}).
#' @param by character vector of grouping columns, or \code{NULL} for a single
#'   overall summary.
#' @param weight optional name of a numeric weight column in \code{dt}.
#'   \code{NULL} (default) weights every producer equally; when supplied, the
#'   trimmed means, medians, and flag shares are all weighted by it.
#' @param lo,hi trimming quantile cut points forwarded to \code{\link{trim_mean}}.
#' @param metrics optional character vector of continuous columns to summarise.
#'   \code{NULL} (default) uses the scores (and, if \code{include_moments},
#'   moments) from \code{\link{efficiency_score_columns}} that are present in
#'   \code{dt}.
#' @param flags optional character vector of logical/0-1 columns to report as
#'   \code{pct_*} shares. \code{NULL} (default) uses the flags from
#'   \code{\link{efficiency_score_columns}} present in \code{dt}.
#' @param medians whether to also report a \code{med_*} median for every metric
#'   (default \code{TRUE}).
#' @param include_moments whether to include the baseline/safety-net
#'   \code{*_base}/\code{*_sn} moments among the default metrics (default
#'   \code{TRUE}).
#' @return data.table, one row per \code{by} group, with \code{n_producers}, a
#'   trimmed mean for every metric, a \code{med_*} median for every metric (when
#'   \code{medians}), and a \code{pct_*} share for every flag.
#' @seealso \code{\link{compute_efficiency_scores}}, \code{\link{efficiency_score_columns}},
#'   \code{\link{trim_mean}}
#' @export
summarise_scores <- function(dt, by, weight = NULL, lo = 0.005, hi = 0.995,
                             metrics = NULL, flags = NULL, medians = TRUE,
                             include_moments = TRUE) {
  data.table::setDT(dt)
  if (!is.null(weight)) stopifnot(weight %in% names(dt))

  cols <- efficiency_score_columns()
  if (is.null(metrics)) {
    metrics <- intersect(c(cols$scores, if (include_moments) cols$moments), names(dt))
  }
  if (is.null(flags)) flags <- intersect(cols$flags, names(dt))

  dt[, {
    w  <- if (is.null(weight)) NULL else get(weight)
    Mv <- .SD[, metrics, with = FALSE]
    Fv <- .SD[, flags,   with = FALSE]
    means <- lapply(Mv, trim_mean, lo, hi, w)
    meds  <- if (medians && length(metrics))
      stats::setNames(lapply(Mv, robust_median, w), paste0("med_", metrics))
    pcts  <- if (length(flags))
      stats::setNames(lapply(Fv, function(col) 100 * flag_rate(col, w)),
                      paste0("pct_", flags))
    c(list(n_producers = .N), means, meds, pcts)
  }, by = by, .SDcols = c(metrics, flags)]
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
