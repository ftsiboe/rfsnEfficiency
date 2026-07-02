#' Toy outcomes dataset for examples and CI
#'
#' A small, committed dataset so examples and the test suite run with no network
#' access. Each insurance pool's calibrated yields *across years* serve as its
#' per-agent outcome draws; the indemnity and premium are illustrative (two
#' regimes, `baseline` and `reform`) and carry no policy meaning.
#'
#' @format A `data.table` with one row per pool x regime x year and columns:
#' \describe{
#'   \item{group_id}{insurance-pool identifier (the agent).}
#'   \item{regime}{`"baseline"` or `"reform"`.}
#'   \item{year}{commodity year (the draw index).}
#'   \item{outcome}{calibrated yield (the end-of-season outcome).}
#'   \item{indemnity}{illustrative shortfall payment below the guarantee.}
#'   \item{premium}{illustrative producer-paid premium.}
#' }
#' @source Built by `data-raw/scripts/build_toy_dataset.R` from the
#'   \href{https://github.com/ftsiboe/USFarmSafetyNetLab/releases/tag/calibrated_yield}{USFarmSafetyNetLab `calibrated_yield` release}.
"toy_outcomes"

#' Score and indicator dictionary
#'
#' A committed look-up table explaining every column produced by
#' `compute_efficiency_scores()` -- the moments, relative indices, headline
#' scores, efficiency ratios, flags, and percent transforms -- plus the naming
#' conventions used by `summarise_scores()` (`n_producers`, `med_*`, `pct_*`) and
#' the report-pipeline `d_*` deltas. Use it to look up what a variable means, how
#' it is calculated, and how to read it. Metrics follow Tsiboe et al. (2025).
#'
#' @format A `data.frame` with one row per variable (or naming convention) and
#'   columns:
#' \describe{
#'   \item{variable}{the column name (or `<...>` naming pattern) it documents.}
#'   \item{group}{family: `moment`, `index`, `score`, `flag`, `percent`,
#'     `summary`, or `delta`.}
#'   \item{description}{what the quantity is.}
#'   \item{calculation}{how it is computed (in terms of the moments/indices).}
#'   \item{interpretation}{how to read its value.}
#' }
#' @source Built internally
"score_dictionary"
