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
