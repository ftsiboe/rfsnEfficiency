#' Run an on-demand efficiency analysis
#'
#' High-level driver: given simulated outcomes (one row per draw/year x group,
#' optionally tagged by regime/scenario), build the outcome if needed, compute
#' the efficacy/efficiency scores per regime, and difference regimes.
#'
#' @param data data.frame/data.table of simulated outcomes. Must contain the
#'   `by` columns and either (`outcome_baseline`, `outcome_treated`) or the raw
#'   components that `build_outcome()` needs (then pass `outcome_args`).
#' @param by character vector of grouping columns (e.g.,
#'   `c("group_id","scenario")`).
#' @param regime_col regime column name (default "regime"); created as "all" if
#'   absent.
#' @param outcome_args named list of arguments forwarded to `build_outcome()`
#'   when the outcome columns are not already present (e.g.,
#'   `list(base_value = "outcome", transfers = "indemnity", premium = "premium")`).
#' @param ref,alt reference and alternative regime labels for the deltas. If
#'   `NULL` (default) and exactly/at least two regimes are present, the first two
#'   (sorted) are used.
#' @return list with `scores` (per regime) and `deltas` (alt - ref).
#'
#' @examples
#' # Self-contained: synthetic outcomes, no external data required.
#' d <- simulate_example_outcomes()
#' res <- run_efficiency_analysis(
#'   data = d, by = c("group_id", "scenario"),
#'   outcome_args = list(base_value = "outcome", transfers = "indemnity",
#'                       premium = "premium"))
#' head(res$scores)
#' head(res$deltas)
#' @export
run_efficiency_analysis <- function(data, by, regime_col = "regime",
                                    outcome_args = NULL,
                                    ref = NULL, alt = NULL) {
  data.table::setDT(data)

  if (!all(c("outcome_baseline", "outcome_treated") %in% names(data))) {
    data <- do.call(build_outcome, c(list(data = data), outcome_args))
    data.table::setDT(data)
  }
  if (!regime_col %in% names(data)) data[[regime_col]] <- "all"

  regimes <- unique(data[[regime_col]])
  if (is.null(ref) || is.null(alt)) {
    s2 <- sort(regimes)
    if (length(s2) >= 2) { ref <- s2[1]; alt <- s2[2] }
  }

  scores <- data.table::rbindlist(lapply(regimes, function(rg) {
    s <- compute_efficiency_scores(data[data[[regime_col]] == rg], by = by)
    s[[regime_col]] <- rg
    s
  }), fill = TRUE)

  deltas <- if (length(regimes) >= 2 && !is.null(ref) && !is.null(alt) &&
                all(c(ref, alt) %in% regimes)) {
    regime_deltas(scores, by = by, regime_col = regime_col, ref = ref, alt = alt)
  } else NULL

  list(scores = scores, deltas = deltas)
}
