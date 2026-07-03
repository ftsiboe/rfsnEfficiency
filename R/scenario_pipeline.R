# Analysis-agnostic pipeline helpers: unnest Monte-Carlo draw list-columns to
# long, assemble scenario outcome columns from a baseline plus a component
# recipe, and score many outcome columns against a common baseline with
# compute_efficiency_scores().

#' Explode nested draw list-columns to long form
#'
#' Unnests one or more equal-length list-columns (e.g. per-agent Monte-Carlo draw
#' vectors) into a long table with one row per input row x draw element. Every
#' column that is neither a draw column nor dropped is carried (recycled) onto the
#' expanded rows. Analysis-agnostic.
#'
#' @param data data.frame/data.table with the list-columns named in
#'   `draw_columns`; all list-columns must have the same length within a row.
#' @param draw_columns character vector of list-column names to unnest.
#' @param value_names names for the unnested output columns (defaults to
#'   `draw_columns`).
#' @param drop character vector of columns to discard (e.g. draw metadata not
#'   needed downstream).
#' @return a data.table, long over draws.
#' @export
explode_draws <- function(data, draw_columns, value_names = draw_columns,
                          drop = character(0)){
  data <- data.table::as.data.table(data)
  id_cols <- setdiff(names(data), c(draw_columns, drop))
  long <- data[, stats::setNames(lapply(.SD, unlist), value_names),
               by = id_cols, .SDcols = draw_columns]
  long[]
}

#' Assemble scenario outcome columns from a baseline plus a component recipe
#'
#' Builds one outcome column per scenario as `baseline + sum(scale * component)`,
#' driven by a long recipe table. Each recipe row adds one component column
#' (optionally scaled by another column, e.g. an adoption rate) onto the named
#' scenario; scenarios with no component rows (or a single `NA` component) equal
#' the baseline. Analysis-agnostic: the scenario design lives entirely in `recipe`.
#'
#' @param data data.table containing `baseline` and every `component`/`scale`
#'   column referenced by `recipe`.
#' @param baseline name of the baseline outcome column added to every scenario.
#' @param recipe data.table with columns `scenario` (output column name),
#'   `component` (column to add; `NA` = baseline only), and `scale` (optional
#'   column to multiply the component by; `NA` = unscaled).
#' @return `data` (modified in place) with one new column per unique `scenario`.
#' @export
assemble_scenario_outcomes <- function(data, baseline, recipe){
  recipe <- data.table::as.data.table(recipe)
  if(! "scale" %in% names(recipe)) recipe[, scale := NA_character_]
  for(a in unique(recipe$scenario)){
    data[, (a) := get(baseline)]
    comp <- recipe[scenario == a & !is.na(component)]
    for(i in seq_len(nrow(comp))){
      cc <- comp$component[i]; sc <- comp$scale[i]
      if(is.na(sc)) data[, (a) := get(a) + get(cc)]
      else          data[, (a) := get(a) + get(sc) * get(cc)]
    }
  }
  data[]
}

#' Score many outcome columns against a common baseline
#'
#' Loops over `outcome_cols`, scoring each against `baseline_col` with
#' `compute_efficiency_scores()` (draws = the rows within each `by` group), and
#' row-binds the per-scenario scores. Optionally merges a `scenario_map` of labels
#' and attaches a per-group `weight`. Analysis-agnostic wrapper.
#'
#' @param data data.table with `by`, `baseline_col`, and every `outcome_cols`
#'   column; one row per group x draw.
#' @param outcome_cols character vector of treated outcome columns to score.
#' @param by character vector of grouping columns (the scored unit; draws are the
#'   rows within each group).
#' @param baseline_col name of the no-treatment baseline column.
#' @param scenario_map optional data.table keyed by `scenario_col` to merge
#'   scenario labels onto the scores.
#' @param weight optional name of a per-group weight column in `data` to carry
#'   onto the scores.
#' @param scenario_col name of the column identifying the scored outcome column
#'   (default `"scenario_col"`).
#' @return a data.table of scores, one row per `by` group x scenario.
#' @export
score_scenarios <- function(data, outcome_cols, by, baseline_col = "revenue00",
                            scenario_map = NULL, weight = NULL, scenario_col = "scenario_col"){
  scores <- data.table::rbindlist(lapply(outcome_cols, function(a){
    d <- data[, c(by, baseline_col, a), with = FALSE]
    data.table::setnames(d, c(baseline_col, a), c("outcome_baseline","outcome_treated"))
    s <- compute_efficiency_scores(d, by = by)
    s[, (scenario_col) := a]
    s
  }), fill = TRUE)
  if(!is.null(scenario_map)) scores <- merge(scores, scenario_map, by = scenario_col)
  if(!is.null(weight)){
    w <- unique(data[, c(by, weight), with = FALSE])
    scores <- merge(scores, w, by = by, all.x = TRUE)
  }
  scores[]
}

#' Balance a scored panel across scenario cells by pruning incomplete units
#'
#' Guards cross-scenario comparisons against a hidden composition confound: a
#' `unit` (e.g. a producer) should contribute to *every* scenario `cell` (e.g.
#' every policy environment x program combination x participation), otherwise
#' differences across cells can mix treatment with sample composition. This
#' checks whether each unit is present -- and, if `require_finite` is given, has
#' finite values for those metric columns -- in every required cell, and (by
#' default) prunes to the units that are, returning a rebalanced panel.
#'
#' The required grid is the set of observed `cell` combinations minus any listed
#' in `allow_missing` (for structurally absent combinations, e.g. a base arm that
#' has no adoption pathway). Pruning at the finest unit level means every coarser
#' aggregation (subgroup) inherits the balance automatically.
#'
#' @param dt data.table with one row per `unit` x `cell`, plus the metric columns.
#' @param unit character vector of columns identifying a unit (the thing that
#'   must be common across cells; carried whole when kept).
#' @param cell character vector of columns defining a scenario cell.
#' @param require_finite optional character vector of metric columns that must be
#'   finite for a row to count as covering its cell. `NULL` (default) requires
#'   only row presence.
#' @param allow_missing optional named list mapping `cell` columns to value(s);
#'   cells whose columns all match are dropped from the required grid (a unit is
#'   not penalised for missing them).
#' @param prun_to_rebalance if `TRUE` (default) drop units that do not cover the
#'   full required grid and return the rebalanced panel; if `FALSE` return `dt`
#'   unchanged (report-only / dry run).
#' @param min_retention when pruning, `stop()` if the retained share (weighted by
#'   `weight` if supplied, else by unit count) falls below this fraction --
#'   heavy pruning signals a structural problem, not routine trimming.
#' @param weight optional per-unit weight column used for the retention share and
#'   the report.
#' @param verbose if `TRUE` (default) message a one-line retention summary.
#' @return a data.table: the rebalanced panel (or `dt` unchanged when
#'   `prun_to_rebalance = FALSE`), with a `"balance_report"` attribute (per-cell
#'   valid-unit counts before, uniform count after, and overall retention).
#' @export
balance_scenarios <- function(dt, unit, cell, require_finite = NULL,
                              allow_missing = NULL, prun_to_rebalance = TRUE,
                              min_retention = 0.5, weight = NULL, verbose = TRUE){
  dt <- data.table::as.data.table(dt)
  miss_cols <- setdiff(c(unit, cell, require_finite, weight), names(dt))
  if(length(miss_cols)) stop("balance_scenarios: columns not found: ", paste(miss_cols, collapse = ", "))

  # Required grid = observed cell combinations minus the allow_missing combos.
  grid <- unique(dt[, cell, with = FALSE])
  if(!is.null(allow_missing)){
    drop_idx <- rep(TRUE, nrow(grid))
    for(nm in names(allow_missing)) drop_idx <- drop_idx & grid[[nm]] %in% allow_missing[[nm]]
    grid <- grid[!drop_idx]
  }
  n_required <- nrow(grid)

  # A row "covers" its cell if present and (optionally) finite on require_finite.
  # Use a local logical vector so the caller's table is not mutated (it is large).
  valid <- if(is.null(require_finite)) rep(TRUE, nrow(dt))
           else Reduce(`&`, lapply(require_finite, function(cc) is.finite(dt[[cc]])))

  # Per unit: number of required cells covered by a valid row.
  valid_req <- dt[valid][grid, on = cell, nomatch = 0]
  cov  <- unique(valid_req[, c(unit, cell), with = FALSE])[, .(.n_cov = .N), by = unit]
  keep <- cov[.n_cov >= n_required, unit, with = FALSE]

  # Retention (by weight if supplied, else by unit count).
  units_all <- unique(dt[, unit, with = FALSE])
  n_all <- nrow(units_all); n_keep <- nrow(keep)
  ret_count <- if(n_all > 0L) n_keep / n_all else NA_real_
  ret_weight <- NA_real_
  if(!is.null(weight)){
    uw  <- unique(dt[, c(unit, weight), with = FALSE])
    tot <- sum(uw[[weight]], na.rm = TRUE)
    kw  <- sum(uw[keep, on = unit, nomatch = 0][[weight]], na.rm = TRUE)
    ret_weight <- if(tot > 0) kw / tot else NA_real_
  }
  ret_check <- if(!is.na(ret_weight)) ret_weight else ret_count

  # Per-cell report: valid units before pruning; after = n_keep (kept units cover
  # every required cell by construction).
  before <- unique(valid_req[, c(unit, cell), with = FALSE])[, .(n_valid_before = .N), by = cell]
  report <- merge(grid, before, by = cell, all.x = TRUE)
  report[is.na(n_valid_before), n_valid_before := 0L]
  report[, `:=`(n_after = n_keep, retention_count = ret_count, retention_weight = ret_weight)]

  if(verbose){
    message(sprintf("balance_scenarios: %d of %d units complete across %d cells (%.1f%% by count%s)%s",
                    n_keep, n_all, n_required, 100 * ret_count,
                    if(!is.na(ret_weight)) sprintf(", %.1f%% by weight", 100 * ret_weight) else "",
                    if(prun_to_rebalance) "" else "  [report only, not pruned]"))
  }

  if(prun_to_rebalance && !is.na(ret_check) && ret_check < min_retention){
    stop(sprintf("balance_scenarios: retention %.1f%% < min_retention %.1f%%; aborting rather than prune the sample this heavily.",
                 100 * ret_check, 100 * min_retention))
  }

  if(!prun_to_rebalance){
    data.table::setattr(dt, "balance_report", report[])
    return(dt[])
  }

  out <- dt[keep, on = unit]
  data.table::setattr(out, "balance_report", report[])
  out[]
}
