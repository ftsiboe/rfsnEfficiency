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
