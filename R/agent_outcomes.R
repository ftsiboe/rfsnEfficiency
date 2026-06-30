# Generic per-agent outcome construction. The user supplies underlying
# end-of-season outcomes (ideally one element per agent) plus program parameters
# (cost, premium, subsidy) and an indemnification rule that MAY be a function.
# The result stacks to a data.table that compute_efficiency_scores() consumes.

#' Resolve a program parameter against an agent's draw data
#'
#' A parameter may be (a) a `function(agent_df)` returning a per-draw vector,
#' (b) the name of a column in `agent_df`, or (c) a numeric scalar/vector
#' (recycled to `n`). Used internally by `build_agent_outcomes()`.
#' @keywords internal
.resolve_param <- function(p, df, n) {
  if (is.function(p)) return(as.numeric(p(df)))
  if (is.character(p) && length(p) == 1L && p %in% names(df)) return(as.numeric(df[[p]]))
  v <- as.numeric(p)
  if (length(v) == 1L) v <- rep(v, n)
  if (length(v) != n) stop("parameter length (", length(v), ") != number of draws (", n, ")")
  v
}

#' Build per-agent baseline/treated outcomes from end-of-season draws + parameters
#'
#' The flexible entry point: hand in each agent's underlying end-of-season
#' outcomes and the program parameters, and get back stacked baseline/treated
#' outcome draws ready for `compute_efficiency_scores()`.
#'
#' \deqn{outcome\_baseline = outcome - cost}
#' \deqn{outcome\_treated  = outcome\_baseline + indemnity - producer\_premium}
#'
#' @param agents a list with one element per agent. Each element is either a
#'   numeric vector of the base end-of-season outcome (draws), or a
#'   data.frame/data.table of draws (rows = crop years / Monte Carlo draws)
#'   containing at least the `outcome` column. List names, if present, become
#'   agent ids.
#' @param indemnity indemnification rule: a `function(agent_df)` returning a
#'   per-draw indemnity vector, OR a column name in each agent's data, OR a
#'   numeric scalar/vector. Indemnity is ADDED to the treated outcome.
#' @param outcome name of the base end-of-season outcome column (default
#'   "outcome"); ignored when an agent element is a plain numeric vector.
#' @param cost,premium,subsidy program parameters, each a `function(agent_df)`,
#'   a column name, or a numeric scalar/vector. `cost` is subtracted from the
#'   base outcome (enters both baseline and treated). `premium` is the
#'   producer-paid premium, subtracted from the treated outcome. `subsidy` is
#'   carried through for program-context summaries (not added to the outcome).
#' @param agent_id_col name of the agent id column in the output (default "agent_id").
#' @param extra_cols optional character vector of additional per-draw columns to
#'   carry through from each agent's data (e.g., "commodity_code", "combination",
#'   "regime", "commodity_year").
#' @param floor_at numeric floor applied to both outcome series (default 0;
#'   `NA` = none).
#' @return a `data.table` stacked over agents with `agent_id`, `draw`,
#'   `outcome_baseline`, `outcome_treated`, and the resolved `cost`,
#'   `producer_premium`, `subsidy`, `indemnity` columns (plus `extra_cols`).
#'
#' @examples
#' # Two agents, each with 100 end-of-season revenue draws; indemnity is a
#' # function paying 70% of the shortfall below a 600 guarantee.
#' set.seed(1)
#' agents <- list(
#'   cornA = data.frame(outcome = pmax(0, rnorm(100, 600, 150))),
#'   cornB = data.frame(outcome = pmax(0, rnorm(100, 550, 200)))
#' )
#' indem <- function(df) pmax(0, 600 - df$outcome) * 0.70
#' d <- build_agent_outcomes(agents, indemnity = indem, premium = 25, subsidy = 0.62)
#' compute_efficiency_scores(d, by = "agent_id")[, .(agent_id, mean_index, risk_index, efficiency)]
#' @export
build_agent_outcomes <- function(agents, indemnity, outcome = "outcome",
                                 cost = 0, premium = 0, subsidy = 0,
                                 agent_id_col = "agent_id", extra_cols = NULL,
                                 floor_at = 0) {
  if (!is.list(agents) || is.data.frame(agents)) {
    stop("`agents` must be a list with one element per agent.")
  }
  ids <- names(agents); if (is.null(ids)) ids <- as.character(seq_along(agents))

  out <- data.table::rbindlist(lapply(seq_along(agents), function(i) {
    a <- agents[[i]]
    df <- if (is.data.frame(a)) data.table::as.data.table(a) else data.table::data.table(outcome = as.numeric(a))
    if (!is.data.frame(a)) outcome_col <- "outcome" else outcome_col <- outcome
    if (!outcome_col %in% names(df)) stop("agent ", ids[i], ": outcome column '", outcome_col, "' not found.")

    n     <- nrow(df)
    base  <- as.numeric(df[[outcome_col]])
    cst   <- .resolve_param(cost,      df, n)
    prem  <- .resolve_param(premium,   df, n)
    suby  <- .resolve_param(subsidy,   df, n)
    indem <- .resolve_param(indemnity, df, n)

    baseline <- base - cst
    treated  <- baseline + indem - prem
    if (!is.na(floor_at)) { baseline <- pmax(baseline, floor_at); treated <- pmax(treated, floor_at) }

    res <- data.table::data.table(
      agent_id         = ids[i],
      draw             = seq_len(n),
      outcome_baseline = baseline,
      outcome_treated  = treated,
      cost             = cst,
      producer_premium = prem,
      subsidy          = suby,
      indemnity        = indem)
    if (!is.null(extra_cols)) {
      keep <- intersect(extra_cols, names(df))
      if (length(keep)) res <- cbind(res, df[, keep, with = FALSE])
    }
    res
  }), fill = TRUE)

  data.table::setnames(out, "agent_id", agent_id_col)
  out[]
}
