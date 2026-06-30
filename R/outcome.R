# The risk-reduction scores run on ONE "outcome" series and its no-safety-net
# baseline. This constructor makes that outcome configurable so the same engine
# yields revenue-based, profit-based, cost-side, or any custom metric without
# touching the scoring code:
#
#   outcome_baseline = base_value - cost
#   outcome_treated  = outcome_baseline + sum(transfers, cost_protection) - premium
#
# `cost` may be a STOCHASTIC per-observation series, so cost risk flows into the
# outcome -- which lets a cost-side product (margin / input-cost protection)
# reduce variability. Such a product pays when cost RISES, supplied via
# `cost_protection`, and can stand alone with no revenue/yield protection.

#' Construct the analysis outcome and its no-safety-net baseline
#'
#' @param data data.frame/data.table with the component columns.
#' @param base_value column name of the gross pre-safety-net outcome
#'   (e.g., "revenue"; for profit pass revenue plus a `cost`).
#' @param cost optional column subtracted from `base_value`. May be a stochastic
#'   per-observation series so cost risk enters the outcome (enabling cost-side
#'   products). `NULL` = revenue; a production-cost column = profit.
#' @param transfers character vector of revenue/yield-side payout columns ADDED
#'   to the treated outcome (e.g., `c("indemnity","title1_payment")`). `NULL`
#'   for products that provide no revenue-side protection.
#' @param cost_protection character vector of cost-side payout columns (a product
#'   that pays when costs rise) ADDED to the treated outcome. `NULL` if none.
#' @param premium column SUBTRACTED from the treated outcome (producer-paid
#'   premium for whichever product(s) are active); `NULL` to ignore.
#' @param floor_at numeric floor applied to both series (default 0; `NA` = none).
#' @return `data` with new columns `outcome_baseline` and `outcome_treated`.
#'
#' @examples
#' \dontrun{
#' build_outcome(d, base_value = "revenue", cost = NULL)                 # revenue
#' build_outcome(d, base_value = "revenue", cost = "input_cost")         # profit
#' build_outcome(d, "revenue", cost = "input_cost", transfers = NULL,    # cost-only
#'               cost_protection = "margin_indemnity", premium = "margin_premium")
#' }
#' @export
build_outcome <- function(data,
                          base_value      = "revenue",
                          cost            = NULL,
                          transfers       = "indemnity",
                          cost_protection = NULL,
                          premium         = "producer_premium",
                          floor_at        = 0) {
  col <- function(nm) if (!is.null(nm) && nm %in% names(data)) as.numeric(data[[nm]]) else NULL
  if (!base_value %in% names(data)) stop("`base_value` column not found: ", base_value)

  base_v <- as.numeric(data[[base_value]])
  cost_v <- if (is.null(cost))    0 else { v <- col(cost);    if (is.null(v)) stop("cost col missing: ", cost) else v }
  prem_v <- if (is.null(premium)) 0 else { v <- col(premium); if (is.null(v)) 0 else v }

  payout_l <- Filter(Negate(is.null), lapply(c(transfers, cost_protection), col))
  payout_v <- if (length(payout_l)) Reduce(`+`, payout_l) else 0

  baseline <- base_v - cost_v
  treated  <- baseline + payout_v - prem_v
  if (!is.na(floor_at)) { baseline <- pmax(baseline, floor_at); treated <- pmax(treated, floor_at) }

  data[["outcome_baseline"]] <- baseline
  data[["outcome_treated"]]  <- treated
  data
}
