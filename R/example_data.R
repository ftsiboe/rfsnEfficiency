#' Simulate a tiny example outcomes dataset (no external data required)
#'
#' Generates synthetic per-draw outcomes for two regimes and a few scenarios so
#' the package can be demonstrated, tested, and learned standalone. This is an
#' ILLUSTRATIVE generator, not a calibrated model -- the numbers carry no meaning.
#'
#' @param n_groups number of groups (e.g., units / commodities / portfolios).
#' @param n_draws draws (years / Monte Carlo) per group per scenario per regime.
#' @param regimes character vector of regime labels.
#' @param seed RNG seed for reproducibility.
#' @return a `data.table` with `group_id`, `scenario`, `regime`, `outcome`,
#'   `indemnity`, and `premium`.
#' @examples
#' d <- simulate_example_outcomes(n_groups = 3, n_draws = 50)
#' head(d)
#' @export
simulate_example_outcomes <- function(n_groups = 6, n_draws = 200,
                                      regimes = c("baseline", "reform"), seed = 1) {
  set.seed(seed)
  scenarios <- c("none", "partial", "full")
  grid <- expand.grid(group_id = seq_len(n_groups), scenario = scenarios,
                      regime = regimes, stringsAsFactors = FALSE)

  rows <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    g       <- grid[i, ]
    outcome <- pmax(0, stats::rnorm(n_draws, mean = 600, sd = 180))
    boost   <- if (g$regime == "reform") 1.25 else 1            # illustrative regime effect
    cover   <- switch(g$scenario, none = 0, partial = 0.40, full = 0.70)
    indemnity <- pmax(0, 600 - outcome) * cover * boost          # pays the shortfall
    premium   <- 25 * cover / boost
    data.frame(group_id = g$group_id, scenario = g$scenario, regime = g$regime,
               outcome = outcome, indemnity = indemnity, premium = premium,
               stringsAsFactors = FALSE)
  }))
  data.table::as.data.table(rows)
}
