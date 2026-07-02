test_that("Equation (3) RRER matches its definition", {
  # RRER = 1(VRS<1) 1(ITS>1) (1-VRS)/(ITS-1)
  expect_equal(risk_reduction_efficiency(its = 1.05, vrs = 0.80),
               (1 - 0.80) / (1.05 - 1))
  expect_equal(risk_reduction_efficiency(its = 0.98, vrs = 0.80), 0)  # mean not lifted
  expect_equal(risk_reduction_efficiency(its = 1.05, vrs = 1.10), 0)  # variability up
  expect_equal(income_transfer_score(110, 100), 1.1)
  expect_equal(variability_reduction_score(0.8, 1.0), 0.8)
})

test_that("compute_efficiency_scores returns the expected score columns", {
  d <- simulate_example_outcomes(n_groups = 3, n_draws = 200, seed = 42)
  d <- build_outcome(d, base_value = "outcome", transfers = "indemnity", premium = "premium")
  s <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"))
  expect_true(all(c("mean_index", "risk_index", "efficiency",
                    "reduces_risk", "mean_gain_pct", "risk_reduction_pct") %in% names(s)))
  expect_true(all(is.finite(s$mean_index)))
  # efficiency is exactly 0 whenever the configuration does not reduce risk
  expect_true(all(s$efficiency[!s$reduces_risk] == 0))
  # efficiency is non-negative
  expect_true(all(s$efficiency >= 0))
})

test_that("build_agent_outcomes accepts a function indemnity and feeds scoring", {
  set.seed(1)
  agents <- list(
    a = data.frame(outcome = pmax(0, rnorm(150, 600, 150))),
    b = data.frame(outcome = pmax(0, rnorm(150, 550, 200)))
  )
  indem <- function(df) pmax(0, 600 - df$outcome) * 0.70
  d <- build_agent_outcomes(agents, indemnity = indem, premium = 25, subsidy = 0.62)
  expect_true(all(c("outcome_baseline", "outcome_treated", "indemnity",
                    "producer_premium", "subsidy") %in% names(d)))
  expect_identical(sort(unique(d$agent_id)), c("a", "b"))
  s <- compute_efficiency_scores(d, by = "agent_id")
  expect_equal(nrow(s), 2L)
})

test_that("run_efficiency_analysis produces scores and regime deltas", {
  d <- simulate_example_outcomes()
  res <- run_efficiency_analysis(
    d, by = c("group_id", "scenario"),
    outcome_args = list(base_value = "outcome", transfers = "indemnity", premium = "premium"))
  expect_true(!is.null(res$scores))
  expect_true(!is.null(res$deltas))
  expect_true("delta" %in% names(res$deltas))
})

test_that("trim_mean trims tails and supports weights", {
  x <- c(1, 2, 3, 4, 1000)
  # At the 10th/90th pctiles both tails are cut (1 and 1000) -> mean(2:4).
  expect_equal(trim_mean(x, lo = 0.10, hi = 0.90), mean(2:4))
  # Non-finite values are ignored; empty input -> NA.
  expect_equal(trim_mean(c(2, 4, NA, Inf), lo = 0, hi = 1), 3)
  expect_true(is.na(trim_mean(numeric(0))))
  # Equal weights reproduce the unweighted trimmed mean.
  expect_equal(trim_mean(x, lo = 0.10, hi = 0.90, weights = rep(1, 5)),
               trim_mean(x, lo = 0.10, hi = 0.90))
  # A zero weight on an observation is equivalent to removing it.
  expect_equal(trim_mean(c(1, 2, 3, 100), lo = 0, hi = 1,
                         weights = c(1, 1, 1, 0)),
               2)
})

test_that("summarise_scores collapses scores and honours weights", {
  d <- simulate_example_outcomes(n_groups = 6, n_draws = 200, seed = 7)
  d <- build_outcome(d, base_value = "outcome", transfers = "indemnity", premium = "premium")
  s <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"))

  out <- summarise_scores(s, by = "scenario")
  cols <- efficiency_score_columns()
  # Every metric is summarised (mean + median) and every flag as a pct_* share.
  expect_true(all(c("n_producers", cols$scores, cols$moments) %in% names(out)))
  expect_true(all(paste0("med_", c(cols$scores, cols$moments)) %in% names(out)))
  expect_true(all(paste0("pct_", cols$flags) %in% names(out)))
  expect_equal(nrow(out), length(unique(s$scenario)))
  expect_true(all(out$pct_reduces_risk >= 0 & out$pct_reduces_risk <= 100))

  # Dropping moments and medians shrinks the output as requested.
  lean <- summarise_scores(s, by = "scenario", medians = FALSE, include_moments = FALSE)
  expect_false(any(cols$moments %in% names(lean)))
  expect_false(any(grepl("^med_", names(lean))))
  expect_true(all(cols$scores %in% names(lean)))

  # by = NULL -> single overall row.
  expect_equal(nrow(summarise_scores(s, by = NULL)), 1L)

  # Constant weights reproduce the unweighted summary (all metric means).
  s[, wt := 1]
  wtd <- summarise_scores(s, by = "scenario", weight = "wt")
  unw <- summarise_scores(s, by = "scenario")
  for (m in c(cols$scores, cols$moments)) expect_equal(wtd[[m]], unw[[m]])

  # Unknown weight column is rejected.
  expect_error(summarise_scores(s, by = "scenario", weight = "not_a_col"))
})

test_that("score_dictionary documents every scored column", {
  skip_if_not(exists("score_dictionary"))   # built by data-raw/scripts/build_score_dictionary.R
  expect_setequal(names(score_dictionary),
                  c("variable", "group", "description", "calculation", "interpretation"))
  cols <- efficiency_score_columns()
  expect_true(all(c(cols$scores, cols$moments, cols$flags) %in% score_dictionary$variable))
})
