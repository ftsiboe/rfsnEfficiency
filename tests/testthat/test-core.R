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
