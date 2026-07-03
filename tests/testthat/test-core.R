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

test_that("ITS and VRS clamp finite ratios but keep NA for undefined ones", {
  # Default clamp c(0.10, 2): extreme finite ratios are winsorized.
  expect_equal(income_transfer_score(300, 100), 2)      # 3.0 -> cap 2
  expect_equal(income_transfer_score(1, 100), 0.10)     # 0.01 -> floor 0.10
  expect_equal(income_transfer_score(110, 100), 1.1)    # inside bounds, untouched
  expect_equal(variability_reduction_score(50, 1), 2)   # 50 -> cap 2
  expect_equal(variability_reduction_score(0.01, 1), 0.10)
  expect_equal(variability_reduction_score(0.8, 1.0), 0.8)

  # clamp = NULL restores the raw ratio.
  expect_equal(income_transfer_score(300, 100, clamp = NULL), 3)
  expect_equal(variability_reduction_score(50, 1, clamp = NULL), 50)

  # Undefined ratios stay NA (a zero denominator), not clamped to a bound.
  expect_true(is.na(income_transfer_score(1, 0)))
  expect_true(is.na(variability_reduction_score(1, 0)))

  # Clamping flows through compute_efficiency_scores, and the two clamps are
  # independent (its_clamp vs vrs_clamp).
  d <- simulate_example_outcomes(n_groups = 4, n_draws = 200, seed = 3)
  d <- build_outcome(d, base_value = "outcome", transfers = "indemnity", premium = "premium")
  s <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"))
  expect_true(all(s$mean_index >= 0.10 & s$mean_index <= 2, na.rm = TRUE))
  expect_true(all(s$cv_index   >= 0.10 & s$cv_index   <= 2, na.rm = TRUE))

  s2 <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"),
                                  its_clamp = c(0.9, 1.1), vrs_clamp = c(0, 5))
  expect_true(all(s2$mean_index >= 0.9 & s2$mean_index <= 1.1, na.rm = TRUE))
  expect_true(all(s2$cv_index   >= 0   & s2$cv_index   <= 5,   na.rm = TRUE))

  # its_clamp = NULL leaves mean_index unclamped while vrs_clamp still binds.
  s3 <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"),
                                  its_clamp = NULL, vrs_clamp = c(0.10, 2))
  expect_true(all(s3$cv_index >= 0.10 & s3$cv_index <= 2, na.rm = TRUE))
})

test_that("calc_insurance_payment_rate returns loss-triggered rates in [0,1]", {
  pr <- function(fy) calc_insurance_payment_rate(
    expected_yield = 100, final_yield = fy, harvest_price = 5,
    price_election_amount = 5, projected_price = 5, trigger_index = 0.85,
    coverage_range = 1, insurance_plan_code = 1)$payment_rate
  expect_equal(pr(100), 0)      # no loss -> no payment
  expect_equal(pr(50),  0.35)   # 50% yield -> 0.85 - 0.50
  expect_equal(pr(0),   0.85)   # total loss -> capped at trigger
  # Vectorized and bounded to [0, 1].
  v <- calc_insurance_payment_rate(
    expected_yield = 100, final_yield = c(120, 85, 0), harvest_price = 5,
    price_election_amount = 5, projected_price = 5, trigger_index = 0.85,
    coverage_range = 1, insurance_plan_code = 1)$payment_rate
  expect_true(all(v >= 0 & v <= 1))
})

test_that("RRER deadband/cap bound the efficiency score", {
  # Default deadband 0.05: a mean gain below it yields 0 (guards the ITS->1 blowup).
  expect_equal(risk_reduction_efficiency(1.001, 0.5), 0)
  # deadband = 0 recovers the raw ratio (large but finite).
  expect_equal(risk_reduction_efficiency(1.001, 0.5, deadband = 0),
               (1 - 0.5) / (1.001 - 1))
  # At the deadband boundary the score is bounded by 1/deadband.
  expect_equal(risk_reduction_efficiency(1.05, 0.0, deadband = 0.05), 20)
  expect_true(risk_reduction_efficiency(1.0000001, 0.0, deadband = 0.05) <= 20)
  # Hard cap applies on top.
  expect_equal(risk_reduction_efficiency(1.06, 0.10, deadband = 0.05, cap = 10), 10)
  # The original test value is unaffected (1.05 sits at the boundary, included).
  expect_equal(risk_reduction_efficiency(1.05, 0.80), (1 - 0.80) / (1.05 - 1))

  # Bound flows through compute_efficiency_scores.
  d <- simulate_example_outcomes(n_groups = 4, n_draws = 200, seed = 5)
  d <- build_outcome(d, base_value = "outcome", transfers = "indemnity", premium = "premium")
  s <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"),
                                 eff_deadband = 0.05)
  expect_true(all(s$efficiency >= 0 & s$efficiency <= 20, na.rm = TRUE))
  s2 <- compute_efficiency_scores(d, by = c("group_id", "scenario", "regime"),
                                  eff_deadband = 0.05, eff_cap = 5)
  expect_true(all(s2$efficiency <= 5, na.rm = TRUE))
})

test_that("balance_scenarios prunes units missing from any required cell", {
  library(data.table)
  ids <- 1:10
  dt  <- CJ(id = ids, env = c("E1", "E2"), arm = c("A", "B"))
  set.seed(11); dt[, m1 := rnorm(.N)]

  # Fully balanced input -> nothing pruned.
  bal <- balance_scenarios(dt, unit = "id", cell = c("env", "arm"),
                           require_finite = "m1", verbose = FALSE)
  expect_equal(uniqueN(bal$id), 10L)
  expect_equal(nrow(bal), nrow(dt))

  # One NA in a single cell drops that unit from every cell.
  d2 <- copy(dt); d2[id == 3 & env == "E2" & arm == "B", m1 := NA_real_]
  b2 <- balance_scenarios(d2, unit = "id", cell = c("env", "arm"),
                          require_finite = "m1", verbose = FALSE)
  expect_false(3 %in% b2$id)
  expect_equal(uniqueN(b2$id), 9L)
  # Every retained unit covers the full env x arm grid.
  expect_true(all(b2[, .N, by = id]$N == 4L))

  # allow_missing whitelists the arm == "B" cells, so id 3 is no longer penalised.
  b3 <- balance_scenarios(d2, unit = "id", cell = c("env", "arm"),
                          require_finite = "m1",
                          allow_missing = list(arm = "B"), verbose = FALSE)
  expect_true(3 %in% b3$id)
  expect_equal(uniqueN(b3$id), 10L)

  # Report-only leaves the data untouched but attaches the balance report.
  rep_only <- balance_scenarios(d2, unit = "id", cell = c("env", "arm"),
                                require_finite = "m1",
                                prun_to_rebalance = FALSE, verbose = FALSE)
  expect_equal(nrow(rep_only), nrow(d2))
  expect_true(3 %in% rep_only$id)
  expect_false(is.null(attr(rep_only, "balance_report")))

  # Heavy pruning below min_retention aborts.
  d4 <- copy(dt); d4[id %in% 1:6 & env == "E1" & arm == "A", m1 := NA_real_]
  expect_error(
    balance_scenarios(d4, unit = "id", cell = c("env", "arm"),
                      require_finite = "m1", min_retention = 0.5, verbose = FALSE),
    "retention")
})

test_that("score_dictionary documents every scored column", {
  skip_if_not(exists("score_dictionary"))   # built by data-raw/scripts/build_score_dictionary.R
  expect_setequal(names(score_dictionary),
                  c("variable", "group", "description", "calculation", "interpretation"))
  cols <- efficiency_score_columns()
  expect_true(all(c(cols$scores, cols$moments, cols$flags) %in% score_dictionary$variable))
})
