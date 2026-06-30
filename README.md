rfsnEfficiency: Risk-reduction efficacy and efficiency of
risk-management configurations
================

- [📖 Introduction](#open_book-introduction)
- [✨ Key Features](#sparkles-key-features)
- [📦 Installation](#package-installation)
- [🚀 Quick Start](#rocket-quick-start)
- [🧮 Example 1: Score a
  configuration](#abacus-example-1-score-a-configuration)
- [🧩 Example 2: Compare regimes](#jigsaw-example-2-compare-regimes)
- [🧠 Example 3: Bring your own
  outcomes](#brain-example-3-bring-your-own-outcomes)
- [📉 Example 4: Any outcome, profit or a cost-side
  product](#chart_with_downwards_trend-example-4-any-outcome-profit-or-a-cost-side-product)
- [📚 Citation](#books-citation)
- [🤝 Contributing](#handshake-contributing)
- [📬 Contact](#mailbox_with_mail-contact)

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg"></a>
<a href="https://lifecycle.r-lib.org/articles/stages.html#experimental"><img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg"></a>
<a href="https://github.com/ftsiboe/rfsnEfficiency/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/ftsiboe/rfsnEfficiency/actions/workflows/R-CMD-check.yaml/badge.svg"></a>
<a href="https://codecov.io/gh/ftsiboe/rfsnEfficiency"><img src="https://codecov.io/gh/ftsiboe/rfsnEfficiency/graph/badge.svg?token=6AEE8ygfD0"></a>
<img src="https://img.shields.io/badge/R-%3E=4.1-blue">
<a><img src="https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg"></a>
<img src="https://img.shields.io/badge/License-GPLv3-blue.svg">
<!-- badges: end -->

# 📖 Introduction

`rfsnEfficiency` is a general, **program-agnostic** toolkit for the
risk-reduction **efficacy** and **efficiency** of risk-management or
safety-net configurations. It separates two questions:

- **Efficacy**: does a configuration lift the mean outcome
  (`mean_index`) and cut its variability (`risk_index`)?
- **Efficiency**: how much risk reduction does it buy *per unit of
  outcome transferred* (`efficiency`, the Risk-Reduction Efficiency
  Ratio)?

Both are computed on a **configurable outcome** (revenue, profit,
margin, or a custom / cost-side measure) relative to a no-program
baseline, and compared across regimes or scenarios. You bring the
outcome draws; the package brings the metrics; there are **no external
data dependencies**.

Its methodological foundations build on a connected body of work:

- Tsiboe, F., Turner, D., Williams, B., Miller, M., Baldwin, K., &
  Dohlman, E. (2025). [**Risk reduction impacts of crop insurance in the
  United States**](https://doi.org/10.1002/aepp.13513). *Applied
  Economic Perspectives and Policy, 47(5)*, 1832–1847. *(source of the
  ITS / VRS / RRER metrics).*
- Tsiboe, F., Tack, J., & Yu, J. (2023). [**Farm-level evaluation of
  area- and agroclimatic-based index
  insurance**](https://doi.org/10.1002/jaa2.77). *Journal of the
  Agricultural and Applied Economics Association, 2(4)*, 616–633. *(the
  farm-level risk-reduction / basis-risk evaluation framework).*
- Tsiboe, F., Turner, D., & Yu, J. (2025). [**Utilizing large-scale
  insurance data sets to calibrate sub-county level crop
  yields**](https://doi.org/10.1111/jori.12494). *Journal of Risk and
  Insurance, 92(1)*, 139–165. *(calibration of the end-of-season yields
  underpinning the outcome draws).*
- Gaku, S., & Tsiboe, F. (2025). [**Evaluation of alternative farm
  safety net program combination
  strategies**](https://doi.org/10.1108/AFR-11-2023-0150). *Agricultural
  Finance Review, 85(2)*, 254–273.
- Tsiboe, F., & Turner, D. (2025). [**Incorporating buy-up price loss
  coverage into the United States farm safety
  net**](https://doi.org/10.1002/aepp.13536). *Applied Economic
  Perspectives and Policy, 47.*

> **Disclaimer:** The metrics implemented here originate in research
> using USDA data, but this package ships no data and is not endorsed by
> or affiliated with USDA or any government agency. See
> [LICENSE](LICENSE) for terms.

> 📐 **Conceptual framework:** [Why net farm income is an incomplete
> yardstick for the farm safety net](vignettes/conceptualization.Rmd)
> walks through the seminal framework and shows how a
> risk-reduction-efficiency indicator **completes** the picture that
> annual net farm income (NFI) leaves incomplete for the U.S. farm
> safety net.

------------------------------------------------------------------------

# ✨ Key Features

- **Configurable outcome.** Score revenue, profit, margin, a cost-side
  product, or any custom measure through one constructor; the scoring
  code never changes.
- **Bring your own outcomes.** Supply end-of-season draws per agent;
  `cost`, `premium`, `subsidy`, and the **indemnity rule may each be a
  function**.
- **Canonical metrics.** Generic `income_transfer_score()` (ITS),
  `variability_reduction_score()` (VRS), and
  `risk_reduction_efficiency()` (RRER, Equation 3 of Tsiboe et
  al. 2025).
- **Efficacy vs. efficiency.** Clear separation of “does it work” (ITS,
  VRS) from “is it worth it” (RRER).
- **Three variability bases.** Total (CV), downside semivariance, and
  loss-conditional shortfall, with headline plus robustness flavors.
- **Regime / scenario comparison.** Automatic `alt − ref` deltas for
  every score via `regime_deltas()` / `run_efficiency_analysis()`.
- **Standalone & tested.** No program-specific terms, no external data,
  and a `testthat` suite.

------------------------------------------------------------------------

# 📦 Installation

``` r
# Install from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("ftsiboe/rfsnEfficiency", force = TRUE, upgrade = "never")
```

------------------------------------------------------------------------

# 🚀 Quick Start

The core entry points are:

- `build_agent_outcomes()` / `build_outcome()` → construct the outcome
  and its no-program baseline
- `compute_efficiency_scores()` → efficacy + efficiency scores by group
- `run_efficiency_analysis()` → one-call driver: outcomes → scores →
  regime deltas

The package ships a small committed dataset, `toy_outcomes`, built from
the [USFarmSafetyNetLab `calibrated_yield`
release](https://github.com/ftsiboe/USFarmSafetyNetLab/releases/tag/calibrated_yield)
by `data-raw/scripts/build_toy_dataset.R`. Each insurance pool’s
calibrated yields *across years* are its per-agent outcome draws, with
an illustrative indemnity and premium under two regimes (`baseline` =
75% guarantee, `reform` = 85% guarantee at a lower premium). It is used
throughout the examples below.

------------------------------------------------------------------------

# 🧮 Example 1: Score a configuration

Compute the efficacy and efficiency scores by pool and regime.

``` r
library(rfsnEfficiency)
data(toy_outcomes)

d  <- build_outcome(toy_outcomes, base_value = "outcome",
                    transfers = "indemnity", premium = "premium")
sc <- compute_efficiency_scores(d, by = c("group_id", "regime"))

sc[, .(group_id, regime, mean_index, risk_index, efficiency, reduces_risk)]
```

> **Discussion:** `mean_index > 1` and `risk_index < 1` mean the
> configuration both lifts mean outcome and cuts variability (it
> “reduces risk”); `efficiency` is the risk reduction bought per unit of
> outcome transferred. For a full walk-through of the data and results,
> see [Example: Discussion of
> Results](data-raw/examples/Example_Discussion.md).

------------------------------------------------------------------------

# 🧩 Example 2: Compare regimes

`run_efficiency_analysis()` scores each regime and differences them.

``` r
res <- run_efficiency_analysis(
  data = toy_outcomes, by = "group_id",
  outcome_args = list(base_value = "outcome", transfers = "indemnity",
                      premium = "premium"),
  ref = "baseline", alt = "reform")

res$scores   # mean_index, risk_index, efficiency, ... per regime
res$deltas   # reform - baseline for every score
```

> **Note:** With no `ref`/`alt` supplied, the first two (sorted) regime
> labels are differenced. A fully synthetic, zero-setup alternative is
> `simulate_example_outcomes()`.

------------------------------------------------------------------------

# 🧠 Example 3: Bring your own outcomes

Supply each agent’s end-of-season draws and the program parameters; the
indemnification rule can be a **function**.

``` r
set.seed(1)
agents <- list(
  unitA = data.frame(outcome = pmax(0, rnorm(200, 600, 150))),
  unitB = data.frame(outcome = pmax(0, rnorm(200, 550, 200)))
)
indem <- function(df) pmax(0, 600 - df$outcome) * 0.70  # 70% of shortfall below 600

d <- build_agent_outcomes(agents, indemnity = indem, premium = 25, subsidy = 0.62)
compute_efficiency_scores(d, by = "agent_id")
```

------------------------------------------------------------------------

# 📉 Example 4: Any outcome, profit or a cost-side product

The same engine scores profit (subtract a `cost`) or a product that pays
only when **costs rise** (`cost_protection`), with no revenue-side
protection.

``` r
# Profit: subtract a (possibly stochastic) cost series
d_profit <- build_outcome(df, base_value = "revenue", cost = "input_cost",
                          transfers = "indemnity")

# Cost-only product: indemnity triggered by rising cost, no yield/revenue cover
d_margin <- build_outcome(df, base_value = "revenue", cost = "input_cost",
                          transfers = NULL,
                          cost_protection = "margin_indemnity",
                          premium = "margin_premium")
```

> **Tip:** Because `cost` can be a stochastic per-draw series, cost risk
> flows into the outcome, which is what lets a margin / input-cost
> product reduce variability.

------------------------------------------------------------------------

# 📚 Citation

If you use `rfsnEfficiency`, please cite the metric source:

- Tsiboe, F., Turner, D., Williams, B., Miller, M., Baldwin, K., &
  Dohlman, E. (2025). *Risk reduction impacts of crop insurance in the
  United States.* *Applied Economic Perspectives and Policy, 47(5)*,
  1832–1847. <https://doi.org/10.1002/aepp.13513>

------------------------------------------------------------------------

# 🤝 Contributing

Contributions, issues, and feature requests are welcome. Please see the
[Code of Conduct](code_of_conduct.md).

------------------------------------------------------------------------

# 📬 Contact

Questions or collaboration ideas? Email **Francis Tsiboe** at
<ftsiboe@hotmail.com>. ⭐ *Star this repository if you find it useful!*
