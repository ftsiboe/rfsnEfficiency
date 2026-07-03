# Standalone insurance payment-rate (indemnification) calculator. Copied and
# renamed from arpcFCIPcalc::arpc_fcip_calc_payment_rate() so rfsnEfficiency has
# no external FCIP dependency; the math is identical (the unused `control`
# argument was dropped). Backs the net-benefit builders in net_benefit.R.

#' Calculate an insurance payment rate (indemnification)
#'
#' Computes a payment rate in `[0, 1]` from expected and realized yields plus
#' price inputs, and returns the intermediate guarantee / revenue-to-count
#' quantities. Vectorized over its inputs.
#'
#' @param expected_yield Numeric. Expected yield level (e.g. APH / approved yield).
#' @param final_yield Numeric. Realized harvested yield.
#' @param harvest_price Numeric. Market price at harvest (used for
#'   revenue-to-count in some plans).
#' @param price_election_amount Numeric. Elected price at sign-up (typically
#'   `projected_price * price_election_percent`).
#' @param projected_price Numeric. Projected / initial price used for guarantee
#'   construction and caps.
#' @param trigger_index Numeric. Coverage trigger as a proportion (e.g. the
#'   coverage level in `[0, 1]`).
#' @param coverage_range Numeric. Coverage range width; default `1`.
#' @param insurance_plan_code Character or numeric. Plan code that determines
#'   whether the guarantee may use harvest price and whether revenue-to-count
#'   uses the harvest or the elected price.
#'
#' @details
#' Plan-code logic is implementation-specific: `insurance_plan_code` membership
#' tests decide (a) whether harvest price can raise the guarantee, and (b)
#' whether revenue-to-count uses `harvest_price` or `price_election_amount`.
#' Ensure your plan-code conventions match those encoded here.
#'
#' @return A named list with `new_price_election_amount`,
#'   `new_insurance_guarantee`, `revenue_to_count`, and `payment_rate`.
#' @seealso [simulate_basic_policy_net_benefit()],
#'   [simulate_supplemental_net_benefit()]
#' @export
calc_insurance_payment_rate <- function(
    expected_yield,
    final_yield,
    harvest_price,
    price_election_amount,
    projected_price,
    trigger_index,
    coverage_range = 1,
    insurance_plan_code){

  # Insurance guarantee at signup
  insurance_guarantee <- expected_yield * price_election_amount

  # Adjusted price election amount (harvest-price inclusion logic)
  new_price_election_amount <- pmax(
    pmin(2 * projected_price, harvest_price, na.rm = TRUE),
    price_election_amount, na.rm = TRUE)

  new_insurance_guarantee <- expected_yield * ifelse(
    insurance_plan_code %in% c(2, 5, 16),
    new_price_election_amount,
    price_election_amount)

  # Revenue to count
  revenue_to_count <- final_yield * ifelse(
    insurance_plan_code %in% c(1, 3, 90, 4, 6, 16),
    price_election_amount,
    harvest_price)

  # Payment rate calculation with zero-division guard
  payment_rate <- pmin(round(pmax(
    (trigger_index - (revenue_to_count / new_insurance_guarantee)) / coverage_range,
    0, na.rm = TRUE), 3), 1, na.rm = TRUE)

  list(
    new_price_election_amount = new_price_election_amount,
    new_insurance_guarantee   = new_insurance_guarantee,
    revenue_to_count          = revenue_to_count,
    payment_rate              = payment_rate
  )
}
