# Draw-level net-benefit builders for Monte-Carlo safety-net simulation
# (Tsiboe, Biram & Hagerman 2026). Both operate in place on a data.table that
# has been exploded to one row per agent x draw (see explode_draws()), and both
# derive a loss-contingent indemnity from calc_insurance_payment_rate():
#   * simulate_basic_policy_net_benefit() -> individual (farm) policy, farm draw
#   * simulate_supplemental_net_benefit() -> area (county) SCO/ECO, pool draw

#' Simulate a basic (individual) policy's per-draw net benefit
#'
#' Adds an individual, loss-contingent indemnity and a scenario net benefit to a
#' draw-level agent table. The indemnity is the M-13 payment rate applied to the
#' simulated farm yield draw (`yield_farm`) against the guarantee, times the
#' underlying liability; it is triggered at the coverage level (full payout at
#' total loss). Because the indemnity does not depend on the subsidy scenario it
#' is computed once (guarded on `underlying_indemnity_amount`) and reused.
#'
#' @param data data.table exploded to one row per agent x draw. Must contain
#'   `underlying_liability_amount`, `underlying_total_premium_amount`,
#'   `approved_yield`, `yield_farm`, `harvest_price`, `projected_price`,
#'   `coverage_level_percent`, `insurance_plan_code`, and the `subsidy_amount`
#'   column named below.
#' @param subsidy_amount name of the per-draw subsidy-amount column to add in.
#' @param net_benefit_label name of the net-benefit column to create
#'   (`indemnity - premium + subsidy`).
#' @param indemnity_label optional name for a copy of the per-draw indemnity to
#'   keep on `data`. `NULL` (default) leaves only `underlying_indemnity_amount`.
#' @return `data` (modified in place); invisibly, with
#'   `underlying_indemnity_amount`, `net_benefit_label`, and (if requested)
#'   `indemnity_label` added.
#' @export
simulate_basic_policy_net_benefit <- function(
    data,
    subsidy_amount,
    net_benefit_label,
    indemnity_label = NULL
){
  if(! "underlying_indemnity_amount" %in% names(data)){
    data[, underlying_indemnity_amount := underlying_liability_amount * calc_insurance_payment_rate(
      expected_yield        = approved_yield,
      final_yield           = yield_farm,
      harvest_price         = harvest_price,
      price_election_amount = projected_price,
      projected_price       = projected_price,
      trigger_index         = coverage_level_percent,
      coverage_range        = coverage_level_percent,
      insurance_plan_code   = insurance_plan_code
    )$payment_rate]
  }

  data[, (net_benefit_label) := underlying_indemnity_amount -
         underlying_total_premium_amount + get(subsidy_amount)]
  if(!is.null(indemnity_label)) data[, (indemnity_label) := underlying_indemnity_amount]
  data[]
}

#' Simulate a supplemental (area) SCO/ECO endorsement's per-draw net benefit
#'
#' Adds an area-triggered, loss-contingent supplemental net benefit to a
#' draw-level agent table. Supplemental liability is the underlying liability
#' scaled to the endorsement band (`area_loss_start_percent - area_loss_end_percent`);
#' premium and subsidy are actuarial. The indemnity is the M-13 payment rate on
#' the COUNTY (pool) draw (`final_county_yield` vs `expected_county_yield`), so
#' payouts follow the area outcome and basis risk is preserved. `trigger_index`
#' is the band top (coverage ratio) and `coverage_range` the band width; the
#' underlying `insurance_plan_code` selects yield vs revenue-to-count logic.
#'
#' @param data data.table exploded to one row per agent x draw. Must contain
#'   `underlying_liability_amount`, `coverage_level_percent`,
#'   `expected_county_yield`, `final_county_yield`, `harvest_price`,
#'   `projected_price`, `insurance_plan_code`, and the band/rate/subsidy columns
#'   named by the string arguments below.
#' @param area_loss_start_percent,area_loss_end_percent column names of the band
#'   top and bottom (coverage ratios).
#' @param base_rate column name of the supplemental base premium rate.
#' @param rate_differential optional column name of a multiplicative premium-rate
#'   differential (OBBBA recalibrations); `NULL` uses `base_rate` alone.
#' @param subsidy_percent column name of the supplemental subsidy share.
#' @param net_benefit_label name of the net-benefit column to create
#'   (`indemnity - premium + subsidy`).
#' @param indemnity_label optional name under which to keep a copy of the per-draw
#'   supplemental indemnity. `NULL` (default) drops it with the other
#'   intermediates.
#' @return `data` (modified in place); the intermediate supplemental columns are
#'   dropped, leaving `net_benefit_label` (and `indemnity_label` if requested).
#' @export
simulate_supplemental_net_benefit <- function(
    data,
    area_loss_start_percent,
    area_loss_end_percent,
    base_rate,
    rate_differential = NULL,
    subsidy_percent,
    net_benefit_label,
    indemnity_label = NULL
){
  data[
    , supplemental_liability_amount := (underlying_liability_amount / coverage_level_percent) *
      round(get(area_loss_start_percent) - get(area_loss_end_percent),2)]

  if(is.null(rate_differential)){
    data[, supplemental_total_premium_amount := supplemental_liability_amount*round(get(base_rate),4)]
  }else{
    data[, supplemental_total_premium_amount := supplemental_liability_amount*round(get(base_rate)*get(rate_differential),4)]
  }

  data[, supplemental_subsidy_amount := supplemental_total_premium_amount*get(subsidy_percent)]

  data[, supplemental_indemnity_amount := supplemental_liability_amount * calc_insurance_payment_rate(
    expected_yield        = expected_county_yield,
    final_yield           = final_county_yield,
    harvest_price         = harvest_price,
    price_election_amount = projected_price,
    projected_price       = projected_price,
    trigger_index         = get(area_loss_start_percent),
    coverage_range        = get(area_loss_start_percent) - get(area_loss_end_percent),
    insurance_plan_code   = insurance_plan_code
  )$payment_rate]

  data[, (net_benefit_label) := supplemental_indemnity_amount - supplemental_total_premium_amount + supplemental_subsidy_amount]
  if(!is.null(indemnity_label)) data[, (indemnity_label) := supplemental_indemnity_amount]
  data[, c("supplemental_liability_amount","supplemental_total_premium_amount",
           "supplemental_subsidy_amount","supplemental_indemnity_amount") := NULL]
  data[]
}
