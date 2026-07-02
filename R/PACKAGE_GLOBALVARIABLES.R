#' @name PACKAGE_GLOBALVARIABLES
#' @title Global variable names used in non-standard evaluation
#' @description Column names created at runtime by data.table's `:=`, registered
#'   in `.onLoad()` so R CMD check does not flag "no visible binding".
#' @format A character vector of names.
#' @keywords internal
PACKAGE_GLOBALVARIABLES <- Filter(nzchar, strsplit(
  "
  . mean_base mean_sn sd_base sd_sn var_base var_sn cv_base cv_sn
  lapv_base lapv_sn lrpv_base lrpv_sn nlapv_base nlapv_sn nlrpv_base nlrpv_sn
  ploss_base ploss_sn res lres2 lossI
  mean_index sd_index var_index cv_index
  downside_index shortfall_index downside_index_n shortfall_index_n
  risk_index risk_index_downside risk_index_shortfall
  mean_gain_flag risk_reduction_flag risk_reduction_flag_downside
  risk_reduction_flag_shortfall reduces_risk
  mean_gain_pct risk_reduction_pct risk_reduction_pct_downside
  risk_reduction_pct_shortfall
  efficiency efficiency_downside efficiency_shortfall
  regime metric value delta cv lapv lrpv nlapv nlrpv ploss var x
  n_producers med_mean_index med_risk_index med_efficiency pct_reduces_risk
  underlying_indemnity_amount underlying_liability_amount underlying_total_premium_amount
  approved_yield yield_farm harvest_price projected_price coverage_level_percent
  insurance_plan_code expected_county_yield final_county_yield
  supplemental_liability_amount supplemental_total_premium_amount
  supplemental_subsidy_amount supplemental_indemnity_amount
  scenario component scale
  ",
  "\\s+"
)[[1]])
