# Rates and percentages
#
# Turns counts into a rate per 1,000 (patient-list denominators) or a
# percentage (register or estimate denominators). Which measure can be divided
# by which denominator is set in config/measure_denominators.csv; how each
# denominator is worded and scaled is in config/denominators.csv.
# No Shiny code here, so it can be unit-tested.


# The "Show as" choices for the selected measures: "Count", plus every
# denominator allowed for at least one of them
scale_choices <- function(measures, pairs, denominators) {
  allowed_codes <- pairs |> filter(measure %in% measures) |> pull(denominator)
  allowed <- denominators |> filter(denominator %in% allowed_codes)
  c("Count" = "count", setNames(allowed$denominator, allowed$unit_label))
}

# Divide each measure that allows it by the denominator for the same area and
# period. Other measures keep their counts (scaled = FALSE).
# Adds: numerator (the original count), denominator_value, scaled.
apply_scaling <- function(d, denom_rows, denom_code, pairs, denominators) {
  d <- mutate(d, numerator = value, denominator_value = NA_real_, scaled = FALSE)
  if (is.null(denom_code) || denom_code == "count") return(d)

  allowed <- pairs |> filter(denominator == denom_code) |> pull(measure)
  multiplier <- denominators |> filter(denominator == denom_code) |> pull(multiplier) |> as.numeric()

  # Denominators are whole-population totals, never a breakdown category
  denominator_values <- denom_rows |>
    filter(submeasure == "") |>
    select(org_code, period_end, matched_denominator = value)

  d |>
    select(-denominator_value) |>
    left_join(denominator_values, by = c("org_code", "period_end")) |>
    mutate(
      scaled = measure %in% allowed,
      denominator_value = if_else(scaled, matched_denominator, NA_real_),
      value = if_else(scaled, numerator / denominator_value * multiplier, value)
    ) |>
    select(-matched_denominator)
}

# Axis and tooltip unit for each row after scaling: "%" for percentage
# measures, "Count" if not scaled, otherwise the rate's wording (e.g. "per
# 1,000 registered patients")
unit_label <- function(measure_unit, scaled, denom_code, denominators) {
  rate_wording <- denominators$unit_label[match(denom_code, denominators$denominator)]
  case_when(
    measure_unit == "percent" ~ "%",
    !scaled ~ "Count",
    TRUE ~ rate_wording
  )
}

# Numbers as shown in tooltips: counts with thousands separators, percentages
# to 1 decimal place, rates to 2. Missing values are suppressed ones.
format_value <- function(x, unit) {
  shown <- if (unit == "Count") {
    formatC(x, format = "f", digits = 0, big.mark = ",")
  } else if (startsWith(unit, "%")) {
    paste0(formatC(x, format = "f", digits = 1), "%")
  } else {
    formatC(x, format = "f", digits = 2, big.mark = ",")
  }
  ifelse(is.na(x), "suppressed", shown)
}
