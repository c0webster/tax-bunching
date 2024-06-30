# comparing bunching vs not bunching for one level of income and donation amount


# CHANGE THESE for comparison

annual_income <- 100000
donation_frac <- 0.10
standard_deduction <- 13850
bunch_years <- 6


# set up tax stuff
brackets <- c(11000, 44725, 95375, 182100, 231250, 578125, Inf)
rates <- c(0.10, 0.12, 0.22, 0.24, .32, .35, .37)

calculate_tax <- function(income) {
  tax <- 0
  for (i in seq_along(brackets)) {
    if (income > brackets[i]) {
      tax <- tax + (brackets[i] - ifelse(i == 1, 0, brackets[i-1])) * rates[i]
    } else {
      tax <- tax + (income - ifelse(i == 1, 0, brackets[i-1])) * rates[i]
      break
    }
  }
  return(tax)
}


# function for calculating total tax paid when bunching
calc_lifetime_tax_by_bunch <- function(bunch, salary, donation_frac, lifetime_years) {
  standard_deduction <- 13850
  total_taxes_paid <- 0
  # total_years_passed <- 0
  money_saved <- 0
  for (i in 1:lifetime_years) {
    if ((i %% bunch == 0) | (i == lifetime_years)) {
      # print(i)
      # donating year
      money_saved <- money_saved + donation_frac * salary
      total_taxes_paid <- total_taxes_paid + calculate_tax(salary - max(money_saved, standard_deduction))
      money_saved <- 0

    } else {
      # saving year
      money_saved <- money_saved + donation_frac * salary
      total_taxes_paid <- total_taxes_paid + calculate_tax(salary - standard_deduction)
    }
  }
  return(total_taxes_paid)
}

# 1. Tax with annual donations (not over the standard deduction)
tax_no_bunch <- calculate_tax(annual_income - standard_deduction)

#2. Tax with bunching at optimal levels (drawn from table), divide by number of years
tax_bunch <- calc_lifetime_tax_by_bunch(bunch_years, annual_income, donation_frac,
                                        40) / 40
# these are the per-year values
tax_no_bunch
tax_bunch
