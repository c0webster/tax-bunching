library(data.table)
library(tidyverse)
library(ggthemes)
# Define tax brackets and rates for single filers
brackets <- c(11000, 44725, 95375, 182100, 231250, 578125, Inf)
rates <- c(0.10, 0.12, 0.22, 0.24, .32, .35, .37)

# Function to calculate tax based on taxable income
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
  if (tax < 0) {
    tax <- 0
  }
  return(tax)
}


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

full_options <- CJ(bunch = 1:10, salary = seq(.2e5, 3e5, 5e3), donation_frac = seq(0.05, .5, .05),
                  lifetime_years = 40)

full_results <- pmap_dbl(full_options, calc_lifetime_tax_by_bunch)


result_dt <- copy(full_options)
result_dt[, taxes_paid := full_results]
result_dt[, min_taxes := min(taxes_paid), .(salary, donation_frac)]
result_dt[, optimal := as.numeric(min_taxes == taxes_paid)]


reg_result <- lm(taxes_paid ~ salary + donation_frac + factor(bunch), data = result_dt )
summary(reg_result)
# target_donation <- .1

fwrite(result_dt, "all_tax_amounts.csv")
