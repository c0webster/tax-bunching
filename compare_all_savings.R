# comparing bunching vs not bunching for all income

library(tidyverse)
library(data.table)

# set up tax stuff
brackets <- c(11000, 44725, 95375, 182100, 231250, 578125, Inf)
rates <- c(0.10, 0.12, 0.22, 0.24, .32, .35, .37)
standard_deduction <- 13850
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



# read in optimal data from previous file
all_tax_amounts <- fread("all_tax_amounts.csv")

opt_bunch_mean <- all_tax_amounts %>%
  filter(donation_frac != 0) %>%
  group_by(salary, donation_frac) %>%
  summarize(bunch = round(mean(bunch[optimal == 1]))) %>%
  setDT()

all_with_optimal <- merge(all_tax_amounts, opt_bunch_mean, by = c("salary", "donation_frac", "bunch"))
all_with_optimal[salary * donation_frac > standard_deduction,
                 taxable_income := (1-donation_frac) * salary]
all_with_optimal[standard_deduction >= salary * donation_frac,
                 taxable_income := salary - standard_deduction]
all_with_optimal[taxable_income < 0, taxable_income := 0]


all_with_optimal[, taxes_no_bunching := sapply(taxable_income, calculate_tax)]
all_with_optimal[, min_taxes := NULL]
all_with_optimal[, taxes_paid_year := taxes_paid / 40]
all_with_optimal[, money_saved_year := taxes_no_bunching - taxes_paid_year]
all_with_optimal[taxes_paid_year < 0]

fwrite(all_with_optimal, "yearly_saved_comparison.csv")
