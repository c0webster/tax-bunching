tax_data <- fread("all_tax_amounts.csv")
library(data.table)
library(tidyverse)
library(ggthemes)
library(viridis)


opt_bunch_mean <- tax_data %>%
  group_by(salary, donation_frac) %>%
  summarize(optimal_bunch = round(mean(bunch[optimal == 1]))) %>%
  setDT()



tax_data %>%
  group_by(salary, donation_frac) %>%
  summarize(optimal_bunch = round(mean(bunch[optimal == 1]))) %>%
  ggplot(aes(x = salary, y = donation_frac, fill = factor(optimal_bunch))) +
  geom_tile() +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Optimal Bunch Years",
       x = "Salary",
       y = "Donation Fraction",
       fill = "Optimal Bunch Years") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(
    labels = scales::dollar_format(scale = 1/1000, suffix = "k"),
    breaks = seq(20000, 300000, 40000),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.5, 0.1),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(20000, 300000), ylim = c(0, 0.5))


tax_data
