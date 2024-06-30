tax_data <- fread("all_tax_amounts.csv")
library(data.table)
library(tidyverse)
library(ggthemes)
library(viridis)


opt_bunch_mean <- tax_data %>%
  group_by(salary, donation_frac) %>%
  summarize(optimal_bunch = round(mean(bunch[optimal == 1]))) %>%
  setDT()

fwrite(opt_bunch_mean, "optimal_bunch_mean.csv")

library(ggplot2)
library(dplyr)
library(viridis)

p <- tax_data %>%
  group_by(salary, donation_frac) %>%
  summarize(optimal_bunch = round(mean(bunch[optimal == 1]))) %>%
  ggplot(aes(x = salary, y = donation_frac, fill = factor(optimal_bunch))) +
  geom_tile() +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Optimal Bunch Years by Salary and Donation Fraction",
       x = "Salary ($)",
       y = "Donation Fraction",
       fill = "Optimal Bunch Years") +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 20, face = "bold")
  ) +
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

# Assuming your last plot is stored in a variable called 'p'
# If not, replace 'p' with your ggplot code

ggsave("optimal_bunch_years_chart.png", plot = p, width = 8, height = 6, units = "in", dpi = 120)
