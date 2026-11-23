library(dplyr)
library(ggplot2)
library(patchwork)

load("Results/figure4_results.RData")

trend_order <- c("Increasing", "Stable", "Decreasing")

model_order <- c(
  "epiENGAGE-baseline", "epiENGAGE-lop_norm", "epiENGAGE-ensemble_mean", "UT-GBQR",
  "epiforecasts-dyngam", "NAU-Copycat", "UMass-alloy", "NAU-INFLAenza",
  "UMass-SAR", "VTSanghani-PRIME"
)

wis_trend <- wis_trend %>%
  mutate(trend = factor(trend, levels = trend_order))

wis_model <- wis_model %>%
  mutate(model_id = factor(model_id, levels = model_order))



# Common x-axis limit
x_max <- max(
  wis_horizon$upper_wis,
  wis_trend$upper_wis,
  wis_model$upper_wis,
  wis_granularity$upper_wis,
  na.rm = TRUE
)

plot_adjusted_wis <- function(data, variable, y_label) {
  
  ggplot(
    data,
    aes(x = adjusted_wis, y = .data[[variable]])
  ) +
    geom_errorbar(
      aes(xmin = lower_wis, xmax = upper_wis),
      width = 0.2,
      orientation = "y"
    ) +
    geom_point(size = 5, color = "steelblue") +
    labs(
      x = "Adjusted expected WIS",
      y = y_label
    ) +
    theme_bw() +
    coord_cartesian(xlim = c(0, x_max)) +
    scale_y_discrete(limits = rev) +
    theme(
      text         = element_text(size = 27),
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      axis.title   = element_text(size = 27),
      legend.text  = element_text(size = 24),
      legend.title = element_text(size = 25.5),
      strip.text   = element_text(size = 25.5)
    )
}

p_horizon <- plot_adjusted_wis(
  wis_horizon, "horizon", "Forecast horizon"
)
p_horizon
p_trend <- plot_adjusted_wis(
  wis_trend, "trend", "Epidemic trend"
)
p_trend
p_model <- plot_adjusted_wis(
  wis_model, "model_id", "Forecasting model"
) 
p_model
p_granularity <- plot_adjusted_wis(
  wis_granularity, "granularity", "Geographic granularity"
)
p_granularity

library(patchwork)

fig4_fixed <- (
  (p_horizon + p_trend) /
    (p_model + p_granularity)
) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(size = 40, face = "bold")
  )

ggsave(
  "Figures/figure4_fixed.png",
  plot = fig4_fixed,
  width = 20,
  height = 20,
  units = "in",
  dpi = 100
)


## r.e. figures

lookup <- read.csv("https://raw.githubusercontent.com/advayvyas/ForecastEvaluation/main/Data/lookup.csv")

location_ci <- re_location %>%
  rename(location = level) %>%
  left_join(lookup, by = "location") %>%
  mutate(
    state_level = ifelse(state == "state", "state", "not-state"),
    state = ifelse(state == "state", location, state)
  ) %>%
  arrange(state, desc(estimate)) %>%
  mutate(
    location = factor(location, levels = rev(unique(location)))
  )

# State boundaries
state_counts <- location_ci %>%
  count(state)

# State boundaries based on plotted location order
state_boundaries <- location_ci %>%
  distinct(location, state) %>%
  mutate(location_order = as.numeric(location)) %>%
  arrange(location_order) %>%
  mutate(
    boundary = state != lead(state)
  ) %>%
  filter(!is.na(boundary) & boundary) %>%
  pull(location_order) %>%
  `+`(0.5)


fig4_location <- ggplot(
  location_ci,
  aes(x = location, y = estimate, color = state)
) +
  geom_vline(
    xintercept = state_boundaries,
    color = "black",
    linewidth = 1
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2
  ) +
  geom_point(size = 5) +
  geom_point(
    data = subset(location_ci, state_level == "state"),
    size = 5,
    shape = 21,
    fill = NA,
    color = "black",
    stroke = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    x = "Location",
    y = "Estimated random effect"
  ) +
  theme_bw() +
  coord_flip() +
  theme(
    text         = element_text(size = 20),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 9, lineheight = 0.8),
    axis.ticks.y = element_blank(),
    axis.title   = element_text(size = 20),
    legend.text  = element_text(size = 20),
    legend.title = element_text(size = 21),
    strip.text   = element_text(size = 20)
  ) +
  scale_y_discrete(drop = FALSE) +
  scale_x_discrete(drop = FALSE)
fig4_location



plot_random_date <- function(data, y_label) {
  
  data <- data %>%
    mutate(date = as.Date(level)) %>%
    arrange(date)
  
  month_breaks <- seq(
    as.Date(format(min(data$date), "%Y-%m-01")),
    max(data$date),
    by = "1 month"
  )
  
  ggplot(
    data,
    aes(x = estimate, y = as.numeric(date))
  ) +
    geom_errorbar(
      aes(xmin = lower, xmax = upper),
      width = 0,
      orientation = "y"
    ) +
    geom_point(size = 5, color = "steelblue") +
    scale_y_reverse(
      breaks = as.numeric(month_breaks),
      labels = format(month_breaks, "%b")
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      x = "Estimated random effect",
      y = y_label
    ) +
    theme_bw() +
    theme(
      text         = element_text(size = 20),
      axis.text.x  = element_text(size = 15),
      axis.text.y  = element_text(size = 15),
      axis.title   = element_text(size = 20),
      legend.text  = element_text(size = 20),
      legend.title = element_text(size = 21),
      strip.text   = element_text(size = 20)
    )
}
fig4_reference <- plot_random_date(
  re_reference,
  "Reference date"
)

fig4_target <- plot_random_date(
  re_target,
  "Target end date"
)

fig4_reference
fig4_target



fig4_random <- (
  (fig4_location + fig4_reference + fig4_target) 
) +
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(size = 40, face = "bold")
  )

ggsave(
  "Figures/figure4_random.png",
  plot = fig4_random,
  width = 20,
  height = 10,
  units = "in",
  dpi = 100
)

