
library(dplyr)
library(ggplot2)
library(zoo)
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(emmeans)

# 1. Load data

standard_wis <- read.csv(
  "https://raw.githubusercontent.com/advayvyas/ForecastEvaluation/main/Data/standard-scoring/standard_wis.csv"
)

raw <- read.csv(
  "https://raw.githubusercontent.com/advayvyas/ForecastEvaluation/main/Data/raw_data.csv"
)

model_ids <- c(
  "NAU-Copycat", "NAU-INFLAenza", "UMass-SAR",
  "UMass-alloy", "UT-GBQR", "VTSanghani-PRIME",
  "epiENGAGE-baseline", "epiENGAGE-ensemble_mean",
  "epiENGAGE-lop_norm", "epiforecasts-dyngam"
)

state_locations <- c(
  "colorado", "georgia", "indiana", "maine",
  "maryland", "massachusetts", "minnesota",
  "north-carolina", "oregon", "south-carolina",
  "texas", "utah", "virginia"
)

standard_wis <- standard_wis %>%
  mutate(wis = as.numeric(wis)) %>%
  filter(
    is.finite(wis),
    wis > 0,
    model_id %in% model_ids,
    location != "nyc"
  )


# 2. Calculate epidemic trends

oracle <- raw %>%
  group_by(location, target_end_date) %>%
  slice_max(reference_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(location, target_end_date, oracle_value) %>%
  arrange(location, target_end_date)

trend_df <- oracle %>%
  group_by(location) %>%
  mutate(
    MA3 = rollmean(
      oracle_value,
      k = 3,
      fill = NA,
      align = "right"
    ),
    pct_change = (MA3 - lag(MA3)) / lag(MA3) * 100,
    trend = case_when(
      pct_change > 5  ~ "Increasing",
      pct_change < -5 ~ "Decreasing",
      TRUE            ~ "Stable"
    ),
    trend = factor(
      trend,
      levels = c("Decreasing", "Stable", "Increasing")
    )
  ) %>%
  ungroup() %>%
  select(location, target_end_date, trend)

standard_wis <- standard_wis %>%
  left_join(
    trend_df,
    by = c("location", "target_end_date")
  )


# 3. Define granularity and factor levels

standard_wis <- standard_wis %>%
  mutate(
    granularity = if_else(
      location %in% state_locations,
      "state",
      "local"
    ),
    model_id = relevel(
      factor(model_id),
      ref = "epiENGAGE-baseline"
    ),
    horizon = factor(horizon),
    trend = relevel(
      factor(trend),
      ref = "Stable"
    ),
    granularity = relevel(
      factor(granularity),
      ref = "state"
    ),
    location = factor(location),
    reference_date = factor(reference_date),
    target_end_date = factor(target_end_date)
  ) %>%
  droplevels()

stopifnot(
  !anyNA(standard_wis[, c(
    "wis", "model_id", "horizon", "trend",
    "granularity", "location",
    "reference_date", "target_end_date"
  )])
)

print(nrow(standard_wis))
print(table(standard_wis$trend))
print(table(standard_wis$granularity))


# 4. Fit Gamma GLMMs

wis_gamma <- glmmTMB(
  wis ~ model_id * (horizon + trend + granularity) +
    (1 | location) +
    (1 | reference_date) +
    (1 | target_end_date),
  dispformula = ~ model_id + horizon + trend + granularity,
  family = Gamma(link = "log"),
  data = standard_wis,
  control = glmmTMBControl(
    parallel = 4,
    optCtrl = list(
      iter.max = 2000,
      eval.max = 3000
    )
  )
)

wis_gamma_ref <- glmmTMB(
  wis ~ model_id * (horizon + trend + granularity) +
    (1 | location) +
    (1 | reference_date),
  dispformula = ~ model_id + horizon + trend + granularity,
  family = Gamma(link = "log"),
  data = standard_wis,
  control = glmmTMBControl(
    parallel = 4,
    optCtrl = list(
      iter.max = 2000,
      eval.max = 3000
    )
  )
)

wis_gamma_target <- glmmTMB(
  wis ~ model_id * (horizon + trend + granularity) +
    (1 | location) +
    (1 | target_end_date),
  dispformula = ~ model_id + horizon + trend + granularity,
  family = Gamma(link = "log"),
  data = standard_wis,
  control = glmmTMBControl(
    parallel = 4,
    optCtrl = list(
      iter.max = 2000,
      eval.max = 3000
    )
  )
)

print(summary(wis_gamma))


# 5. Compare models

model_aic <- AIC(
  wis_gamma,
  wis_gamma_ref,
  wis_gamma_target
)

random_variance <- VarCorr(wis_gamma)

compare_coef <- bind_rows(
  tidy(wis_gamma, effects = "fixed") %>%
    mutate(model = "Both"),
  tidy(wis_gamma_ref, effects = "fixed") %>%
    mutate(model = "Reference only"),
  tidy(wis_gamma_target, effects = "fixed") %>%
    mutate(model = "Target only")
)

print(model_aic)
print(random_variance)

compare_coef %>%
  select(model, component, term, estimate, std.error) %>%
  arrange(term, model) %>%
  print(n = Inf)


# 6. Model diagnostics

set.seed(123)

res <- simulateResiduals(
  fittedModel = wis_gamma,
  n = 1000,
  plot = FALSE
)

res_ref <- simulateResiduals(
  fittedModel = wis_gamma_ref,
  n = 1000,
  plot = FALSE
)

res_target <- simulateResiduals(
  fittedModel = wis_gamma_target,
  n = 1000,
  plot = FALSE
)

diagnostics <- list(
  both = list(
    uniformity = testUniformity(res, plot = FALSE),
    dispersion = testDispersion(res, plot = FALSE),
    outliers = testOutliers(res, plot = FALSE)
  ),
  reference = list(
    uniformity = testUniformity(res_ref, plot = FALSE),
    dispersion = testDispersion(res_ref, plot = FALSE),
    outliers = testOutliers(res_ref, plot = FALSE)
  ),
  target = list(
    uniformity = testUniformity(res_target, plot = FALSE),
    dispersion = testDispersion(res_target, plot = FALSE),
    outliers = testOutliers(res_target, plot = FALSE)
  )
)

print(diagnostics)

mf <- model.frame(wis_gamma)

plotResiduals(res, mf$model_id)
plotResiduals(res, mf$trend)
plotResiduals(res, mf$horizon)
plotResiduals(res, mf$granularity)


dir.create("Figures/diagnostics", recursive = TRUE, showWarnings = FALSE)

for (name in c("both", "reference", "target")) {
  
  r <- switch(
    name,
    both = res,
    reference = res_ref,
    target = res_target
  )
  
  png(
    paste0("Figures/diagnostics/", name, "_diagnostics.png"),
    width = 2000,
    height = 1200,
    res = 200
  )
  
  plot(r)
  dev.off()
}

for (variable in c("model_id", "trend", "horizon", "granularity")) {
  
  png(
    paste0("Figures/diagnostics/residual_", variable, ".png"),
    width = 1600,
    height = 1200,
    res = 200
  )
  
  plotResiduals(res, mf[[variable]])
  dev.off()
}

# 7. Extract adjusted expected WIS

emm <- emmeans(
  wis_gamma,
  ~ model_id * horizon * trend * granularity,
  component = "cond"
)

emm <- regrid(
  emm,
  transform = "response"
)

get_adjusted_wis <- function(variable) {
  
  result <- emmeans(
    emm,
    specs = as.formula(paste("~", variable)),
    weights = "equal"
  )
  
  as.data.frame(confint(result)) %>%
    rename(
      adjusted_wis = response,
      lower_wis = asymp.LCL,
      upper_wis = asymp.UCL
    )
}

wis_horizon <- get_adjusted_wis("horizon")
wis_trend <- get_adjusted_wis("trend")
wis_model <- get_adjusted_wis("model_id")
wis_granularity <- get_adjusted_wis("granularity")


# 8. Extract random effects on log scale

re <- ranef(
  wis_gamma,
  condVar = TRUE
)$cond

get_random_effects <- function(re, variable) {
  
  est <- re[[variable]]
  se <- sqrt(attr(est, "condVar")[1, 1, ])
  
  tibble(
    level = rownames(est),
    estimate = est[, 1],
    SE = se,
    lower = estimate - 1.96 * SE,
    upper = estimate + 1.96 * SE
  )
}

re_location <- get_random_effects(re, "location")
re_reference <- get_random_effects(re, "reference_date")
re_target <- get_random_effects(re, "target_end_date")


# 9. Save results

dir.create("Results", showWarnings = FALSE)

save(
  standard_wis,
  trend_df,
  wis_gamma,
  wis_gamma_ref,
  wis_gamma_target,
  model_aic,
  random_variance,
  compare_coef,
  res,
  res_ref,
  res_target,
  diagnostics,
  emm,
  wis_horizon,
  wis_trend,
  wis_model,
  wis_granularity,
  re_location,
  re_reference,
  re_target,
  file = "Results/figure4_results.RData"
)
