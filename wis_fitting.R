# ============================================================
# wis_fitting.R
# 1. Load data   2. Define scenarios (data file + formula)   3. Run what you want   4. Compare
# Run sections 1 and 2 first, then run only the scenario lines you need in section 3.
# Written without running R; if something errors, send the message.
# ============================================================

source("/Users/dk29776/Dropbox/UTAustin/ForecastEvaluation/wis_functions.R")

# ------------------------------------------------------------
# 1. Load data
#   standard_wis : the rows used in every model (models, locations, horizons, reference dates)
#   raw          : forecasts and observations, only used to compute the epidemic trend
# The score each scenario models is read from its own `score_file` (section 2).
# ------------------------------------------------------------
standard_wis <- read.csv(file.path(data_dir, "standard-scoring/standard_wis.csv")) %>%
  mutate(wis = as.numeric(wis), horizon = as.integer(as.character(horizon))) %>%
  filter(is.finite(wis), model_id %in% model_ids, location != "nyc")

raw <- read.csv(file.path(data_dir, "raw_data.csv"))

base <- list(standard_wis = standard_wis, trend_df = make_trend_df(raw))

# ------------------------------------------------------------
# 2. Scenarios: data and model are written out so you can see exactly what each one fits.
#   score_file   : file (path or URL) with the score to model
#   y_transform  : applied to the score before fitting (identity, log, ...)
#   keep_zeros   : TRUE keeps rows with score exactly 0 (needs a family that allows zeros: tweedie).
#                  Default FALSE drops them (Gamma and log(WIS) can't use zeros)
#   formula / dispformula / family : the model. Gamma(link = "log") needs y > 0;
#                  tweedie(link = "log") allows y = 0; gaussian() is for y = log(WIS)
#   score_label / mean_type : wording of the figures
# Add a scenario by copying one block (new name, new data file or new formula).
# ------------------------------------------------------------
scenarios <- list(

  # Gamma(log) on the original WIS
  wis_gamma = list(
    score_file  = file.path(data_dir, "standard-scoring/standard_wis.csv"),
    y_transform = identity,
    formula     = wis ~ model_id * (horizon + trend + granularity) +
                    (1 | location) + (1 | target_end_date),
    dispformula = ~ model_id + horizon + trend + granularity,
    family      = Gamma(link = "log"),
    score_label = "WIS",
    mean_type   = "mean"
  ),

  # Gamma(log) on WIS computed after log1p of forecasts and observations
  log1p_gamma = list(
    score_file  = "https://raw.githubusercontent.com/advayvyas/ForecastEvaluation/main/Data/standard-scoring/log_standard_wis.csv",
    y_transform = identity,
    formula     = wis ~ model_id * (horizon + trend + granularity) +
                    (1 | location) + (1 | target_end_date),
    dispformula = ~ model_id + horizon + trend + granularity,
    family      = Gamma(link = "log"),
    score_label = "log1p-WIS",
    mean_type   = "mean"
  ),

  # Gamma(log) on WIS computed after sqrt of forecasts and observations
  sqrt_gamma = list(
    score_file  = file.path(data_dir, "standard-scoring/sqrt_standard_wis.csv"),
    y_transform = identity,
    formula     = wis ~ model_id * (horizon + trend + granularity) +
                    (1 | location) + (1 | target_end_date),
    dispformula = ~ model_id + horizon + trend + granularity,
    family      = Gamma(link = "log"),
    score_label = "sqrt-WIS",
    mean_type   = "mean"
  ),

  # Tweedie(log) on the original WIS, zeros kept (keep_zeros = TRUE). The power p is estimated:
  # p near 2 behaves like a Gamma, p between 1 and 2 allows exact zeros.
  wis_tweedie = list(
    score_file  = file.path(data_dir, "standard-scoring/standard_wis.csv"),
    y_transform = identity,
    keep_zeros  = TRUE,
    formula     = wis ~ model_id * (horizon + trend + granularity) +
                    (1 | location) + (1 | target_end_date),
    dispformula = ~ model_id + horizon + trend + granularity,
    family      = tweedie(link = "log"),
    score_label = "WIS",
    mean_type   = "mean"
  ),

  # Gaussian on y = log(WIS). Not a proper score: use effects, not ranks, as the main result.
  logwis_gaussian = list(
    score_file  = file.path(data_dir, "standard-scoring/standard_wis.csv"),
    y_transform = log,
    formula     = wis ~ model_id * (horizon + trend + granularity) +
                    (1 | location) + (1 | target_end_date),
    dispformula = ~ model_id + horizon + trend + granularity,
    family      = gaussian(link = "identity"),
    score_label = "WIS",
    mean_type   = "geometric mean"
  )
)

# ------------------------------------------------------------
# 3. Run. Pick the lines you want (Cmd+Enter on a line). Each run fits "all" and "drop4"
#    (first 4 weekly submissions dropped), prints the results and saves figure4_*_<name>[_drop4].png
#
#    variants = c(all = 0L)                 only the full data
#    variants = c(drop4 = 4L)               only the first 4 weeks dropped
#    diagnostics = FALSE                    skip DHARMa (faster)
#    show = TRUE                            also print the three figures
# ------------------------------------------------------------
results <- list()

results$wis_gamma       <- run_scenario("wis_gamma")
results$log1p_gamma     <- run_scenario("log1p_gamma")
results$sqrt_gamma      <- run_scenario("sqrt_gamma")
results$wis_tweedie     <- run_scenario("wis_tweedie")
results$logwis_gaussian <- run_scenario("logwis_gaussian")

# one variant only, no diagnostics, e.g.:
# results$sqrt_gamma <- run_scenario("sqrt_gamma", variants = c(all = 0L), diagnostics = FALSE)

# ------------------------------------------------------------
# 4. Compare overall ranks of everything that has been run
# ------------------------------------------------------------
compare_ranks(results)

# Look at one result:  results$wis_gamma$all$figs$adjusted,  results$sqrt_gamma$drop4$ranking$overall, ...
