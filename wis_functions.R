# ============================================================
# wis_functions.R
# Shared settings, data builders, model fitting, ranking and figure functions.
# Do not run on its own: wis_fitting.R sources this file.
# Written without running R; if something errors, send the message.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(zoo)
  library(ggplot2); library(patchwork)
  library(glmmTMB); library(DHARMa); library(emmeans)
})
emm_options(lmer.df = "asymptotic")

# ------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------
project_dir <- "/Users/dk29776/Dropbox/UTAustin/ForecastEvaluation"
data_dir    <- file.path(project_dir, "Data")
fig_dir     <- file.path(project_dir, "Figures")

baseline_model <- "epiENGAGE-baseline"

model_ids <- c("NAU-Copycat", "NAU-INFLAenza", "UMass-SAR", "UMass-alloy", "UT-GBQR",
               "VTSanghani-PRIME", "epiENGAGE-baseline", "epiENGAGE-ensemble_mean",
               "epiENGAGE-lop_norm", "epiforecasts-dyngam")

state_locations <- c("colorado", "georgia", "indiana", "maine", "maryland", "massachusetts",
                     "minnesota", "north-carolina", "oregon", "south-carolina", "texas",
                     "utah", "virginia")

# Fixed model order used in every figure (top to bottom)
model_order <- c("epiENGAGE-baseline", "epiENGAGE-lop_norm", "epiENGAGE-ensemble_mean",
                 "UT-GBQR", "epiforecasts-dyngam", "NAU-Copycat", "UMass-alloy",
                 "NAU-INFLAenza", "UMass-SAR", "VTSanghani-PRIME")

# Reference level of each variable for the within-model / interaction contrasts
reference <- c(horizon = "1", trend = "Stable", granularity = "state")

ctrl <- glmmTMBControl(parallel = 4,
                       optCtrl = list(iter.max = 2000, eval.max = 2000))

join_keys <- c("location", "target_end_date", "horizon", "model_id", "reference_date")

# ------------------------------------------------------------
# 2. Data helpers (the data itself is loaded in wis_fitting.R)
# ------------------------------------------------------------
# Epidemic trend (3-week rolling mean, +-5% threshold; same as fig4_DK.Rmd) from raw_data.csv
make_trend_df <- function(raw) {
  raw %>%
    group_by(location, target_end_date) %>%
    slice_max(reference_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(location, target_end_date, oracle_value) %>%
    arrange(location, target_end_date) %>%
    group_by(location) %>%
    mutate(
      MA3 = rollmean(oracle_value, k = 3, fill = NA, align = "right"),
      pct_change = (MA3 - lag(MA3)) / lag(MA3) * 100,
      trend = case_when(pct_change > 5 ~ "Increasing", pct_change < -5 ~ "Decreasing", TRUE ~ "Stable"),
      trend = factor(trend, levels = c("Decreasing", "Stable", "Increasing"))
    ) %>%
    ungroup() %>%
    select(location, target_end_date, trend)
}

# One score table keyed by join_keys, with the score in `wis_t`. `score_file` is a path or URL.
read_score <- function(score_file) {
  d <- read.csv(score_file) %>%
    mutate(horizon = as.integer(as.character(horizon)), wis_t = as.numeric(wis))
  if (min(d$horizon, na.rm = TRUE) == 0) d <- d %>% mutate(horizon = horizon + 1L)   # file has 0-3; standard_wis has 1-4
  d %>% select(all_of(join_keys), wis_t)
}

# Model data for one score file: y = y_transform(score), with trend and granularity added.
# `base` = list(standard_wis, trend_df), loaded in wis_fitting.R. Rows come from standard_wis
# (models, locations); the score file only supplies the score.
build_model_data <- function(base, score_file, y_transform = identity, keep_zeros = FALSE) {
  scored <- read_score(score_file)
  stopifnot(!anyDuplicated(base$standard_wis[join_keys]), !anyDuplicated(scored[join_keys]))

  # Sanity check: the same location/date/horizon must give DIFFERENT scores for different models
  chk <- scored %>%
    filter(model_id %in% c("epiENGAGE-baseline", "UT-GBQR")) %>%
    pivot_wider(names_from = model_id, values_from = wis_t) %>%
    summarise(cor = cor(`epiENGAGE-baseline`, `UT-GBQR`, use = "complete.obs"))
  if (!is.finite(chk$cor) || chk$cor > 0.999)
    stop("Score file '", score_file, "' is identical across models (cor = ", chk$cor, "): the file is broken")

  base$standard_wis %>%
    select(-wis, -any_of(c("trend", "granularity"))) %>%
    inner_join(scored, by = join_keys, relationship = "one-to-one") %>%
    filter(is.finite(wis_t), if (keep_zeros) wis_t >= 0 else wis_t > 0) %>%   # Gamma / log(WIS) can't take zeros; Tweedie can
    mutate(wis = y_transform(wis_t)) %>%
    select(-wis_t) %>%
    left_join(base$trend_df, by = c("location", "target_end_date"), relationship = "many-to-one") %>%
    filter(!is.na(trend)) %>%                              # safety only: the first weeks get "Stable" (NA falls into case_when's TRUE branch), as in fig4_DK.Rmd
    mutate(
      granularity = factor(if_else(location %in% state_locations, "state", "local"), levels = c("local", "state")),
      horizon     = factor(horizon),
      trend       = relevel(factor(trend), ref = "Stable"),
      model_id    = relevel(factor(model_id), ref = baseline_model),
      location    = factor(location),
      target_end_date = factor(target_end_date)
    )
}

# Drop the first n weekly submissions (same idea as fig4-sens.Rmd)
drop_first_weeks <- function(data, n_weeks = 4) {
  if (n_weeks == 0) return(droplevels(data))
  first_dates <- sort(unique(as.Date(data$reference_date)))[seq_len(n_weeks)]
  data %>% filter(!as.Date(reference_date) %in% first_dates) %>% droplevels()
}

# ------------------------------------------------------------
# 3. Fitting and diagnostics
# ------------------------------------------------------------
# Fit one scenario. The formulas, dispersion model and family come from `spec`
# (defined in wis_fitting.R), so the model is visible where it is chosen.
fit_scenario <- function(data, spec) {
  data <- droplevels(data)
  f <- spec$formula
  environment(f) <- environment()                      # so emmeans can find `data`
  d <- if (is.null(spec$dispformula)) ~ 1 else spec$dispformula
  environment(d) <- environment()

  fit <- glmmTMB(f, dispformula = d, family = spec$family, data = data, control = ctrl)
  attr(fit, "fit_data") <- data
  fit
}

check_fit <- function(fit) {
  out <- list(convergence = fit$fit$convergence, message = fit$fit$message,
              pdHess = fit$sdr$pdHess, logLik = logLik(fit), AIC = AIC(fit))
  # Tweedie: estimated power p (p = 1: Poisson-like, p = 2: Gamma)
  fp <- tryCatch(family_params(fit), error = function(e) NULL)
  if (!is.null(fp)) out$family_params <- fp
  out
}

# DHARMa residual checks. Draws the plots on screen, prints the tests, and returns everything
# needed to redraw the same pages into a PDF (see save_diagnostics_pdf).
run_diagnostics <- function(fit, data, title = "") {
  cat("\n---- DHARMa:", title, "----\n")
  set.seed(123)
  sim <- simulateResiduals(fit, n = 1000)

  tests <- list(testDispersion(sim, plot = FALSE), testUniformity(sim, plot = FALSE), testOutliers(sim, plot = FALSE))
  walk(tests, print)

  disp_fig <- tryCatch(plot_dispersion(fit, title),
                       error = function(e) { message("dispersion figure skipped: ", conditionMessage(e)); NULL })

  diag <- list(title = title, sim = sim, data = data, disp_fig = disp_fig,
               text = c(title, "", unlist(map(tests, ~ c(capture.output(print(.x)), "")))))
  draw_diagnostics(diag)
  invisible(diag)
}

# Dispersion by model_id / horizon / trend / granularity.
#  top row    : the dispersion parameter estimated by glmmTMB (exp of the dispersion linear predictor,
#               95% CI from emmeans). Gaussian fit: sigma on the log(WIS) scale.
#  bottom row : what the data show, using the fitted means: mean of ((y - mu) / mu)^2 for Gamma
#               (squared coefficient of variation), mean of (y - mu)^2 for Gaussian.
#               Higher = more variable in this row. If the top row runs the OPPOSITE way, glmmTMB's
#               parameter is on the inverse (shape-like) scale for that family; read the bottom row.
plot_dispersion <- function(fit, title = "") {
  edata <- attr(fit, "fit_data")
  gauss <- is_gaussian(fit)
  vars  <- c("model_id", "horizon", "trend", "granularity")
  mu    <- predict(fit, type = "response")
  e     <- if (gauss) (edata$wis - mu)^2 else ((edata$wis - mu) / mu)^2
  src_model <- "glmmTMB dispersion parameter (95% CI)"
  src_data  <- if (gauss) "Observed residual variance" else "Observed CV^2 of y / fitted mean"

  one <- function(v) {
    lv <- levels(droplevels(edata[[v]]))
    emp <- data.frame(variable = v, level = lv, source = src_data,
                      est = as.numeric(tapply(e, edata[[v]], mean, na.rm = TRUE)[lv]), lo = NA_real_, hi = NA_real_)
    em <- as.data.frame(emmeans(fit, as.formula(paste("~", v)), component = "disp",
                                type = "response", data = edata))
    est <- intersect(c("response", "emmean"), names(em))[1]
    lo  <- grep("^(asymp.LCL|lower.CL)$", names(em), value = TRUE)
    hi  <- grep("^(asymp.UCL|upper.CL)$", names(em), value = TRUE)
    mod <- data.frame(variable = v, level = as.character(em[[v]]), source = src_model,
                      est = em[[est]], lo = em[[lo]], hi = em[[hi]])
    bind_rows(mod, emp)
  }
  dd <- map_dfr(vars, one) %>%
    mutate(variable = factor(variable, levels = vars),
           source = factor(source, levels = c(src_model, src_data)),
           level = factor(level, levels = unique(level)))

  ggplot(dd, aes(x = level, y = est)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, na.rm = TRUE) +
    geom_point(size = 2, color = "steelblue") +
    facet_grid(source ~ variable, scales = "free", space = "free_x") +
    labs(x = NULL, y = NULL, title = paste("Dispersion by predictor:", title)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
          strip.text = element_text(size = 8))
}

# One diagnostics "page set": test results as text, DHARMa overview, residuals by predictor
draw_diagnostics <- function(diag, text_page = FALSE) {
  if (text_page) {
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
    plot.new()
    text(0, 1, paste(diag$text, collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.55)
  }
  if (!is.null(diag$disp_fig)) print(diag$disp_fig)
  plot(diag$sim, main = diag$title)
  par(mfrow = c(2, 2))
  plotResiduals(diag$sim, form = diag$data$model_id,    main = paste(diag$title, "- model"))
  plotResiduals(diag$sim, form = diag$data$horizon,     main = paste(diag$title, "- horizon"))
  plotResiduals(diag$sim, form = diag$data$trend,       main = paste(diag$title, "- trend"))
  plotResiduals(diag$sim, form = diag$data$granularity, main = paste(diag$title, "- granularity"))
  par(mfrow = c(1, 1))
}

# All diagnostics of a list (e.g. "all" and "drop4" of one scenario) into ONE multi-page PDF
save_diagnostics_pdf <- function(diags, file) {
  pdf(file, width = 11, height = 8.5)
  on.exit(dev.off(), add = TRUE)
  walk(diags, draw_diagnostics, text_page = TRUE)
  cat("saved:", file, "\n")
}

is_gaussian <- function(fit) identical(family(fit)$family, "gaussian")

# ------------------------------------------------------------
# 4. Estimated marginal means and contrasts
# ------------------------------------------------------------
get_emm <- function(fit, spec_string) {
  emmeans(fit, as.formula(spec_string), weights = "proportional", data = attr(fit, "fit_data"))
}

# Adjusted mean WIS by model and grouping variable (exp of the link-scale emmean; random effects at 0).
# Gamma(log): arithmetic mean.  Gaussian on log(WIS): geometric mean.
get_adjusted_wis <- function(fit, variable) {
  result <- as.data.frame(confint(get_emm(fit, paste("~ model_id |", variable))))
  lo <- grep("^(asymp.LCL|lower.CL)$", names(result), value = TRUE)
  hi <- grep("^(asymp.UCL|upper.CL)$", names(result), value = TRUE)
  result %>% mutate(adjusted_wis = exp(emmean), lower_wis = exp(.data[[lo]]), upper_wis = exp(.data[[hi]]))
}

get_within_contrast <- function(fit, var) {
  emm <- get_emm(fit, paste("~", var, "| model_id"))
  lv <- unique(as.character(as.data.frame(emm)[[var]]))
  ref_idx <- match(reference[[var]], lv)
  if (is.na(ref_idx)) stop("Reference level not found: ", reference[[var]], ". Levels: ", paste(lv, collapse = ", "))
  res <- contrast(emm, method = "trt.vs.ctrl", ref = ref_idx, adjust = "none") |>
    confint(adjust = "none") |> as.data.frame()
  lo <- grep("^(asymp.LCL|lower.CL)$", names(res), value = TRUE)
  hi <- grep("^(asymp.UCL|upper.CL)$", names(res), value = TRUE)
  res %>% mutate(term = gsub(" - ", " vs ", contrast, fixed = TRUE),
                 pct = 100 * (exp(estimate) - 1),
                 lo = 100 * (exp(.data[[lo]]) - 1), hi = 100 * (exp(.data[[hi]]) - 1))
}

get_interaction_contrast <- function(fit, var) {
  emm <- get_emm(fit, paste("~", var, "| model_id"))
  lv <- unique(as.character(as.data.frame(emm)[[var]]))
  ref_idx <- match(reference[[var]], lv)
  if (is.na(ref_idx)) stop("Reference level not found for ", var)

  within_all <- update(contrast(emm, method = "trt.vs.ctrl", ref = ref_idx, adjust = "none"), by = NULL)
  within_df <- as.data.frame(within_all)

  out <- list()
  for (term_i in unique(as.character(within_df$contrast))) {
    idx <- which(within_df$contrast == term_i)
    sub_emm <- within_all[idx]
    model_levels <- as.character(as.data.frame(sub_emm)$model_id)
    b_idx <- match(baseline_model, model_levels)
    if (is.na(b_idx)) stop("Baseline model not found: ", baseline_model)
    r <- as.data.frame(summary(contrast(sub_emm, method = "trt.vs.ctrl", ref = b_idx, adjust = "none"),
                               infer = c(TRUE, TRUE), adjust = "none"))
    r$model_id <- model_levels[-b_idx]
    r$term <- gsub(" - ", " vs ", term_i, fixed = TRUE)
    out[[term_i]] <- r
  }
  res <- bind_rows(out)
  lo <- grep("^(asymp.LCL|lower.CL)$", names(res), value = TRUE)
  hi <- grep("^(asymp.UCL|upper.CL)$", names(res), value = TRUE)
  res %>%
    mutate(pct = 100 * (exp(estimate) - 1), lo = 100 * (exp(.data[[lo]]) - 1),
           hi = 100 * (exp(.data[[hi]]) - 1), significant = !is.na(p.value) & p.value < 0.05) %>%
    select(model_id, term, estimate, pct, lo, hi, p.value, significant)
}

# ------------------------------------------------------------
# 5. Ranking
# ------------------------------------------------------------
# Gamma(log) fit: arithmetic mean score per model (overall and by horizon / trend / granularity).
# Gaussian fit on log(WIS): geometric mean, plus an arithmetic mean assuming log-normal errors.
rank_models <- function(fit, data) {
  edata <- attr(fit, "fit_data")
  gauss <- is_gaussian(fit)
  val   <- if (gauss) "emmean" else "response"
  emm_  <- function(f) {
    if (gauss) emmeans(fit, f, weights = "equal", data = edata)
    else       emmeans(fit, f, weights = "equal", data = edata, regrid = "response")
  }

  emm_o   <- emm_(~ model_id)
  overall <- as.data.frame(emm_o) %>% mutate(rank = rank(.data[[val]])) %>% arrange(rank)
  if (gauss) overall <- overall %>% mutate(geo_mean = exp(emmean))

  # rank of each model within each level of `var` (1 = lowest mean score)
  rank_by <- function(var) {
    as.data.frame(emm_(as.formula(paste("~ model_id |", var)))) %>%
      group_by(.data[[var]]) %>% mutate(rank = rank(.data[[val]])) %>% ungroup() %>%
      select(model_id, all_of(var), rank) %>%
      pivot_wider(names_from = all_of(var), values_from = rank,
                  names_prefix = if (var == "horizon") "h" else "")
  }

  # ratio vs baseline (< 1: lower score than baseline; CI excluding 1 = clear difference)
  ref_idx <- match(baseline_model, levels(data$model_id))
  ctr <- contrast(emmeans(fit, ~ model_id, weights = "equal", data = edata),
                  "trt.vs.ctrl", ref = ref_idx, adjust = "none") %>%
    confint(adjust = "none") %>% as.data.frame()
  lo <- grep("^(asymp.LCL|lower.CL)$", names(ctr), value = TRUE)
  hi <- grep("^(asymp.UCL|upper.CL)$", names(ctr), value = TRUE)
  ratio_vs_baseline <- ctr %>%
    mutate(ratio = exp(estimate), ratio_lo = exp(.data[[lo]]), ratio_hi = exp(.data[[hi]]))

  out <- list(overall = overall,
              by_horizon = rank_by("horizon"), by_trend = rank_by("trend"),
              by_granularity = rank_by("granularity"),
              ratio_vs_baseline = ratio_vs_baseline)

  if (gauss) {   # arithmetic mean under log-normal errors, exp(mu + sigma^2 / 2); for ordering only
    out$arith_lognormal <- map_dfr(levels(data$model_id), function(m) {
      nd <- data
      nd$model_id <- factor(m, levels = levels(data$model_id))
      mu <- predict(fit, newdata = nd, type = "link", re.form = NA)
      sg <- predict(fit, newdata = nd, type = "disp", re.form = NA)
      data.frame(model_id = m, arith_mean = mean(exp(mu + sg^2 / 2)))
    }) %>% mutate(rank_arith = rank(arith_mean)) %>% arrange(rank_arith)
  }
  out
}

# ------------------------------------------------------------
# 6. Figures
# ------------------------------------------------------------
plot_adjusted <- function(results, var, level_order = NULL, xlab = "Adjusted mean WIS") {
  if (!is.null(level_order)) results[[var]] <- factor(results[[var]], levels = level_order)
  ggplot(results, aes(x = adjusted_wis, y = factor(model_id, levels = rev(model_order)))) +
    geom_errorbarh(aes(xmin = lower_wis, xmax = upper_wis), height = 0.2) +
    geom_point(size = 2.5, color = "steelblue") +
    facet_wrap(as.formula(paste("~", var)), nrow = 1) +
    labs(x = xlab, y = "Forecasting model") +
    theme_bw()
}

contrast_theme <- function() {
  theme_bw() +
    theme(axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 10),
          legend.position = "bottom", legend.text = element_text(size = 10),
          panel.grid.minor = element_blank())
}

plot_within <- function(dat, xlab) {
  dat <- dat %>% mutate(model_id = factor(model_id, levels = rev(intersect(model_order, unique(as.character(model_id))))),
                        term = factor(term, levels = unique(term)))
  dodge <- position_dodge(width = 0.65)
  ggplot(dat, aes(x = pct, y = model_id, color = term, group = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, position = dodge, linewidth = 0.6) +
    geom_point(size = 2.5, position = dodge) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = xlab, y = NULL, color = NULL) +
    contrast_theme()
}

plot_interaction <- function(dat, xlab) {
  lev <- rev(intersect(model_order, unique(as.character(dat$model_id))))
  dat <- dat %>% mutate(model_id = factor(model_id, levels = lev), term = factor(term, levels = unique(term)))
  dodge <- position_dodge(width = 0.65)
  ggplot(dat, aes(x = pct, y = model_id, color = term, group = term)) +
    scale_y_discrete(limits = lev) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.2, position = dodge, linewidth = 0.6) +
    geom_point(size = 2.5, position = dodge) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = xlab, y = NULL, color = NULL) +
    contrast_theme()
}

tag_theme <- function(size) theme(plot.tag = element_text(size = size, face = "bold"), plot.tag.position = c(0, 1))

# spec$mean_type ("mean" or "geometric mean") and spec$score_label ("WIS", "log1p-WIS", ...) set the wording
make_figures <- function(fit, tag, spec, save = TRUE) {
  mt <- spec$mean_type; sl <- spec$score_label

  xl <- paste("Adjusted", mt, sl)
  fig_a <- plot_adjusted(get_adjusted_wis(fit, "horizon"), "horizon", xlab = xl)
  fig_b <- plot_adjusted(get_adjusted_wis(fit, "trend"), "trend",
                         level_order = c("Increasing", "Stable", "Decreasing"), xlab = xl)
  fig_c <- plot_adjusted(get_adjusted_wis(fit, "granularity"), "granularity", xlab = xl) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
  adjusted <- (fig_a / ((fig_b | fig_c) + plot_layout(widths = c(3, 2)))) +
    plot_layout(heights = c(1, 0.8)) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") & tag_theme(22)

  three <- function(dats, xlab, plotter, size) {
    panels <- Map(function(d, ttl) plotter(d, xlab) + ggtitle(ttl), dats, c("Horizon", "Trend", "Granularity"))
    (panels[[1]] | panels[[2]] | panels[[3]]) + plot_layout(ncol = 3, widths = c(1.3, 1, 0.8)) +
      plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") & tag_theme(size)
  }
  within <- three(map(c("horizon", "trend", "granularity"), ~ get_within_contrast(fit, .x)),
                  paste0("Within-model change in ", mt, " ", sl, " (%)"), plot_within, 20)
  interaction <- three(map(c("horizon", "trend", "granularity"), ~ get_interaction_contrast(fit, .x)),
                       paste0("Interaction contrast in ", mt, " ", sl, " (%)"), plot_interaction, 22)

  figs <- list(adjusted = adjusted, within = within, interaction = interaction)
  if (save) {
    f <- function(prefix) file.path(fig_dir, paste0(prefix, "_", tag, ".png"))
    ggsave(f("figure4_geomean"), figs$adjusted, width = 2000 / 150, height = 1400 / 150, dpi = 150)
    ggsave(f("figure4_within_model_contrasts"), figs$within, width = 20, height = 6, dpi = 300)
    ggsave(f("figure4_interaction_contrasts"), figs$interaction, width = 20, height = 6, dpi = 300)
  }
  figs
}

# ------------------------------------------------------------
# 7. Run one scenario
# ------------------------------------------------------------
# Fits one scenario for each variant (all data, first 4 weeks dropped, ...), prints the results,
# saves the figures as figure4_*_<name>[_<variant>].png and returns a list with one entry per variant.
#   variants : named integer vector, number of first weekly submissions to drop
#   diagnostics : DHARMa residual checks;  show : also print the three figures
run_scenario <- function(name,
                         variants = c(all = 0L, drop4 = 4L),
                         diagnostics = TRUE, show = FALSE,
                         scenario_list = scenarios, base_data = base) {
  spec <- scenario_list[[name]]
  if (is.null(spec)) stop("Unknown scenario: ", name, ". Available: ", paste(names(scenario_list), collapse = ", "))

  cat("\n==================== ", name, " ====================\n")
  cat("score file : ", spec$score_file, "\n")
  cat("formula    : ", deparse(spec$formula, width.cutoff = 200), "\n")
  cat("dispersion : ", deparse(spec$dispformula), "\n")
  cat("family     : ", spec$family$family, "(", spec$family$link, ")\n")

  keep_zeros <- isTRUE(spec$keep_zeros)
  data_all <- build_model_data(base_data, spec$score_file, spec$y_transform, keep_zeros)
  cat("rows:", nrow(data_all), " | zeros kept:", keep_zeros, " | zero scores in data:", sum(data_all$wis == 0), "\n")

  out <- list()
  diags <- list()
  for (v in names(variants)) {
    key  <- if (v == "all") name else paste0(name, "_", v)
    data <- drop_first_weeks(data_all, variants[[v]])
    cat("\n-------- ", key, " (rows: ", nrow(data), ") --------\n")

    fit <- fit_scenario(data, spec)
    print(summary(fit))
    print(check_fit(fit))
    if (diagnostics) diags[[key]] <- run_diagnostics(fit, data, key)

    ranking <- rank_models(fit, data)
    print(ranking$overall)
    print(ranking$by_horizon)
    print(ranking$by_trend)
    print(ranking$by_granularity)
    print(ranking$ratio_vs_baseline)
    if (!is.null(ranking$arith_lognormal)) print(ranking$arith_lognormal)

    figs <- make_figures(fit, key, spec)
    if (show) { print(figs$adjusted); print(figs$within); print(figs$interaction) }

    out[[v]] <- list(key = key, spec = spec, data = data, fit = fit, ranking = ranking, figs = figs,
                     diag = diags[[key]])
  }

  # one PDF per scenario with the diagnostics of all its variants
  if (diagnostics && length(diags) > 0) {
    save_diagnostics_pdf(diags, file.path(fig_dir, paste0("diagnostics_", name, ".pdf")))
  }
  out
}

# Overall ranks of every fitted scenario/variant side by side.
# `results` = list of run_scenario() outputs, e.g. list(wis_gamma = ..., sqrt_gamma = ...)
compare_ranks <- function(results) {
  flat <- unlist(results, recursive = FALSE)           # one entry per scenario x variant
  map(flat, ~ .x$ranking$overall %>% select(model_id, !!.x$key := rank)) %>%
    reduce(full_join, by = "model_id") %>%
    arrange(across(2))
}
