# ============================================================================
# FRE497_FMVP_Analysis
# ============================================================================
# The analysis is built as a ladder, starting from PTS alone:
#
#     Baseline      PTS
#     + DRtg        does defence add anything?
#     + box score   do rebounds, assists, steals, blocks, efficiency add anything?
#     + clutch      do clutch-time statistics add anything?   (1997-2026 only)
#
# Validation
#     LOSO           hold out one season, train on all others. Tests
#                    generalisability. NOT a forecast: later seasons can
#                    inform earlier ones.
#     Forward-chain  train only on earlier seasons, predict the next one, then
#                    expand the window. The forecasting test.


# ============================================================================
# 1. LIBRARIES
# ============================================================================
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(survival)
  library(stringi)
  library(ranger)
  library(xgboost)
  library(nnet)

# ============================================================================
# 2. LOAD RAW DATA
# ============================================================================

set.seed(45)
SEED <- 45

fmvp_raw <- read_excel("NBA_Finals_Data_1984_2026.xlsx")
clutch_raw <- read_excel("NBA_Finals_Clutch_Stats_PerGame_1997_2026.xlsx")


# ============================================================================
# 3. CLEAN THE DATA
# ============================================================================

# Names differ between the two files in accents and punctuation,
# for example: "Manu Ginobili" vs "Manu Ginóbili", "J.R. Smith" vs "JR Smith".
make_player_key <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  gsub("[^a-z]", "", tolower(x))
}

# "2000-01" becomes 2001. "1999-00" becomes 2000, not 1900.
season_to_year <- function(season) {
  season     <- as.character(season)
  start_year <- as.integer(sub("-.*$", "", season))
  end_digits <- as.integer(sub("^.*-", "", season))
  end_year   <- floor(start_year / 100) * 100 + end_digits
  ifelse(end_year < start_year, end_year + 100L, end_year)
}

# TS% is renamed to TS so it works in model formulas without backticks.
fmvp_clean <- fmvp_raw |>
  rename(TS = `TS%`) |>
  mutate(
    Year       = as.integer(Year),
    Team       = as.character(Team),
    Player     = as.character(Player),
    FMVP       = as.integer(FMVP),
    player_key = make_player_key(Player),
    across(c(MP, PTS, TRB, AST, STL, BLK, TS, DRtg), as.numeric)
  ) |>
  select(Year, Team, Player, player_key, FMVP, MP,
         PTS, TRB, AST, STL, BLK, TS, DRtg)

# The clutch statistics, named with a cl_ prefix so they are easy to tell
# apart from the full-series statistics. Percentages are kept on their own
# scale; every predictor is standardised before modelling
clutch_predictors <- c("cl_MIN", "cl_PTS", "cl_REB", "cl_AST", "cl_TOV",
                       "cl_STL", "cl_BLK", "cl_TS", "cl_USG")

clutch_clean <- clutch_raw |>
  mutate(
    Year       = season_to_year(Season),
    player_key = make_player_key(Player),
    cl_MIN     = as.numeric(MIN),
    cl_PTS     = as.numeric(PTS),
    cl_REB     = as.numeric(REB),
    cl_AST     = as.numeric(AST),
    cl_TOV     = as.numeric(TOV),
    cl_STL     = as.numeric(STL),
    cl_BLK     = as.numeric(BLK),
    cl_TS      = as.numeric(`TS%`),
    cl_USG     = as.numeric(`USG%`)
  ) |>
  select(Year, player_key, all_of(clutch_predictors))

# A duplicated Year + player key would silently duplicate main-file rows.
clutch_duplicates <- clutch_clean |>
  count(Year, player_key, name = "rows") |>
  filter(rows > 1)


# ============================================================================
# 4. MERGE CLUTCH STATISTICS
# ============================================================================

# A championship-roster player absent from the clutch file logged no clutch
# minutes, so his clutch statistics are 0, not missing.
# cl_MIN = 0 already encodes "did not play in clutch time", so no separate
# indicator variable is needed.
fmvp_merged <- fmvp_clean |>
  left_join(clutch_clean, by = c("Year", "player_key")) |>
  mutate(across(all_of(clutch_predictors), ~ replace_na(.x, 0)))

clutch_years <- sort(unique(clutch_clean$Year))


# ============================================================================
# REPORTING HELPERS
# ============================================================================

print_section <- function(title, note = NULL) {
  cat("\n", strrep("=", 72), "\n", sep = "")
  cat(title, "\n", sep = "")
  if (!is.null(note)) cat(note, "\n", sep = "")
  cat(strrep("=", 72), "\n", sep = "")
}

print_subsection <- function(title) {
  cat("\n", title, "\n", sep = "")
}


# ============================================================================
# 5. BUILD THE CHOICE SETS
# ============================================================================
# Two-step filter:
#   (a) MP > 0. A player who did not play in the Finals cannot win FMVP.
#   (b) Complete data on the predictors used. Some players appeared but
#       attempted no shots, so TS is mathematically undefined.

box_score_predictors <- c("PTS", "TRB", "AST", "STL", "BLK", "TS", "DRtg")

build_choice_set <- function(data, predictors, years, label) {

  played   <- data |>
    filter(Year %in% years, !is.na(MP), MP > 0)
  complete <- played |>
    filter(if_all(all_of(predictors), ~ !is.na(.x)))

  audit <- played |>
    group_by(Year) |>
    summarise(Candidates_Before = n(), Winners_Before = sum(FMVP), .groups = "drop") |>
    left_join(
      complete |>
        group_by(Year) |>
        summarise(Candidates = n(), Winners = sum(FMVP), .groups = "drop"),
      by = "Year"
    ) |>
    mutate(
      Candidates = replace_na(Candidates, 0L),
      Winners = replace_na(Winners, 0L),
      Candidates_Removed = Candidates_Before - Candidates,
      Winners_Removed = Winners_Before - Winners
    )

  bad_years <- audit |> filter(Winners != 1)
  if (nrow(bad_years) > 0) {
    stop(label, " choice set does not have exactly one FMVP in every season: ",
         paste(bad_years$Year, collapse = ", "), call. = FALSE)
  }

  if (any(audit$Winners_Removed != 0)) {
    stop(label, " filtering removed an FMVP winner.", call. = FALSE)
  }

  cat(sprintf("%-22s %3d players | %2d seasons | %2d-%2d candidates | %2d winners\n",
              label, nrow(complete), nrow(audit),
              min(audit$Candidates), max(audit$Candidates), sum(audit$Winners)))

  complete
}

print_section("CHOICE SETS")
base_data   <- build_choice_set(fmvp_merged, box_score_predictors, 1984:2026,
                                "Full period 1984-2026")
clutch_data <- build_choice_set(fmvp_merged, c(box_score_predictors, clutch_predictors),
                                1997:2026, "Clutch era 1997-2026")

cat(sprintf("%-22s %.1f%% of clutch-era players logged clutch minutes\n",
            "Clutch minutes", 100 * mean(clutch_data$cl_MIN > 0)))
cat(sprintf("%-22s %d duplicate player-season keys\n",
            "Clutch file audit", nrow(clutch_duplicates)))


# ============================================================================
# 6. SCORING AND SEASON-LEVEL METRICS
# ============================================================================

# Run an expression, collecting warnings instead of printing them and turning
# errors into a flag rather than stopping the script.
try_quietly <- function(expr) {
  warnings_seen <- character()
  value <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  list(value = value,
       warnings = paste(unique(warnings_seen), collapse = " | "),
       failed = inherits(value, "error"))
}

# Standardise using TRAINING-fold statistics only. Using test-fold statistics
# would leak information from the held-out season into the model.
scale_train_test <- function(train, test, predictors) {
  for (v in predictors) {
    centre <- mean(train[[v]], na.rm = TRUE)
    spread <- sd(train[[v]], na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) spread <- 1
    test[[v]]  <- (test[[v]]  - centre) / spread
    train[[v]] <- (train[[v]] - centre) / spread
  }
  list(train = train, test = test)
}

# Turn raw model output into probabilities summing to 1 within a season.
#   "softmax"   for linear predictors (conditional logit)
#   "normalize" for absolute probabilities (forest, boosting, network)
to_choice_probability <- function(x, method) {
  x <- as.numeric(x)
  if (all(!is.finite(x))) return(rep(1 / length(x), length(x)))
  x[!is.finite(x)] <- if (method == "softmax") min(x, na.rm = TRUE) else 0
  p <- if (method == "softmax") exp(x - max(x)) else pmax(x, 0)
  if (!is.finite(sum(p)) || sum(p) <= 0) rep(1 / length(p), length(p)) else p / sum(p)
}

finish_predictions <- function(test, raw_score, method, sample_label, validation,
                               model_name, fit_status) {
  test |>
    mutate(Sample = sample_label, Validation = validation, Model = model_name,
           Raw_Score = as.numeric(raw_score), Fit_Status = fit_status) |>
    group_by(Year) |>
    mutate(Choice_Probability = to_choice_probability(Raw_Score, method)) |>
    arrange(desc(Choice_Probability), desc(PTS), Player, .by_group = TRUE) |>
    mutate(Predicted_Rank = row_number()) |>
    ungroup()
}

# Choice log loss is unbounded, so a floor stops one catastrophic season from
# dominating the average. Median log loss is reported alongside the mean.
prob_floor <- 1e-9

evaluate_by_season <- function(scores) {
  scores |>
    group_by(Sample, Validation, Model, Year) |>
    arrange(Predicted_Rank, .by_group = TRUE) |>
    summarise(
      Actual_Winner      = Player[FMVP == 1][1],
      Predicted_Winner   = Player[1],
      Winner_Probability = Choice_Probability[FMVP == 1][1],
      Winner_Rank        = Predicted_Rank[FMVP == 1][1],
      Choice_Brier       = sum((Choice_Probability - FMVP)^2),
      Fit_Status         = first(Fit_Status),
      .groups = "drop"
    ) |>
    mutate(
      Correct        = as.integer(Winner_Rank == 1),
      In_Top_2       = as.integer(Winner_Rank <= 2),
      Choice_LogLoss = -log(pmax(Winner_Probability, prob_floor))
    )
}

# Model_Order keeps the ladder in its logical order in printed tables rather
# than sorting by performance.
summarise_models <- function(by_season, model_levels) {
  by_season |>
    group_by(Sample, Validation, Model) |>
    summarise(
      Seasons     = n(),
      N_Correct   = sum(Correct),
      Top1        = mean(Correct),
      Top2        = mean(In_Top_2),
      LogLoss     = mean(Choice_LogLoss),
      LogLoss_Med = median(Choice_LogLoss),
      Brier       = mean(Choice_Brier),
      Missed      = paste(Year[Correct == 0], collapse = ", "),
      .groups = "drop"
    ) |>
    mutate(Model = factor(Model, levels = model_levels)) |>
    arrange(Sample, Validation, Model)
}


# ============================================================================
# 7. MODEL FUNCTIONS
# ============================================================================

# A standardised coefficient larger than this means the fit has diverged.
# Separation is expected here: in folds excluding the anomalous seasons,
# scoring nearly separates winners from non-winners, so the unpenalized
# estimate runs toward infinity.
coef_limit       <- 20
ridge_theta_base <- 1   # fallback strength, used only when a fit diverges

# force_ridge = TRUE applies ridge unconditionally. This is used for BOTH arms
# of the clutch comparison, so the arms differ only in their feature set and
# never in estimation method.
fit_conditional_logit <- function(train, test, predictors, sample_label, validation,
                                  model_name, force_ridge = FALSE,
                                  ridge_theta = ridge_theta_base, ...) {

  scaled <- scale_train_test(train, test, predictors)

  plain <- try_quietly(clogit(
    as.formula(paste("FMVP ~", paste(predictors, collapse = " + "), "+ strata(Year)")),
    data = scaled$train, method = "exact"))

  diverged <- plain$failed ||
    any(!is.finite(coef(plain$value))) ||
    any(abs(coef(plain$value)) > coef_limit, na.rm = TRUE) ||
    plain$warnings != ""

  if (force_ridge || diverged) {
    penalised <- try_quietly(clogit(
      as.formula(paste0("FMVP ~ ridge(", paste(predictors, collapse = ", "),
                        ", theta = ", ridge_theta, ") + strata(Year)")),
      data = scaled$train, method = "efron"))
    fit        <- penalised$value
    fit_status <- if (force_ridge) "ridge_forced" else "ridge_fallback"
  } else {
    fit        <- plain$value
    fit_status <- "exact"
  }

  # predict.clogit() cannot score a held-out season, because that season is a
  # NEW stratum. The linear predictor is computed by hand from the fitted
  # coefficients instead.
  if (inherits(fit, "error")) {
    raw_score  <- rep(0, nrow(test))
    fit_status <- "failed"
  } else {
    beta <- as.numeric(coef(fit))
    if (any(!is.finite(beta)) || length(beta) != length(predictors)) {
      raw_score  <- rep(0, nrow(test))
      fit_status <- "failed"
    } else {
      raw_score <- as.numeric(
        as.matrix(scaled$test[, predictors, drop = FALSE]) %*% beta)
    }
  }

  finish_predictions(test, raw_score, "softmax", sample_label, validation,
                     model_name, fit_status)
}

# Fixed, conservative hyperparameters throughout. With 30-43 winners per
# training fold, per-fold tuning would itself be high-variance noise-chasing.
fit_random_forest <- function(train, test, predictors, sample_label, validation,
                              model_name, fold_seed, ...) {
  train_rf <- train |> mutate(FMVP_factor = factor(FMVP, levels = c(0, 1)))

  forest <- try_quietly(ranger(
    formula = as.formula(paste("FMVP_factor ~", paste(predictors, collapse = " + "))),
    data = train_rf, probability = TRUE, num.trees = 500,
    mtry = max(1, floor(sqrt(length(predictors)))), min.node.size = 2,
    seed = fold_seed, num.threads = 1))

  raw_score <- if (forest$failed) rep(mean(train$FMVP), nrow(test)) else
    predict(forest$value, data = test)$predictions[, "1"]

  finish_predictions(test, raw_score, "normalize", sample_label, validation,
                     model_name, if (forest$failed) "failed" else "fixed_hyperparameters")
}

# scale_pos_weight offsets the roughly 1-in-12 positive rate. Note xgboost
# ignores a `seed` parameter in R; set.seed() is what controls it.
fit_xgboost <- function(train, test, predictors, sample_label, validation,
                        model_name, fold_seed, ...) {
  scaled  <- scale_train_test(train, test, predictors)
  x_train <- as.matrix(scaled$train[, predictors, drop = FALSE])
  x_test  <- as.matrix(scaled$test[,  predictors, drop = FALSE])

  set.seed(fold_seed)
  booster <- try_quietly(xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "logloss",
                  max_depth = 2, eta = 0.05, subsample = 0.80,
                  colsample_bytree = 0.80, min_child_weight = 2, lambda = 1,
                  scale_pos_weight = sum(train$FMVP == 0) / max(sum(train$FMVP == 1), 1),
                  nthread = 1),
    data = xgb.DMatrix(data = x_train, label = train$FMVP),
    nrounds = 100, verbose = 0))

  raw_score <- if (booster$failed) rep(mean(train$FMVP), nrow(test)) else
    as.numeric(predict(booster$value, newdata = x_test))

  finish_predictions(test, raw_score, "normalize", sample_label, validation,
                     model_name, if (booster$failed) "failed" else "fixed_hyperparameters")
}

# Weight decay is essential at this sample size; an unregularised network
# would simply memorise the training folds.
fit_neural_network <- function(train, test, predictors, sample_label, validation,
                               model_name, fold_seed, ...) {
  scaled  <- scale_train_test(train, test, predictors)
  x_train <- as.matrix(scaled$train[, predictors, drop = FALSE])
  x_test  <- as.matrix(scaled$test[,  predictors, drop = FALSE])

  set.seed(fold_seed)
  network <- try_quietly(nnet(x = x_train, y = train$FMVP, size = 2, decay = 0.10,
                              maxit = 500, entropy = TRUE, trace = FALSE))

  raw_score <- if (network$failed) rep(mean(train$FMVP), nrow(test)) else
    as.numeric(predict(network$value, newdata = x_test, type = "raw"))

  finish_predictions(test, raw_score, "normalize", sample_label, validation,
                     model_name, if (network$failed) "failed" else "fixed_hyperparameters")
}


# ============================================================================
# 8. VALIDATION ENGINE
# ============================================================================

run_validation <- function(data, model_list, sample_label, validation,
                           first_train_end = NULL) {

  seasons <- sort(unique(data$Year))
  test_seasons <- if (validation == "Forward-chain") {
    seasons[seasons > first_train_end]
  } else {
    seasons
  }

  results <- list()
  slot <- 1

  for (test_year in test_seasons) {
    train <- if (validation == "Forward-chain") {
      data |> filter(Year < test_year)
    } else {
      data |> filter(Year != test_year)
    }
    test <- data |> filter(Year == test_year)

    for (m in seq_along(model_list)) {
      spec <- model_list[[m]]

      # A per-fold seed derived from SEED, so each fold's result is stable
      # regardless of what runs before it.
      results[[slot]] <- spec$fun(
        train = train, test = test, predictors = spec$predictors,
        sample_label = sample_label, validation = validation,
        model_name = spec$name,
        fold_seed = SEED + test_year + 1000 * m,
        force_ridge = isTRUE(spec$force_ridge),
        ridge_theta = if (is.null(spec$ridge_theta)) ridge_theta_base else spec$ridge_theta)
      slot <- slot + 1
    }
  }

  bind_rows(results)
}


# ============================================================================
# 9. PART 1 -- DOES ANYTHING BEYOND SCORING HELP?  (1984-2026)
# ============================================================================
# The baseline is PTS alone, fitted as a one-variable choice model. It ranks
# players identically to "the highest scorer wins", while also producing
# calibrated probabilities on the same scale as every other model.

baseline_predictors  <- c("PTS")
plus_drtg_predictors <- c("PTS", "DRtg")
# box_score_predictors was defined in section 6, where the choice set needed it

base_forward_first_train_end <- 1993   # first forecast season is 1994

part1_models <- list(
  list(name = "PTS (baseline)",  predictors = baseline_predictors,  fun = fit_conditional_logit),
  list(name = "+ DRtg",          predictors = plus_drtg_predictors, fun = fit_conditional_logit),
  list(name = "+ box score",     predictors = box_score_predictors, fun = fit_conditional_logit),
  list(name = "Random forest",   predictors = box_score_predictors, fun = fit_random_forest),
  list(name = "XGBoost",         predictors = box_score_predictors, fun = fit_xgboost),
  list(name = "Neural network",  predictors = box_score_predictors, fun = fit_neural_network)
)

part1_order <- c("PTS (baseline)", "+ DRtg", "+ box score",
                 "Random forest", "XGBoost", "Neural network")

part1_scores <- bind_rows(
  run_validation(base_data, part1_models, "1984-2026", "LOSO"),
  run_validation(base_data, part1_models, "1984-2026", "Forward-chain",
                 base_forward_first_train_end)
)


# ============================================================================
# 10. PART 2 -- DO CLUTCH STATISTICS HELP?  (1997-2026)
# ============================================================================
# Both arms use the identical choice sets AND identical estimation (ridge with
# the same theta), so the only difference between them is the feature set.
# Ridge is forced because the clutch model has only about 1.9 winners per
# predictor, where an unpenalized fit diverges.

with_clutch_predictors <- c(box_score_predictors, clutch_predictors)

clutch_forward_first_train_end <- 2006   # first forecast season is 2007
ridge_theta_clutch <- 10                 # forced on BOTH arms, symmetrically

part2_base_models <- list(
  list(name = "Conditional logit", predictors = box_score_predictors, fun = fit_conditional_logit,
       force_ridge = TRUE, ridge_theta = ridge_theta_clutch),
  list(name = "Random forest", predictors = box_score_predictors, fun = fit_random_forest),
  list(name = "XGBoost",       predictors = box_score_predictors, fun = fit_xgboost),
  list(name = "Neural network",predictors = box_score_predictors, fun = fit_neural_network)
)

part2_clutch_models <- list(
  list(name = "Conditional logit", predictors = with_clutch_predictors, fun = fit_conditional_logit,
       force_ridge = TRUE, ridge_theta = ridge_theta_clutch),
  list(name = "Random forest", predictors = with_clutch_predictors, fun = fit_random_forest),
  list(name = "XGBoost",       predictors = with_clutch_predictors, fun = fit_xgboost),
  list(name = "Neural network",predictors = with_clutch_predictors, fun = fit_neural_network)
)

part2_order <- c("Conditional logit", "Random forest", "XGBoost", "Neural network")

part2_scores <- bind_rows(
  run_validation(clutch_data, part2_base_models,   "Box score only",  "LOSO"),
  run_validation(clutch_data, part2_base_models,   "Box score only",  "Forward-chain",
                 clutch_forward_first_train_end),
  run_validation(clutch_data, part2_clutch_models, "With clutch",     "LOSO"),
  run_validation(clutch_data, part2_clutch_models, "With clutch",     "Forward-chain",
                 clutch_forward_first_train_end)
)


# ============================================================================
# 11. RESULTS
# ============================================================================

part1_by_season <- evaluate_by_season(part1_scores)
part2_by_season <- evaluate_by_season(part2_scores)

part1_summary <- summarise_models(part1_by_season, part1_order)
part2_summary <- summarise_models(part2_by_season, part2_order)

show_table <- function(x) {
  x |>
    mutate(Top1 = sprintf("%.3f", Top1), Top2 = sprintf("%.3f", Top2),
           LogLoss = sprintf("%.3f", LogLoss), LogLoss_Med = sprintf("%.3f", LogLoss_Med),
           Brier = sprintf("%.3f", Brier)) |>
    as.data.frame() |>
    print(row.names = FALSE)
}

print_section(
  "PART 1: DOES ANYTHING BEYOND SCORING HELP? (1984-2026)",
  "Metrics: higher Top1/Top2 is better; lower LogLoss/Brier is better."
)

for (v in c("Forward-chain", "LOSO")) {
  print_subsection(
    paste0(v, if (v == "Forward-chain") " (forecast-style test)" else
      " (generalisability check)")
  )
  show_table(part1_summary |> filter(Validation == v) |>
               select(Model, Seasons, N_Correct, Top1, Top2, LogLoss, LogLoss_Med, Brier))
}

print_section(
  "PART 2: DO CLUTCH STATISTICS HELP? (1997-2026)",
  "Same seasons, same players, same estimation; only the clutch variables change."
)

for (v in c("Forward-chain", "LOSO")) {
  print_subsection(v)
  side_by_side <- part2_summary |>
    filter(Validation == v) |>
    select(Model, Sample, Top1, LogLoss, Brier) |>
    tidyr::pivot_wider(names_from = Sample,
                       values_from = c(Top1, LogLoss, Brier)) |>
    mutate(Model = factor(Model, levels = part2_order)) |>
    arrange(Model) |>
    transmute(Model,
              `Top1 box` = sprintf("%.3f", `Top1_Box score only`),
              `Top1 +clutch` = sprintf("%.3f", `Top1_With clutch`),
              `LogLoss box` = sprintf("%.3f", `LogLoss_Box score only`),
              `LogLoss +clutch` = sprintf("%.3f", `LogLoss_With clutch`),
              `Change` = sprintf("%+.3f", `LogLoss_With clutch` - `LogLoss_Box score only`))
  print(as.data.frame(side_by_side), row.names = FALSE)
}
cat("\nChange = LogLoss with clutch minus LogLoss without clutch.\n")
cat("Negative change means clutch helped; positive change means clutch hurt.\n")


# ============================================================================
# 12. IS THE DIFFERENCE REAL?  PAIRED TESTS
# ============================================================================
# The season is the unit of observation, so whole seasons are resampled.

bootstrap_reps <- 2000

compare_pair <- function(by_season, sample_a, model_a, sample_b, model_b,
                         validation, label) {
  set.seed(SEED)

  a <- by_season |> filter(Sample == sample_a, Model == model_a,
                           Validation == validation) |> arrange(Year)
  b <- by_season |> filter(Sample == sample_b, Model == model_b,
                           Validation == validation) |> arrange(Year)
  if (nrow(a) == 0 || !identical(a$Year, b$Year)) return(tibble())

  gap   <- a$Choice_LogLoss - b$Choice_LogLoss
  draws <- replicate(bootstrap_reps, {
    idx <- sample.int(nrow(a), nrow(a), replace = TRUE)
    mean(gap[idx])
  })

  # Seasons where exactly one of the two models picked the winner.
  a_only     <- sum(a$Correct == 1 & b$Correct == 0)
  b_only     <- sum(a$Correct == 0 & b$Correct == 1)
  discordant <- a_only + b_only

  tibble(
    Comparison = label, Validation = validation, Seasons = nrow(a),
    LogLoss_Change = mean(gap),
    CI_Lower = quantile(draws, 0.025), CI_Upper = quantile(draws, 0.975),
    Disagreed = discordant,
    p_value = if (discordant == 0) NA_real_ else
      binom.test(a_only, discordant, p = 0.5)$p.value)
}

comparisons <- bind_rows(
  compare_pair(part1_by_season, "1984-2026", "+ DRtg",
               "1984-2026", "PTS (baseline)", "Forward-chain",
               "+ DRtg vs PTS"),
  compare_pair(part1_by_season, "1984-2026", "+ box score",
               "1984-2026", "+ DRtg", "Forward-chain",
               "+ box score vs + DRtg"),
  compare_pair(part1_by_season, "1984-2026", "+ box score",
               "1984-2026", "PTS (baseline)", "Forward-chain",
               "+ box score vs PTS"),
  compare_pair(part1_by_season, "1984-2026", "+ box score",
               "1984-2026", "PTS (baseline)", "LOSO",
               "+ box score vs PTS"),
  compare_pair(part2_by_season, "With clutch", "Conditional logit",
               "Box score only", "Conditional logit", "Forward-chain",
               "+ clutch vs box score"),
  compare_pair(part2_by_season, "With clutch", "Conditional logit",
               "Box score only", "Conditional logit", "LOSO",
               "+ clutch vs box score")
)

print_section(
  "PAIRED SEASON-LEVEL COMPARISONS",
  "Negative LogLoss change favours the first model; intervals crossing zero suggest no clear difference."
)
cat("Disagreed = seasons where the two models picked different players.\n\n")

print(comparisons |>
        mutate(LogLoss_Change = sprintf("%+.3f", LogLoss_Change),
               Interval = sprintf("[%+.3f, %+.3f]", CI_Lower, CI_Upper),
               p_value = ifelse(is.na(p_value), "-", sprintf("%.3f", p_value))) |>
        transmute(Comparison, Validation, Seasons, Change = LogLoss_Change,
                  `95% CI` = Interval, `Diff Picks` = Disagreed, p = p_value) |>
        as.data.frame(), row.names = FALSE)


# ============================================================================
# 13. FORMAL TESTS AND COEFFICIENTS
# ============================================================================
# Fitted UNPENALIZED on the full sample. Ridge is used for prediction only:
# ridge coefficients invalidate the usual standard errors and likelihood-ratio
# statistics, so inference and prediction are kept separate.

# The full model with all nine clutch variables does not converge unpenalized
# (about 1.9 winners per predictor), so the formal clutch test uses a reduced
# set that does converge.
reduced_clutch_predictors <- c(box_score_predictors, "cl_MIN", "cl_PTS")

fit_for_inference <- function(data, predictors) {
  for (v in predictors) {
    spread <- sd(data[[v]], na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) spread <- 1
    data[[v]] <- (data[[v]] - mean(data[[v]], na.rm = TRUE)) / spread
  }
  try_quietly(clogit(
    as.formula(paste("FMVP ~", paste(predictors, collapse = " + "), "+ strata(Year)")),
    data = data, method = "exact"))
}

nested_test <- function(data, small, large, label) {
  small_fit <- fit_for_inference(data, small)
  large_fit <- fit_for_inference(data, large)

  unusable <- small_fit$failed || large_fit$failed ||
    any(abs(coef(small_fit$value)) > coef_limit, na.rm = TRUE) ||
    any(abs(coef(large_fit$value)) > coef_limit, na.rm = TRUE) ||
    small_fit$warnings != "" ||
    large_fit$warnings != ""

  if (unusable) {
    return(tibble(Test = label, LR = NA_real_, df = NA_integer_,
                  p_value = NA_real_, Note = "did not converge"))
  }

  lr <- 2 * (as.numeric(logLik(large_fit$value)) - as.numeric(logLik(small_fit$value)))
  df <- length(coef(large_fit$value)) - length(coef(small_fit$value))
  tibble(Test = label, LR = lr, df = df,
         p_value = pchisq(lr, df = df, lower.tail = FALSE), Note = "")
}

print_section(
  "LIKELIHOOD-RATIO TESTS",
  "These full-sample tests ask whether each added variable set improves explanatory fit."
)

print(bind_rows(
  nested_test(base_data, baseline_predictors, plus_drtg_predictors,
              "PTS -> + DRtg                    (1984-2026)"),
  nested_test(base_data, plus_drtg_predictors, box_score_predictors,
              "+ DRtg -> + box score            (1984-2026)"),
  nested_test(base_data, baseline_predictors, box_score_predictors,
              "PTS -> + box score               (1984-2026)"),
  nested_test(clutch_data, box_score_predictors, reduced_clutch_predictors,
              "box score -> + clutch (reduced)  (1997-2026)"),
  nested_test(clutch_data, box_score_predictors, with_clutch_predictors,
              "box score -> + clutch (all nine) (1997-2026)")
) |>
  mutate(LR = sprintf("%.3f", LR),
         p_value = ifelse(is.na(p_value), "-", sprintf("%.4f", p_value))) |>
  as.data.frame(), row.names = FALSE)

print_section(
  "FULL-SAMPLE COEFFICIENTS: BOX-SCORE MODEL (1984-2026)",
  "Coefficients are per one standard deviation; lower DRtg means better defense."
)

box_fit <- fit_for_inference(base_data, box_score_predictors)
box_coefficients <- summary(box_fit$value)$coefficients
print(data.frame(
  Variable    = rownames(box_coefficients),
  Coefficient = sprintf("%+.3f", box_coefficients[, 1]),
  Std_Error   = sprintf("%.3f",  box_coefficients[, 3]),
  p_value     = sprintf("%.4f",  box_coefficients[, 5])
), row.names = FALSE)


# ============================================================================
# 14. WHICH SEASONS ARE HARD?
# ============================================================================

print_section(
  "HARD SEASONS",
  "Seasons missed by at least four of the six Part 1 models under forward-chain validation."
)

hard_seasons <- part1_by_season |>
  filter(Validation == "Forward-chain", Correct == 0) |>
  count(Year, Actual_Winner, name = "Models_Missing") |>
  filter(Models_Missing >= 4) |>
  arrange(desc(Models_Missing), Year)

print(as.data.frame(hard_seasons), row.names = FALSE)
cat("\n(out of", length(part1_order), "models)\n")

fit_audit <- part1_scores |>
  bind_rows(part2_scores) |>
  distinct(Sample, Validation, Model, Year, Fit_Status) |>
  count(Sample, Validation, Model, Fit_Status, name = "Folds")

print_subsection("FIT STATUS")
print(as.data.frame(fit_audit |> filter(Fit_Status != "fixed_hyperparameters")),
      row.names = FALSE)
