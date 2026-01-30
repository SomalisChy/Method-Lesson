# ************************************************************
# Replication Lesson (Stata -> R) 
# ************************************************************
install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")
install.packages("survey")
install.packages("twang")
install.packages("ggplot2")
install.packages("devtools")
install.packages("twangContinuous")  # run once
library(twangContinuous)

library(devtools)
library(dplyr)
library(tidyr)
library(haven)
library(twang)
library(survey)
library(ggplot2)



# Set working directory (Windows path: use forward slashes or double backslashes)
setwd("C:/Users/chy/Box/replication project_jj/data_stata")

# --- Load data -------------------------------------------------
els <- readRDS("C:/Users/chy/Box/replication project_jj/data_stata/els_0212.rds")


# --- Bookmark #1: Clean and define treatment variable ----------
treat_vars <- paste0("BYA38", LETTERS[1:16])  # BYA38A ... BYA38P

els <- els %>%
 
  mutate(across(all_of(treat_vars), ~ ifelse(.x %in% c(-9, -8, -7, -4), NA, .x))) %>%
  
# Drop rows if ANY of BYA38A-P is missing
filter(if_all(all_of(treat_vars), ~ !is.na(.x))) %>%
  # Create index (sum across all variables)
  mutate(sch_strict = rowSums(across(all_of(treat_vars)), na.rm = TRUE)) %>%
  # Quartiles (like xtile, nq(4))
  mutate(qrt_sch_strict = ntile(sch_strict, 4)) %>%
  # Binary highest vs lowest (as in your Stata)
  mutate(
    bi_sch_str = case_when(
      sch_strict == 0  ~ 0,
      sch_strict == 13 ~ 1,
      TRUE ~ NA_real_
    )
  )

summary(els$sch_strict)

# --- Bookmark #2: Outcomes ------------------------------------
els <- els %>%
  # Math
  mutate(BYTXMSTD = ifelse(BYTXMSTD == -8, NA, BYTXMSTD)) %>%
  # Behavior BYS24E
  mutate(
    new_BYS24E = case_when(
      BYS24E == 1 ~ 0,
      BYS24E %in% 2:5 ~ 1,
      BYS24E %in% c(-9, -8, -7, -6, -4) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  # Behavior BYS24F
  mutate(
    new_BYS24F = case_when(
      BYS24F == 1 ~ 0,
      BYS24F %in% 2:5 ~ 1,
      BYS24F %in% c(-9, -8, -7, -6, -4) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(els$new_BYS24E, useNA = "ifany")
table(els$new_BYS24F, useNA = "ifany")

# --- Bookmark #3: Selection vars ------------------------------
byf_vars <- paste0("BYF09", LETTERS[1:5])  # BYF09A ... BYF09E
byp_vars <- c("BYP67", "BYP68")

els <- els %>%
  mutate(across(all_of(byf_vars), ~ ifelse(.x %in% c(-9, -8), NA, .x))) %>%
  mutate(across(all_of(byp_vars), ~ ifelse(.x %in% c(-9, -8, -7, -6, -4, -2, -1), NA, .x)))

# --- Bookmark #4: Covariates ----------------------------------
els <- els %>%
  mutate(
    new_BYRACE = case_when(
      BYRACE == 1 ~ 2,
      BYRACE == 2 ~ 8,
      BYRACE == 7 ~ 1,
      BYRACE %in% 4:6 ~ 2,
      BYRACE == 8 ~ 4,
      BYRACE %in% c(-8, -4) ~ NA_real_,
      TRUE ~ BYRACE
    ),
    new_BYFCOMP = case_when(
      BYFCOMP %in% 2:4 ~ 1,
      BYFCOMP %in% 5:8 ~ 2,
      BYFCOMP %in% c(9, -9, -8, -4) ~ NA_real_,
      TRUE ~ BYFCOMP
    )
  )

table(els$new_BYRACE, useNA = "ifany")
table(els$new_BYFCOMP, useNA = "ifany")

# ************************************************************
# Bookmark #5: Selection models + weight visualizations (Twang)
# ************************************************************

## convert to data.frame
els <- els %>% as.data.frame()

# Check structure
class(els)

# ---- 0) Define covariates you want in the selection models ----
# (Selection vars: BYF09A-E, BYP67, BYP68)
# (Covariates: BYSEX, new_BYRACE, BYSES1, BYURBAN, BYP46, BYP49, BYS67, new_BYFCOMP)
covars <- c(
  byf_vars,            # BYF09A ... BYF09E
  byp_vars,            # BYP67, BYP68
  "BYSEX",
  "new_BYRACE",
  "BYSES1",
  "BYURBAN",
  "BYP46",
  "BYP49",
  "BYS67",
  "new_BYFCOMP"
)

# Helper: quick weight distribution plot
plot_weight_dist <- function(dat, w_col, treat_col, title_txt = "") {
  ggplot(dat, aes(x = .data[[w_col]])) +
    geom_histogram(bins = 60) +
    facet_wrap(vars(.data[[treat_col]]), scales = "free_y") +
    labs(
      title = title_txt,
      x = "Weight",
      y = "Count"
    )
}

# ============================================================
# 1) BINARY selection model (PS): bi_sch_str
#    - uses els$bi_sch_str (0 vs 1), drops NA in treat + covars
# ============================================================

els_bin <- els %>%
  filter(!is.na(bi_sch_str)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  mutate(
    # FORCE numeric 0/1 (not factor)
    bi_sch_str = as.numeric(bi_sch_str)
  ) %>%
  filter(bi_sch_str %in% c(0, 1)) %>%     # drop anything not 0/1
  as.data.frame()                         # TWANG requires data.frame

# Sanity check (must show only 0 and 1)
table(els_bin$bi_sch_str, useNA = "ifany")
str(els_bin$bi_sch_str)

set.seed(123)

ps_bin <- twang::ps(
  formula = as.formula(paste("bi_sch_str ~", paste(covars, collapse = " + "))),
  data    = els_bin,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.01,
  perm.test.iters = 0,
  stop.method = c("es.mean", "ks.max"),
  estimand = "ATE",
  verbose = FALSE
)

# --- Get ATE weights (one set per stop.method; pick one) ---
w_bin <- get.weights(ps_bin, stop.method = "es.mean")
els_bin$w_bin <- as.numeric(w_bin)

# --- Visualize weighting (built-in Twang plots) ---
# Common useful plots: "boxplot" of propensity scores, "es" balance, "ks"
plot(ps_bin, plots = 1)  # propensity score distributions
plot(ps_bin, plots = 2)  # balance (ES) by stop methods

# --- Visualize weighting (custom): weight distributions by treatment ---
print(plot_weight_dist(
  dat = els_bin,
  w_col = "w_bin",
  treat_col = "bi_sch_str",
  title_txt = "Binary PS weights by treatment group (bi_sch_str)"
))

# (Optional) Visualize propensity score distribution by group
els_bin$ps_hat <- ps_bin$ps$es.mean  # uses es.mean stop method fitted ps
ggplot(els_bin, aes(x = ps_hat)) +
  geom_histogram(bins = 60) +
  facet_wrap(~ bi_sch_str, scales = "free_y") +
  labs(title = "Propensity score distribution (binary PS)", x = "Propensity score", y = "Count")

# ============================================================
# 2) MULTINOMIAL selection model (MNPS): qrt_sch_strict (4 groups)
# ============================================================

els_mn <- els %>%
  filter(!is.na(qrt_sch_strict)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  mutate(qrt_sch_strict = factor(qrt_sch_strict, levels = 1:4)) %>%
  as.data.frame()                         # TWANG requires data.frame

   set.seed(123)

mnps_q <- mnps(
  formula        = as.formula(paste("qrt_sch_strict ~", paste(covars, collapse = " + "))),
  data           = els_mn,
  n.trees        = 5000,
  interaction.depth = 2,
  shrinkage      = 0.01,
  perm.test.iters = 0,
  stop.method    = c("es.mean", "ks.max"),
  estimand       = "ATE",
  verbose        = FALSE
)

# --- Get ATE weights (choose stop.method) ---
w_mn <- get.weights(mnps_q, stop.method = "es.mean")
els_mn$w_mn <- as.numeric(w_mn)

# --- Visualize weighting (built-in Twang plots) ---
plot(mnps_q, plots = 1)  # generalized PS distributions
plot(mnps_q, plots = 2)  # balance (ES) plot

# --- Visualize weighting (custom): weight distributions by quartile ---
print(plot_weight_dist(
  dat = els_mn,
  w_col = "w_mn",
  treat_col = "qrt_sch_strict",
  title_txt = "MNPS weights by treatment group (qrt_sch_strict quartiles)"
))

# ============================================================
# 3) CONTINUOUS selection model (PS.CONT): sch_strict (0–13)
# ============================================================

els_cont <- els %>%
  filter(!is.na(sch_strict)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  as.data.frame() 

set.seed(123)

ps_cont_fit <- ps.cont(
  formula        = as.formula(paste("sch_strict ~", paste(covars, collapse = " + "))),
  data           = els_cont,
  n.trees        = 5000,
  interaction.depth = 2,
  shrinkage      = 0.01,
  perm.test.iters = 0,
  stop.method    = c("es.mean", "ks.max"),
  verbose        = FALSE
)

# --- Get continuous-treatment weights (choose stop.method) ---
w_cont <- get.weights(ps_cont_fit, stop.method = "es.mean")
els_cont$w_cont <- as.numeric(w_cont)


# --- Visualize weighting (custom): weight distribution overall + by binned sch_strict ---

els_cont <- els_cont %>%
  mutate(
    sch_strict_bin = factor(
      ntile(sch_strict, 4),
      labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")
    )
  )

# Overall weight distribution
ggplot(els_cont, aes(x = w_cont)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Continuous PS weights (overall)",
    x = "Weight",
    y = "Count"
  )

# By binned treatment
plot_weight_dist(
  dat        = els_cont,
  w_col      = "w_cont",
  treat_col  = "sch_strict_bin",
  title_txt  = "Continuous PS weights by sch_strict quartile"
)




# ************************************************************
# Bookmark #6: Outcome models for EACH outcome
# Structure per outcome:
#   1) Outcome = Treatment
#   2) Outcome = Treatment (PSWT)
#   3) Outcome = Treatment + Controls (PSWT)
# ************************************************************

# Outcomes to run (continuous vs binary)
outcomes <- list(
  BYTXMSTD   = "continuous",
  new_BYS24E = "binary",
  new_BYS24F = "binary"
)

# Controls to use in weighted + controls models
# (re-uses the same covars vector you defined above)
controls <- covars

# Helper: fit the 3-model set for one outcome/treatment/weights
fit_outcome_set <- function(dat, outcome, outcome_type, treat, w_col, controls) {
  
  # Keep complete cases needed for the 3 models
  keep_vars <- unique(c(outcome, treat, w_col, controls))
  d <- dat %>%
    dplyr::select(dplyr::all_of(keep_vars)) %>%
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[treat]])) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(controls), ~ !is.na(.x))) %>%
    as.data.frame()
  
  f1 <- as.formula(paste(outcome, "~", treat))
  f3 <- as.formula(paste(outcome, "~", treat, "+", paste(controls, collapse = " + ")))
  
  if (outcome_type == "continuous") {
    m1 <- lm(f1, data = d)
    m2 <- lm(f1, data = d, weights = d[[w_col]])
    m3 <- lm(f3, data = d, weights = d[[w_col]])
  } else if (outcome_type == "binary") {
    # expects outcome coded 0/1
    m1 <- glm(f1, data = d, family = binomial())
    m2 <- glm(f1, data = d, family = binomial(), weights = d[[w_col]])
    m3 <- glm(f3, data = d, family = binomial(), weights = d[[w_col]])
  } else {
    stop("Unknown outcome_type: ", outcome_type)
  }
  
  list(
    N = nrow(d),
    model_unweighted = m1,
    model_pswt       = m2,
    model_pswt_ctrl  = m3
  )
}

# ----------------------------
# A) Binary treatment outcomes
# ----------------------------
results_bin <- list()
for (o in names(outcomes)) {
  results_bin[[o]] <- fit_outcome_set(
    dat          = els_bin,
    outcome      = o,
    outcome_type = outcomes[[o]],
    treat        = "bi_sch_str",
    w_col        = "w_bin",
    controls     = controls
  )
}

# Print summaries (Binary)
cat("\n==============================\n")
cat("OUTCOME MODELS: Binary Treatment (bi_sch_str)\n")
cat("==============================\n")
for (o in names(results_bin)) {
  cat("\n--- Outcome:", o, "| N =", results_bin[[o]]$N, "---\n")
  cat("\n1) Outcome = Treatment\n");              print(summary(results_bin[[o]]$model_unweighted))
  cat("\n2) Outcome = Treatment (PSWT)\n");       print(summary(results_bin[[o]]$model_pswt))
  cat("\n3) Outcome = Treatment + Controls (PSWT)\n"); print(summary(results_bin[[o]]$model_pswt_ctrl))
}

# --------------------------------
# B) Multinomial treatment outcomes
# --------------------------------
results_mn <- list()
for (o in names(outcomes)) {
  results_mn[[o]] <- fit_outcome_set(
    dat          = els_mn,
    outcome      = o,
    outcome_type = outcomes[[o]],
    treat        = "qrt_sch_strict",
    w_col        = "w_mn",
    controls     = controls
  )
}

# Print summaries (MNPS)
cat("\n==============================\n")
cat("OUTCOME MODELS: Multinomial Treatment (qrt_sch_strict)\n")
cat("==============================\n")
for (o in names(results_mn)) {
  cat("\n--- Outcome:", o, "| N =", results_mn[[o]]$N, "---\n")
  cat("\n1) Outcome = Treatment\n");              print(summary(results_mn[[o]]$model_unweighted))
  cat("\n2) Outcome = Treatment (PSWT)\n");       print(summary(results_mn[[o]]$model_pswt))
  cat("\n3) Outcome = Treatment + Controls (PSWT)\n"); print(summary(results_mn[[o]]$model_pswt_ctrl))
}

# --------------------------------
# C) Continuous treatment outcomes
# --------------------------------
results_cont <- list()
for (o in names(outcomes)) {
  results_cont[[o]] <- fit_outcome_set(
    dat          = els_cont,
    outcome      = o,
    outcome_type = outcomes[[o]],
    treat        = "sch_strict",
    w_col        = "w_cont",
    controls     = controls
  )
}

# Print summaries (Continuous)
cat("\n==============================\n")
cat("OUTCOME MODELS: Continuous Treatment (sch_strict)\n")
cat("==============================\n")
for (o in names(results_cont)) {
  cat("\n--- Outcome:", o, "| N =", results_cont[[o]]$N, "---\n")
  cat("\n1) Outcome = Treatment\n");              print(summary(results_cont[[o]]$model_unweighted))
  cat("\n2) Outcome = Treatment (PSWT)\n");       print(summary(results_cont[[o]]$model_pswt))
  cat("\n3) Outcome = Treatment + Controls (PSWT)\n"); print(summary(results_cont[[o]]$model_pswt_ctrl))
}





