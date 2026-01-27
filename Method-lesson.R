# ************************************************************
# Replication Lesson (Stata -> R) | Using .RData
# ************************************************************
install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")
install.packages("survey")
install.packages("twang")
install.packages("ggplot2")
install.packages("devtools")
install_github("twangProject/twang")

library(devtools)
library(dplyr)
library(tidyr)
library(haven)
library(twang)
library(survey)
library(ggplot2)
packageVersion("twang")
ls("package:twang")


# Set working directory (Windows path: use forward slashes or double backslashes)
setwd("C:/Users/chy/Box/replication project_jj/data_stata")

# --- Load data -------------------------------------------------
els <- read_dta("C:/Users/chy/Box/replication project_jj/data_stata/els_02_12.dta")


# --- Bookmark #1: Clean and define treatment variable ----------
treat_vars <- paste0("BYA38", LETTERS[1:16])  # BYA38A ... BYA38P

els <- els %>%
  select(BYA38A, BYA38B,	BYA38C,	BYA38D,	BYA38E,	BYA38F,
         BYA38G,	BYA38H,	BYA38I,	BYA38J,	BYA38K,	BYA38L,	
         BYA38M,	BYA38N,	BYA38O,	BYA38P,	BYTXMSTD,	
         BYS24E,	BYS24F,	BYF09A,	BYF09B,	BYF09C,	BYF09D,	BYF09E,	
         BYP67,	BYP68,	BYSEX,	BYRACE,	BYSES1,	BYURBAN,	BYP46,	
         BYP49,	BYS67,	BYFCOMP
) %>%
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

# --- Visualize weighting (built-in Twang plots) ---
plot(ps_cont_fit, plots = 1)  # treatment/score-related diagnostic plot(s)
plot(ps_cont_fit, plots = 2)  # balance plot(s)

# --- Visualize weighting (custom): weight distribution overall + by binned sch_strict ---
els_cont <- els_cont %>%
  mutate(sch_strict_bin = cut(sch_strict, breaks = quantile(sch_strict, probs = seq(0, 1, 0.25)), include.lowest = TRUE))

# Overall
ggplot(els_cont, aes(x = w_cont)) +
  geom_histogram(bins = 60) +
  labs(title = "Continuous PS weights (overall)", x = "Weight", y = "Count")

# By binned treatment
print(plot_weight_dist(
  dat = els_cont,
  w_col = "w_cont",
  treat_col = "sch_strict_bin",
  title_txt = "Continuous PS weights by sch_strict quartile-bins"
))

# ============================================================
# (Optional) Export balance tables to inspect quickly
# ============================================================

bal_bin <- bal.table(ps_bin)
bal_mn  <- bal.table(mnps_q)
bal_con <- bal.table(ps_cont_fit)

# View a quick peek (these objects can be large)
str(bal_bin, max.level = 1)
str(bal_mn,  max.level = 1)
str(bal_con, max.level = 1)






