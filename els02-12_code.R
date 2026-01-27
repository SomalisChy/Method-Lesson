# ----------------------------
# 1) Install packages (one-time)
# ----------------------------
install.packages("dplyr")     # tools for data cleaning (filter, mutate, select)
install.packages("haven")     # reads Stata/SPSS/SAS files
install.packages("survey")    # survey-weighted analysis (svyglm)
install.packages("twang")     # propensity score weighting with boosted trees
install.packages("ggplot2")   # plotting

# If TWANG functions are missing, install the development version from GitHub
install.packages("devtools")  
devtools::install_github("twangProject/twang")  # installs TWANG from GitHub

# ----------------------------
# 2) Load packages (every session)
# ----------------------------
library(dplyr)
library(haven)
library(twang)
library(survey)
library(ggplot2)

packageVersion("twang")       # tells you the version installed (debugging)
ls("package:twang")           # lists available functions in twang

# ----------------------------
# 3) Set working directory + load data
# ----------------------------
setwd("...")
# "setwd" tells R where to look for files by default

els <- read_dta(".../els_02_12.dta")
# read_dta loads a Stata .dta file into R as a data frame-like object

# ----------------------------
# 4) Define treatment item names (BYA38A ... BYA38P)
# ----------------------------
treat_vars <- paste0("BYA38", LETTERS[1:16])
# paste0 combines text to create variable names:
# LETTERS[1:16] = A B C ... P
# so treat_vars becomes: BYA38A, BYA38B, ..., BYA38P

# ----------------------------
# 5) Keep only the variables you need
# ----------------------------
els <- els %>%
  select(
    BYA38A, BYA38B, BYA38C, BYA38D, BYA38E, BYA38F,
    BYA38G, BYA38H, BYA38I, BYA38J, BYA38K, BYA38L,
    BYA38M, BYA38N, BYA38O, BYA38P,
    BYTXMSTD, BYS24E, BYS24F,
    BYF09A, BYF09B, BYF09C, BYF09D, BYF09E,
    BYP67, BYP68, BYSEX, BYRACE, BYSES1, BYURBAN, BYP46,
    BYP49, BYS67, BYFCOMP
  )
# select() drops everything else to make the dataset smaller and faster to work with

# ----------------------------
# 6) Clean treatment items: replace special negative codes with NA (missing)
# ----------------------------
els <- els %>%
  mutate(across(all_of(treat_vars), ~ ifelse(.x %in% c(-9, -8, -7, -4), NA, .x)))
# mutate() = create/modify variables
# across() = apply the same rule to many columns
# ifelse() = "if value is one of these codes, replace with NA"

# ----------------------------
# 7) Drop rows where ANY treatment component is missing
# ----------------------------
els <- els %>%
  filter(if_all(all_of(treat_vars), ~ !is.na(.x)))
# filter() keeps rows that match a condition
# if_all() means: ALL listed columns must be non-missing
# Result: you keep only students/schools with complete BYA38A-P

# ----------------------------
# 8) Create treatment index + quartiles + binary high-vs-low
# ----------------------------
els <- els %>%
  mutate(
    sch_strict = rowSums(across(all_of(treat_vars)), na.rm = TRUE),
    # rowSums adds across columns *within each row* (creates a 0–13-ish index)

    qrt_sch_strict = ntile(sch_strict, 4)
    # ntile(x,4) splits the index into 4 equal-sized groups (quartiles)
  ) %>%
  mutate(
    bi_sch_str = case_when(
      sch_strict == 0  ~ 0,   # lowest strictness group
      sch_strict == 13 ~ 1,   # highest strictness group
      TRUE ~ NA_real_         # everyone else becomes missing for this binary
    )
  )

summary(els$sch_strict)  # quick summary: min/median/mean/max etc.

# ----------------------------
# 9) Outcome recodes
# ----------------------------
els <- els %>%
  mutate(BYTXMSTD = ifelse(BYTXMSTD == -8, NA, BYTXMSTD)) %>%
  mutate(
    new_BYS24E = case_when(
      BYS24E == 1 ~ 0,
      BYS24E %in% 2:5 ~ 1,
      BYS24E %in% c(-9, -8, -7, -6, -4) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    new_BYS24F = case_when(
      BYS24F == 1 ~ 0,
      BYS24F %in% 2:5 ~ 1,
      BYS24F %in% c(-9, -8, -7, -6, -4) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(els$new_BYS24E, useNA = "ifany")  # frequency table, includes missing
table(els$new_BYS24F, useNA = "ifany")

# ----------------------------
# 10) Selection variables cleaning (neighborhood/area conditions)
# ----------------------------
byf_vars <- paste0("BYF09", LETTERS[1:5])   # BYF09A ... BYF09E
byp_vars <- c("BYP67", "BYP68")

els <- els %>%
  mutate(across(all_of(byf_vars), ~ ifelse(.x %in% c(-9, -8), NA, .x))) %>%
  mutate(across(all_of(byp_vars), ~ ifelse(.x %in% c(-9, -8, -7, -6, -4, -2, -1), NA, .x)))
# same idea: replace invalid codes with NA

# ----------------------------
# 11) Covariate recodes (race + family composition)
# ----------------------------
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

# ----------------------------
# 12) Define covariate list for TWANG (selection model predictors)
# ----------------------------
covars <- c(
  byf_vars, byp_vars,
  "BYSEX", "new_BYRACE", "BYSES1", "BYURBAN", "BYP46", "BYP49", "BYS67", "new_BYFCOMP"
)

# ----------------------------
# 13) Helper function: plot weight distributions
# ----------------------------
plot_weight_dist <- function(dat, w_col, treat_col, title_txt = "") {
  ggplot(dat, aes(x = .data[[w_col]])) +
    geom_histogram(bins = 60) +
    facet_wrap(vars(.data[[treat_col]]), scales = "free_y") +
    labs(title = title_txt, x = "Weight", y = "Count")
}
# This makes a reusable plot function so you don’t repeat code

# ============================================================
# 14) TWANG MODEL 1: Binary PS (ps) for bi_sch_str
# ============================================================

els_bin <- els %>%
  filter(!is.na(bi_sch_str)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  mutate(bi_sch_str = as.numeric(bi_sch_str)) %>%  # TWANG wants numeric 0/1
  filter(bi_sch_str %in% c(0, 1)) %>%
  as.data.frame()
# TWANG expects a plain data.frame (not tibble), so we convert

set.seed(123)  # makes results reproducible (same weights every run)

ps_bin <- twang::ps(
  formula = as.formula(paste("bi_sch_str ~", paste(covars, collapse = " + "))),
  data    = els_bin,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.01,
  stop.method = c("es.mean", "ks.max"),
  estimand = "ATE",
  verbose = FALSE
)
# twang::ps fits a boosted-tree propensity score model
# stop.method tells TWANG how to pick the best iteration (balance criteria)

w_bin <- get.weights(ps_bin, stop.method = "es.mean")
els_bin$w_bin <- as.numeric(w_bin)

plot(ps_bin, plots = 1)  # propensity score distributions by group
plot(ps_bin, plots = 2)  # balance plots (effect size / KS)

print(plot_weight_dist(els_bin, "w_bin", "bi_sch_str",
  "Binary PS weights by treatment group (bi_sch_str)"
))

# ============================================================
# 15) TWANG MODEL 2: Multinomial PS (mnps) for quartiles
# ============================================================

els_mn <- els %>%
  filter(!is.na(qrt_sch_strict)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  mutate(qrt_sch_strict = factor(qrt_sch_strict, levels = 1:4)) %>%
  as.data.frame()

set.seed(123)

mnps_q <- mnps(
  formula = as.formula(paste("qrt_sch_strict ~", paste(covars, collapse = " + "))),
  data    = els_mn,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.01,
  stop.method = c("es.mean", "ks.max"),
  estimand = "ATE",
  verbose = FALSE
)

w_mn <- get.weights(mnps_q, stop.method = "es.mean")
els_mn$w_mn <- as.numeric(w_mn)

plot(mnps_q, plots = 1)
plot(mnps_q, plots = 2)

print(plot_weight_dist(els_mn, "w_mn", "qrt_sch_strict",
  "MNPS weights by treatment group (quartiles)"
))

# ============================================================
# 16) TWANG MODEL 3: Continuous treatment (ps.cont) for sch_strict
# ============================================================

els_cont <- els %>%
  filter(!is.na(sch_strict)) %>%
  filter(if_all(all_of(covars), ~ !is.na(.x))) %>%
  as.data.frame()

set.seed(123)

ps_cont_fit <- ps.cont(
  formula = as.formula(paste("sch_strict ~", paste(covars, collapse = " + "))),
  data    = els_cont,
  n.trees = 5000,
  interaction.depth = 2,
  shrinkage = 0.01,
  stop.method = c("es.mean", "ks.max"),
  verbose = FALSE
)

w_cont <- get.weights(ps_cont_fit, stop.method = "es.mean")
els_cont$w_cont <- as.numeric(w_cont)

plot(ps_cont_fit, plots = 1)
plot(ps_cont_fit, plots = 2)

# Optional: balance tables
bal_bin <- bal.table(ps_bin)
bal_mn  <- bal.table(mnps_q)
bal_con <- bal.table(ps_cont_fit)

str(bal_bin, max.level = 1)  # quick look at object structure
