Propensity Score Weighting with TWANG (ELS 2002–2012)
Software & Packages
Language: R and Studio; (https://posit.co/download/rstudio-desktop/)
Core package: twang

Overview

This repository implements propensity score–based causal inference models using the twang package in R to estimate the effects of school security strictness on student academic and behavioral outcomes. The analysis uses data from the Education Longitudinal Study (ELS: 2002–2012) and applies binary, multinomial, and continuous treatment frameworks.

Goal: Estimate causal effects of school security strictness

The workflow is designed to mirror best practices in observational causal analysis:

- Define treatment intensity

- Estimate selection models

- Generate and diagnose weights

- Estimate weighted outcome models

- Compare results across specifications

Data Source:
Education Longitudinal Study of 2002–2012 (ELS)
National Center for Education Statistics (NCES)
Codebook:
https://nces.ed.gov/datalab/onlinecodebook/session/codebook/8028f151-fc03-40e2-afe3-5677e24a2c43

Modeling Strategy
1. Selection Models (TWANG)
Treatment Type	Model
  Binary	ps()
  Multinomial (quartiles)	mnps()
  Continuous	ps.cont()

Each model:
Estimates propensity scores using boosted trees
Optimizes covariate balance (not prediction)
Produces inverse probability weights

2. Diagnostics
Balance checks before vs. after weighting
Visualization of weights and balance statistics
Model stopping rules based on standardized effect sizes

3. Outcome Models
For each outcome set:
Outcome ~ Treatment
Outcome ~ Treatment (PS-weighted)
Outcome ~ Treatment + Controls (PS-weighted)

Estimation Framework
Primary analysis:
OLS regression
Survey-weighted models (svyglm)

Design: Observational, quasi-experimental



