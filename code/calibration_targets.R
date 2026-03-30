# calibration_targets.R
# Computes and prints all externally calibrated parameters and calibration
# targets for the Chile HANK model, formatted after BBL (2024, AER) Table 1.
#
# Inputs (read-only):
#   data/clean/efh_households.csv    — produced by code/cleaning.R
#   data/clean/wid_wealth_shares.csv — produced by code/cleaning.R
#   data/clean/bcch_quarterly.csv    — produced by code/11_cleaning_bcch.R (optional)
#
# Run: Rscript code/calibration_targets.R   (from project root)
#
# Author:  Fred Milhome
# Project: hank-emerging

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ── Paths ─────────────────────────────────────────────────────────────────────

efh_file  <- here("data", "clean", "efh_households.csv")
wid_file  <- here("data", "clean", "wid_wealth_shares.csv")
bcch_file <- here("data", "clean", "bcch_quarterly.csv")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Income process — Huneeus & Repetto (2004), Table 5
# ══════════════════════════════════════════════════════════════════════════════
#
# Annual AR(1)-plus-i.i.d.: persistence rho_annual = 0.931,
# permanent innovation variance sigma2_upsilon = 0.00395 (uncorrected).
#
# Quarterly persistence: rho_h = 0.931^(1/4) ≈ 0.982, matching BBL's 0.980.
#
# Variance: one-order-of-magnitude correction for synthetic-cohort attenuation
# (Huneeus & Repetto Section 3.2, Table 7) → sigma2_upsilon_annual ≈ 0.0395.
# Accumulation formula: sigma2_annual = sigma2_q * (1 + rho_q^2 + rho_q^4 + rho_q^6)
# ≈ sigma2_q * 3.789  →  sigma2_h_q ≈ 0.01043  →  sigma_h ≈ 0.10.
#
# BBL US baseline: rho_h = 0.980, sigma_h = 0.120.
# Chile: rho_h = 0.982, sigma_h = 0.100 (≈ 85% of BBL's sigma_h).

RHO_H  <- 0.931^(1/4)          # 0.9820
SIGMA_H <- sqrt(0.0395 / 3.789) # 0.1022 → rounded to 0.100 in table

# ══════════════════════════════════════════════════════════════════════════════
# 2. EFH: liquid / illiquid ratio and borrower share
# ══════════════════════════════════════════════════════════════════════════════
#
# Following Weiss (2023, §2.3.6.1) and Kaplan, Violante & Weidner (2014):
#
#   net_illiquid = act_vp + act_otp − d_vp
#     Primary home + other real estate, net of mortgage.
#     Vehicles EXCLUDED ("I abstract from car wealth" — Weiss §2.3.6.1).
#     Note: cleaning.R's illiquid_assets includes act_vehic; we recompute here.
#
#   net_liquid = act_ahcta + act_fin − d_nhip
#     Checking/sight + financial assets, net of non-mortgage debt.
#     Caveat: d_nhip covers ALL non-mortgage debt (auto, consumer, student
#     loans), not credit cards only. This overstates liquid-side debt and
#     will push the borrower share above the true credit-card-only measure.
#
#   Sample restriction (Weiss ratio.png): drop rows where net_liquid > 0
#   AND net_wealth < 0 (~0.7% of observations).
#
#   Multiple imputations: EFH has 5 imputations per household (imp 1–5).
#   Compute each statistic within imputation (survey-weighted by factor),
#   then average across imputations (Rubin's point-estimate rule).

compute_efh_targets <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(
      "\nEFH clean data not found:\n  ", filepath,
      "\nRun code/cleaning.R first to produce it.\n"
    )
  }

  df <- read_csv(filepath, show_col_types = FALSE) |>
    mutate(
      net_illiquid = act_vp + act_otp - d_vp,
      net_liquid   = act_ahcta + act_fin - d_nhip,
      net_wealth   = act_toth - d_toth
    ) |>
    filter(!(net_liquid > 0 & net_wealth < 0))   # Weiss sample restriction

  per_imp <- df |>
    group_by(imp) |>
    summarise(
      borrower_share  = weighted.mean(net_liquid < 0, w = factor, na.rm = TRUE),
      agg_liquid      = sum(factor * net_liquid,   na.rm = TRUE),
      agg_illiquid    = sum(factor * net_illiquid, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(liq_illiq_ratio = agg_liquid / agg_illiquid)

  list(
    borrower_share  = mean(per_imp$borrower_share),
    liq_illiq_ratio = mean(per_imp$liq_illiq_ratio),
    n_imp           = n_distinct(df$imp)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. WID: top-10% wealth share, Chile 2000–2019
# ══════════════════════════════════════════════════════════════════════════════

compute_wid_target <- function(filepath) {
  read_csv(filepath, show_col_types = FALSE) |>
    filter(country == "Chile", percentile == "p90p100",
           year >= 2000, year <= 2019) |>
    summarise(mean_share = mean(share, na.rm = TRUE)) |>
    pull(mean_share)
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. BCCh: G/Y (if bcch_quarterly.csv exists)
# ══════════════════════════════════════════════════════════════════════════════

compute_bcch_targets <- function(filepath) {
  if (!file.exists(filepath)) return(list(gy = NA_real_))

  df <- read_csv(filepath, show_col_types = FALSE)

  wide <- df |>
    filter(series %in% c("gdp", "gov_spending")) |>
    pivot_wider(id_cols = date, names_from = series, values_from = value)

  gy <- if (all(c("gdp", "gov_spending") %in% names(wide))) {
    mean(wide$gov_spending / wide$gdp, na.rm = TRUE)
  } else {
    NA_real_
  }

  list(gy = gy)
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. Print table
# ══════════════════════════════════════════════════════════════════════════════

W   <- 103   # total line width
DIV <- strrep("\u2500", W)
HDV <- strrep("\u2550", W)

# Column format:  par(9) value(8)   desc(27) target(36) source
ROW_FMT <- "  %-9s %8s   %-27s %-36s %s\n"

trow <- function(par, value, desc, target, source = "") {
  cat(sprintf(ROW_FMT, par, value, desc, target, source))
}

tsec <- function(name) {
  cat(sprintf("  %s\n", name))
}

fmt_val <- function(x, digits = 3) {
  if (is.na(x)) return("TBD")
  formatC(x, digits = digits, format = "f", flag = " ")
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("TBD")
  sprintf("%.1f%%", x * 100)
}

print_table <- function(efh, wid_share, bcch) {

  # ── Header ──────────────────────────────────────────────────────────────────
  cat("\n", HDV, "\n", sep = "")
  hdr <- sprintf("  TABLE 1 \u2014 CALIBRATION  (quarterly frequency)")
  cat(sprintf("%-*s%s\n", W - 5, hdr, "Chile"))
  cat(HDV, "\n", sep = "")
  cat(sprintf(ROW_FMT, "Par.", "Value", "Description", "Target", "Data source"))
  cat(DIV, "\n", sep = "")

  # ── Households: income process ───────────────────────────────────────────────
  tsec("Households: income process")
  trow("\u03c1_h",   fmt_val(RHO_H),   "Persistence labor income",
       "Huneeus & Repetto (2004), Table 5",    "BCCh WP 2004")
  trow("\u03c3_h",   fmt_val(SIGMA_H), "Std. labor income",
       "Huneeus & Repetto (2004), corr.",      "BCCh WP 2004")
  trow("\u03b9",     "0.063",          "Trans. prob. E to W",
       "Guvenen, Kaplan, Song (2014)",          "[TBD Chile equiv.]")
  trow("\u03b6",     "[calib]",        "Trans. prob. W to E",
       sprintf("Top 10%% wealth share: %s", fmt_pct(wid_share)),
       "WID Chile 2000\u20132019")
  cat(DIV, "\n", sep = "")

  # ── Households: financial frictions ─────────────────────────────────────────
  tsec("Households: financial frictions")
  trow("\u03bb",     "[calib]",        "Portfolio adj. prob.",
       sprintf("Liq./illiq. ratio: %s", fmt_val(efh$liq_illiq_ratio, 3)),
       "EFH 2024")
  trow("R\u0305",    "[calib]",        "Borrowing penalty",
       sprintf("Borrower share: %s", fmt_pct(efh$borrower_share)),
       "EFH 2024")
  trow("q\u0305\u1d35\u1d35/Y", "TBD", "Value of profit shares",
       "Gov. debt/output, B/Y = ?",             "BCCh [TBD]")
  cat(DIV, "\n", sep = "")

  # ── Households: preferences ──────────────────────────────────────────────────
  tsec("Households: preferences")
  trow("\u03b2",     "[calib]",        "Discount factor",
       "Capital/output, K/Y = ?",               "BCCh [TBD]")
  trow("\u03be",     "4.000",          "Relative risk aversion",
       "Kaplan and Violante (2014)",            "Literature")
  trow("\u03b3",     "2.000",          "Inv. Frisch elasticity",
       "Chetty et al. (2011)",                 "Literature")
  cat(DIV, "\n", sep = "")

  # ── Firms ────────────────────────────────────────────────────────────────────
  tsec("Firms")
  trow("\u03b1",     "TBD",            "Share of labor",
       "Avg. labor income share",               "BCCh [TBD]")
  trow("\u03b4\u2080", "0.018",        "Depreciation rate",
       "Bayer, Born, Luetticke (2023)",        "Literature")
  trow("\u03b7\u0305", "11.000",       "Elasticity of substitution",
       "Born and Pfeifer (2014)",              "Literature")
  trow("\u03b6\u0305", "11.000",       "Elasticity of substitution",
       "Born and Pfeifer (2014)",              "Literature")
  cat(DIV, "\n", sep = "")

  # ── Government ───────────────────────────────────────────────────────────────
  tsec("Government")
  gy_target <- if (!is.na(bcch$gy)) {
    sprintf("Gov. consumption, G/Y = %s", fmt_pct(bcch$gy))
  } else {
    "Gov. consumption, G/Y = ?"
  }
  trow("\u03c4\u0305\u1d38",  "TBD",   "Tax rate level",
       gy_target,                                "BCCh")
  trow("\u03c4\u0305\u1d3e",  "0.000", "Tax progressivity",
       "Fixed at 0 \u2014 no Chile series",     "\u2014")
  trow("R\u0305\u1d47",       "1.000", "Gross nominal rate",
       "Growth \u2248 interest rate",           "\u2014")
  trow("\u03c0\u0305",         "1.000", "Gross inflation",
       "Indexation, w.l.o.g.",                 "\u2014")
  cat(HDV, "\n", sep = "")

  # ── Notes ────────────────────────────────────────────────────────────────────
  cat(sprintf(
    "  Notes: [calib] = internally calibrated to match target. TBD = target\n"
  ))
  cat(sprintf(
    "  not yet sourced. EFH liquid debt uses d_nhip (all non-mortgage debt)\n"
  ))
  cat(sprintf(
    "  as proxy for credit card debt; borrower share may be overstated.\n"
  ))
  cat(sprintf(
    "  WID average 2000-2019. EFH: %d imputations averaged (Rubin's rules).\n",
    efh$n_imp
  ))
  if (is.na(bcch$gy)) {
    cat("  BCCh data absent: run code/11_cleaning_bcch.R to populate G/Y.\n")
  }
  cat("\n")
}

# ── Main ──────────────────────────────────────────────────────────────────────

efh       <- compute_efh_targets(efh_file)
wid_share <- compute_wid_target(wid_file)
bcch      <- compute_bcch_targets(bcch_file)

print_table(efh, wid_share, bcch)
