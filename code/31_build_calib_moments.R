# 31_build_calib_moments.R
# Assembles calib_series_chile.csv and computes / prints all externally
# calibrated parameters and calibration targets for the Chile HANK model,
# formatted after BBL (2024, AER) Table 1.
#
# Inputs:
#   data/raw/chile_manual_pulls/
#     Series stock de capital consumo de capital fijo. Período 1985-2024 Referencia 2018.xlsx
#       Sheet 1: Net capital stock  (col 1 = year, col 5 = Total, precios constantes 2018)
#       Sheet 2: Capital consumption (same layout)
#   data/raw/bcch_raw/<latest>/bcch_main.rds   (from 11_cleaning_bcch.R)
#   data/clean/efh_households.csv              (from 13_cleaning_EFH.R)
#   data/clean/wid_chile_annual.csv            (from 10_cleaning_wid.R)
#
# Outputs:
#   data/clean/calib_series_chile.csv / .rds  — annual capital + macro panel
#   Console: BBL-style calibration table
#
# Run after:  10_cleaning_wid.R + 11_cleaning_bcch.R + 13_cleaning_EFH.R
# Run before: 21_timeseries_processing.R (reads calib_series_chile.csv)
#
# Author:  Fred Milhome
# Project: hank-emerging

suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
})

source(here("code", "config.R"))   # USE_INTERNAL_DEMAND, PERIOD_START, PERIOD_END

# ── Paths ─────────────────────────────────────────────────────────────────────

xl_path <- here("data", "raw", "chile_manual_pulls",
  "Series stock de capital consumo de capital fijo. Período 1985-2024 Referencia 2018.xlsx")
if (!file.exists(xl_path)) stop("Capital stock Excel not found:\n  ", xl_path)

bcch_folder <- tail(sort(Sys.glob(here("data", "raw", "bcch_raw", "*"))), 1)
if (length(bcch_folder) == 0) stop("No BCCh data found. Run 11_cleaning_bcch.R first.")
cat(sprintf("Using BCCh pull: %s\n", bcch_folder))

efh_file <- here("data", "clean", "efh_households.csv")
wid_file <- here("data", "clean", "wid_chile_annual.csv")

out_csv  <- here("data", "clean", "calib_series_chile.csv")
out_rds  <- here("data", "clean", "calib_series_chile.rds")

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
# Accumulation formula: sigma2_annual = sigma2_q × (1 + ρ² + ρ⁴ + ρ⁶)
#   ≈ sigma2_q × 3.789  →  sigma2_h_q ≈ 0.01043  →  sigma_h ≈ 0.10.
#
# BBL US baseline: rho_h = 0.980, sigma_h = 0.120.
# Chile: rho_h = 0.982, sigma_h = 0.100 (≈ 85% of BBL's sigma_h).

RHO_H   <- 0.931^(1/4)           # 0.9820
SIGMA_H <- sqrt(0.0395 / 3.789)  # 0.1022 → rounded to 0.100 in table

# ══════════════════════════════════════════════════════════════════════════════
# 2. Capital stock and capital consumption from BCCh Excel
# ══════════════════════════════════════════════════════════════════════════════
#
# Sheet structure (identical for sheets 1 and 2):
#   Rows 1-8: title, subtitle, units, blank, column headers (two rows), blank
#   Row 9+:   data rows — col 1 = year, col 5 = Total (precios constantes 2018)
#   Year column may contain footnote markers "2023 (3)" — stripped by as.integer().
#
# Units: billions of 2018 CLP (miles de millones de pesos de 2018).

read_capital_sheet <- function(sheet_num, col_name) {
  raw <- read_excel(xl_path, sheet = sheet_num, skip = 8, col_names = FALSE)
  raw |>
    transmute(
      year  = suppressWarnings(as.integer(`...1`)),
      value = as.numeric(`...5`)
    ) |>
    filter(!is.na(year), !is.na(value), year >= 1985) |>
    rename(!!col_name := value)
}

cat("Reading capital stock (Sheet 1)...\n")
cap_stock   <- read_capital_sheet(1, "capital_stock")

cat("Reading capital consumption (Sheet 2)...\n")
cap_consump <- read_capital_sheet(2, "capital_consumption")

capital <- inner_join(cap_stock, cap_consump, by = "year") |>
  mutate(delta_annual = capital_consumption / capital_stock)

cat(sprintf("Capital data: %d rows, years %d–%d\n",
            nrow(capital), min(capital$year), max(capital$year)))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Annual BCCh series (sum quarterly flows per year)
# ══════════════════════════════════════════════════════════════════════════════
#
# GDP and government spending are flow variables — annual total = sum of 4 quarters.
# Internal demand (ddi) computed as:
#   ddi series if present in BCCh pull, else reconstructed as pib − xbs + ibs.
# BCCh quarterly national accounts are in 2018 CLP (SA, not annualised).

bcch <- readRDS(file.path(bcch_folder, "bcch_main.rds"))

annual_bcch <- bcch |>
  mutate(year = year(date)) |>
  filter(series %in% c("pib", "ddi", "xbs", "ibs", "cog")) |>
  group_by(year, series) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = series, values_from = value) |>
  mutate(
    gdp_ddi = if ("ddi" %in% names(pick(everything()))) {
      coalesce(ddi, pib - coalesce(xbs, 0) + coalesce(ibs, 0))
    } else {
      pib - coalesce(xbs, 0) + coalesce(ibs, 0)
    }
  ) |>
  rename(gdp_pib = pib, gov_spending = cog) |>
  select(year, gdp_pib, gdp_ddi, gov_spending)

cat(sprintf("BCCh annual: %d rows, years %d–%d\n",
            nrow(annual_bcch), min(annual_bcch$year), max(annual_bcch$year)))

# ── Join and write calib_series_chile ─────────────────────────────────────────

calib_series <- left_join(capital, annual_bcch, by = "year") |>
  arrange(year)

write_csv(calib_series, out_csv)
saveRDS(calib_series,   out_rds)
cat(sprintf("Wrote calib_series_chile: %d rows × %d cols  [%d–%d]\n",
            nrow(calib_series), ncol(calib_series),
            min(calib_series$year), max(calib_series$year)))

# ══════════════════════════════════════════════════════════════════════════════
# 4. EFH: liquid / illiquid ratio and borrower share
# ══════════════════════════════════════════════════════════════════════════════
#
# Following Weiss (2023, §2.3.6.1) and Kaplan, Violante & Weidner (2014):
#
#   net_illiquid = act_vp + act_otp − d_vp
#     Primary home + other real estate, net of mortgage.
#     Vehicles EXCLUDED ("I abstract from car wealth" — Weiss §2.3.6.1).
#
#   net_liquid = act_ahcta + act_fin − d_nhip
#     Checking/sight + financial assets, net of non-mortgage debt.
#     Caveat: d_nhip covers ALL non-mortgage debt, not credit cards only.
#     This overstates liquid-side debt and will push borrower share above
#     the true credit-card-only measure.
#
#   Sample restriction (Weiss): drop rows where net_liquid > 0 AND
#   net_wealth (= act_toth − d_toth) < 0. This removes observations with
#   positive liquid assets but negative total net wealth — an economically
#   inconsistent combination that inflates the liq/illiq ratio.
#
#   Multiple imputations: EFH has 5 imputations per household (imp 1–5).
#   Each target is computed within each imputation (survey-weighted by factor),
#   then averaged across imputations (Rubin's point-estimate rule).

if (!file.exists(efh_file)) {
  stop("EFH clean data not found:\n  ", efh_file,
       "\nRun code/13_cleaning_EFH.R first.")
}

efh <- read_csv(efh_file, show_col_types = FALSE) |>
  mutate(
    net_illiquid = act_vp + act_otp - d_vp,
    net_liquid   = act_ahcta + act_fin - d_nhip,
    net_wealth   = act_toth - d_toth          # comprehensive total, for Weiss filter
  )

# Report drop rate using imp == 1 (count each household once)
efh_imp1 <- filter(efh, imp == 1)
n_hh_total   <- nrow(efh_imp1)
efh_imp1_kept <- filter(efh_imp1, !(net_liquid > 0 & net_wealth < 0))
n_hh_dropped <- n_hh_total - nrow(efh_imp1_kept)
wt_total     <- sum(efh_imp1$factor,       na.rm = TRUE)
wt_dropped   <- sum(efh_imp1$factor[efh_imp1$id %in%
                    setdiff(efh_imp1$id, efh_imp1_kept$id)], na.rm = TRUE)

cat(sprintf(
  "\nEFH sample restriction (net_liquid > 0 & net_wealth < 0, Weiss):\n"
))
cat(sprintf("  Households dropped: %d / %d  (%.1f%% unweighted, %.1f%% by population weight)\n",
            n_hh_dropped, n_hh_total,
            100 * n_hh_dropped / n_hh_total,
            100 * wt_dropped   / wt_total))

efh_clean <- efh |>
  filter(!(net_liquid > 0 & net_wealth < 0))

efh_per_imp <- efh_clean |>
  group_by(imp) |>
  summarise(
    borrower_share  = weighted.mean(net_liquid < 0, w = factor, na.rm = TRUE),
    agg_liquid      = sum(factor * net_liquid,   na.rm = TRUE),
    agg_illiquid    = sum(factor * net_illiquid, na.rm = TRUE),
    .groups         = "drop"
  ) |>
  mutate(liq_illiq_ratio = agg_liquid / agg_illiquid)

efh_targets <- list(
  borrower_share  = mean(efh_per_imp$borrower_share),
  liq_illiq_ratio = mean(efh_per_imp$liq_illiq_ratio),
  n_imp           = n_distinct(efh_clean$imp)
)

cat(sprintf("  Borrower share    — per imp: %s  | MI avg: %.4f  (US BBL: 0.022)\n",
            paste(sprintf("%.4f", efh_per_imp$borrower_share), collapse = ", "),
            efh_targets$borrower_share))
cat(sprintf("  Liq./illiq. ratio — per imp: %s  | MI avg: %.4f  (US BBL: 0.065)\n",
            paste(sprintf("%.4f", efh_per_imp$liq_illiq_ratio), collapse = ", "),
            efh_targets$liq_illiq_ratio))

# ══════════════════════════════════════════════════════════════════════════════
# 5. WID: top-10% wealth share, Chile 2000–2019
# ══════════════════════════════════════════════════════════════════════════════
#
# Variable: shweal_z — net personal wealth share.
# Percentile: p90p100 — top 10%.
# Period: 2000–2019 (pre-pandemic, longest clean run).
# Source: wid_chile_annual.csv (from 10_cleaning_wid.R).

wid_top10_share <- if (file.exists(wid_file)) {
  read_csv(wid_file, show_col_types = FALSE) |>
    filter(variable == "shweal", percentile == "p90p100",
           year >= 2000, year <= 2019) |>
    summarise(m = mean(share, na.rm = TRUE)) |>
    pull(m)
} else {
  message("WID file not found: ", wid_file, "\nRun 10_cleaning_wid.R first.")
  NA_real_
}

cat(sprintf("\nWID top-10%% wealth share (Chile 2000–2019): %.4f  (US BBL: ~0.67)\n",
            wid_top10_share))

# ══════════════════════════════════════════════════════════════════════════════
# 6. Derived macro targets from calib_series_chile
# ══════════════════════════════════════════════════════════════════════════════
#
# K/Y_q  = 4 × capital_stock / gdp_col  (quarterly ratio = annual × 4)
# δ₀     = mean(delta_annual) / 4       (quarterly depreciation rate)
# G/Y    = gov_spending / gdp_col       (ratio of annual flows — scale-invariant)
#
# All series filtered to [PERIOD_START, PERIOD_END] from config.R (NULL = all years).
# GDP concept follows USE_INTERNAL_DEMAND (TRUE → gdp_ddi, FALSE → gdp_pib).

cs <- calib_series
if (!is.null(PERIOD_START)) cs <- filter(cs, year >= PERIOD_START)
if (!is.null(PERIOD_END))   cs <- filter(cs, year <= PERIOD_END)

gdp_col <- if (USE_INTERNAL_DEMAND) cs$gdp_ddi else cs$gdp_pib

macro_targets <- list(
  ky_q   = mean(4 * cs$capital_stock / gdp_col, na.rm = TRUE),
  delta0 = mean(cs$delta_annual,                na.rm = TRUE) / 4,
  gy     = mean(cs$gov_spending / gdp_col,      na.rm = TRUE)
)

period_label <- sprintf("%s–%s",
                        if (is.null(PERIOD_START)) min(cs$year) else PERIOD_START,
                        if (is.null(PERIOD_END))   max(cs$year) else PERIOD_END)
gdp_label    <- if (USE_INTERNAL_DEMAND) "internal demand (ddi)" else "GDP (pib)"

cat(sprintf("\nMacro targets (%s, %s):\n", period_label, gdp_label))
cat(sprintf("  K/Y_q  = %.3f  (US BBL RANK: 11.44, HANK: ~10)\n", macro_targets$ky_q))
cat(sprintf("  δ₀     = %.4f  (annual = %.1f%%)\n",
            macro_targets$delta0, 100 * macro_targets$delta0 * 4))
cat(sprintf("  G/Y    = %.4f  (%.1f%%)\n",
            macro_targets$gy, 100 * macro_targets$gy))

# ══════════════════════════════════════════════════════════════════════════════
# 7. Print calibration table
# ══════════════════════════════════════════════════════════════════════════════

W   <- 103
DIV <- strrep("\u2500", W)
HDV <- strrep("\u2550", W)

ROW_FMT <- "  %-9s %8s   %-27s %-36s %s\n"

trow <- function(par, value, desc, target, source = "") {
  cat(sprintf(ROW_FMT, par, value, desc, target, source))
}

tsec <- function(name) cat(sprintf("  %s\n", name))

fmt_val <- function(x, digits = 3) {
  if (is.na(x)) return("TBD")
  formatC(x, digits = digits, format = "f", flag = " ")
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("TBD")
  sprintf("%.1f%%", x * 100)
}

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
     sprintf("Top 10%% wealth share: %s", fmt_pct(wid_top10_share)),
     "WID Chile 2000\u20132019")
cat(DIV, "\n", sep = "")

# ── Households: financial frictions ─────────────────────────────────────────
tsec("Households: financial frictions")
trow("\u03bb",     "[calib]",        "Portfolio adj. prob.",
     sprintf("Liq./illiq. ratio: %s", fmt_val(efh_targets$liq_illiq_ratio, 3)),
     "EFH 2024")
trow("R\u0305",    "[calib]",        "Borrowing penalty",
     sprintf("Borrower share: %s", fmt_pct(efh_targets$borrower_share)),
     "EFH 2024")
trow("q\u0305\u1d35\u1d35/Y", "TBD", "Value of profit shares",
     "Gov. debt/output, B/Y = ?",             "BCCh [TBD]")
cat(DIV, "\n", sep = "")

# ── Households: preferences ──────────────────────────────────────────────────
tsec("Households: preferences")
trow("\u03b2",     "[calib]",        "Discount factor",
     sprintf("Capital/output, K/Y_q = %s", fmt_val(macro_targets$ky_q, 2)),
     "BCCh capital stock (yearly)")
trow("\u03be",     "4.000",          "Relative risk aversion",
     "Kaplan and Violante (2014)",            "Literature")
trow("\u03b3",     "2.000",          "Inv. Frisch elasticity",
     "Chetty et al. (2011)",                 "Literature")
cat(DIV, "\n", sep = "")

# ── Firms ────────────────────────────────────────────────────────────────────
tsec("Firms")
trow("\u03b1",     "TBD",            "Share of labor",
     "Avg. labor income share",               "BCCh [TBD — no wage-bill series]")
trow("\u03b4\u2080", fmt_val(macro_targets$delta0, 4), "Depreciation rate",
     sprintf("Annual depr. = %s", fmt_pct(macro_targets$delta0 * 4)),
     "BCCh cap. consumption (yearly)")
trow("\u03b7\u0305", "11.000",       "Elasticity of substitution",
     "Born and Pfeifer (2014)",              "Literature")
trow("\u03b6\u0305", "11.000",       "Elasticity of substitution",
     "Born and Pfeifer (2014)",              "Literature")
cat(DIV, "\n", sep = "")

# ── Government ───────────────────────────────────────────────────────────────
tsec("Government")
trow("\u03c4\u0305\u1d38",  "TBD",   "Tax rate level",
     sprintf("Gov. consumption, G/Y = %s", fmt_pct(macro_targets$gy)),
     sprintf("BCCh (%s)", gdp_label))
trow("\u03c4\u0305\u1d3e",  "0.000", "Tax progressivity",
     "Fixed at 0 \u2014 no Chile series",     "\u2014")
trow("R\u0305\u1d47",       "1.000", "Gross nominal rate",
     "Growth \u2248 interest rate",           "\u2014")
trow("\u03c0\u0305",         "1.000", "Gross inflation",
     "Indexation, w.l.o.g.",                 "\u2014")
cat(HDV, "\n", sep = "")

cat(sprintf(
  "  Notes: [calib] = internally calibrated to match target. TBD = target not yet sourced.\n"
))
cat(sprintf(
  "  EFH liquid debt uses d_nhip (all non-mortgage debt) as proxy for credit card debt;\n"
))
cat(sprintf(
  "  borrower share may be overstated. Sample restriction drops %d/%d households (%.1f%% of pop.).\n",
  n_hh_dropped, n_hh_total, 100 * wt_dropped / wt_total
))
cat(sprintf(
  "  EFH: %d imputations averaged (Rubin's rules). WID: shweal p90p100, Chile 2000–2019.\n",
  efh_targets$n_imp
))
cat(sprintf(
  "  α (labor share) and B/Y (govt debt/output) require additional BCCh series not yet pulled.\n"
))
cat("\n")
