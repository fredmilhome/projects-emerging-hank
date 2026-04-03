# 14_cleaning_series_calib.R
# Assembles annual calibration-target series for Chile RANK/HANK.
#
# Inputs:
#   data/raw/chile_manual_pulls/Series stock de capital consumo de capital
#     fijo. Período 1985-2024 Referencia 2018.xlsx
#       Sheet 1: Net capital stock  (Cuadro 1 — Stock de capital neto)
#       Sheet 2: Capital consumption (Cuadro 2 — Consumo de capital fijo)
#       Both sheets: skip 8 rows; col 1 = year, col 5 = Total (precios constantes 2018)
#       Units: billions of 2018 CLP (miles de millones de pesos de 2018)
#   data/raw/bcch_raw/<latest>/bcch_main.rds  — quarterly BCCh macro series
#
# Outputs:
#   data/clean/calib_series_chile.csv
#   data/clean/calib_series_chile.rds
#   Columns:
#     year                — integer
#     capital_stock       — net capital stock, billions of 2018 CLP
#     capital_consumption — capital consumption,  billions of 2018 CLP
#     delta_annual        — capital_consumption / capital_stock  (annual rate)
#     gdp_pib             — annual GDP = sum of 4 quarterly flows (billions 2018 CLP)
#     gdp_ddi             — annual internal demand = pib − net_exports (same units)
#     gov_spending        — annual gov. consumption (cog) = sum of 4 quarters
#
# Run after: 11_cleaning_bcch.R
# Run before: calibration_targets.R
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

# ── Paths ─────────────────────────────────────────────────────────────────────

xl_path <- here("data", "raw", "chile_manual_pulls",
  "Series stock de capital consumo de capital fijo. Período 1985-2024 Referencia 2018.xlsx")

if (!file.exists(xl_path)) stop("Capital stock Excel not found:\n  ", xl_path)

bcch_folder <- tail(sort(Sys.glob(here("data", "raw", "bcch_raw", "*"))), 1)
if (length(bcch_folder) == 0) stop("No BCCh data found. Run 11_cleaning_bcch.R first.")
cat(sprintf("Using BCCh pull: %s\n", bcch_folder))

out_csv <- here("data", "clean", "calib_series_chile.csv")
out_rds <- here("data", "clean", "calib_series_chile.rds")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Capital stock and capital consumption from Excel
# ══════════════════════════════════════════════════════════════════════════════
#
# Sheet structure (identical for sheets 1 and 2):
#   Rows 1-8: title, subtitle, units, blank, column headers (two rows), blanks
#   Row 9 onward: data rows — col 1 = year, col 5 = Total (precios constantes 2018)
#   Year column may contain footnote markers: "2023 (3)", "2024 (4)" — stripped by
#   as.integer() coercion (non-numeric characters are ignored, giving NA for
#   purely non-numeric rows which then filter out).
#
# "Precios constantes": cols 2-5 (Edificación, Resto edificación, Maquinaria, Total)
# "Precios corrientes": cols 7-10 (same breakdown) — not used here.

read_capital_sheet <- function(sheet_num, col_name) {
  raw <- read_excel(xl_path, sheet = sheet_num, skip = 8, col_names = FALSE)
  raw |>
    transmute(
      year  = suppressWarnings(as.integer(`...1`)),   # strips " (3)", " (4)" markers
      value = as.numeric(`...5`)                       # Total, precios constantes 2018
    ) |>
    filter(!is.na(year), !is.na(value), year >= 1985) |>
    rename(!!col_name := value)
}

cat("Reading capital stock (Sheet 1)...\n")
cap_stock <- read_capital_sheet(1, "capital_stock")

cat("Reading capital consumption (Sheet 2)...\n")
cap_consump <- read_capital_sheet(2, "capital_consumption")

capital <- inner_join(cap_stock, cap_consump, by = "year") |>
  mutate(delta_annual = capital_consumption / capital_stock)

cat(sprintf("Capital data: %d rows, years %d-%d\n",
            nrow(capital), min(capital$year), max(capital$year)))

# ══════════════════════════════════════════════════════════════════════════════
# 2. Annual BCCh series (sum quarterly flows per year)
# ══════════════════════════════════════════════════════════════════════════════
#
# GDP and government spending are flow variables — annual total = sum of 4 quarters.
# Internal demand (ddi) computed as:
#   ddi series if present in BCCh pull, else reconstructed as pib - xbs + ibs.
# GDP deflator (deflator) is a price index; capital stock Excel is already in
# real 2018 CLP, so no additional deflation needed for annual BCCh flows —
# the BCCh quarterly national accounts series are also in 2018 CLP (SA).

bcch <- readRDS(file.path(bcch_folder, "bcch_main.rds"))

annual_bcch <- bcch |>
  mutate(year = year(date)) |>
  filter(series %in% c("pib", "ddi", "xbs", "ibs", "cog")) |>
  group_by(year, series) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = series, values_from = value) |>
  mutate(
    # Prefer ddi directly; fall back to pib - xbs + ibs if ddi missing
    gdp_ddi = if ("ddi" %in% names(pick(everything()))) {
      coalesce(ddi, pib - coalesce(xbs, 0) + coalesce(ibs, 0))
    } else {
      pib - coalesce(xbs, 0) + coalesce(ibs, 0)
    }
  ) |>
  rename(
    gdp_pib      = pib,
    gov_spending  = cog
  ) |>
  select(year, gdp_pib, gdp_ddi, gov_spending)

cat(sprintf("BCCh annual: %d rows, years %d-%d\n",
            nrow(annual_bcch), min(annual_bcch$year), max(annual_bcch$year)))

# ══════════════════════════════════════════════════════════════════════════════
# 4. Borrower share and portfolio liquidity targets
# ══════════════════════════════════════════════════════════════════════════════
#
# Source: EFH 2024 imputed dataset (efh_households.csv, from 13_cleaning_EFH.R)
# The file has 5 imputations per household (imp 1–5), each with an expansion
# weight (factor). Following multiple-imputation practice, each target is
# computed separately per imputation (weighted) and then averaged across
# imputations (Rubin's rules — point estimate = simple mean over M imputations).
#
# WARNING (from 13_cleaning_EFH.R): for the portfolio-liquidity target, households
# with POSITIVE net_liquid_assets AND NON-POSITIVE net_total_assets must be
# dropped before computing the aggregate ratio. These observations arise when
# negative illiquid wealth (net of mortgage) makes total assets zero or negative
# while liquid assets are still positive, producing an economically meaningless
# ratio (positive numerator, non-positive denominator). The drop rate is
# reported below.

efh_path <- here("data", "clean", "efh_households.csv")
if (!file.exists(efh_path)) stop("EFH clean file not found. Run 13_cleaning_EFH.R first.")

efh <- read_csv(efh_path, show_col_types = FALSE)
n_hh <- n_distinct(efh$id)
cat(sprintf("\nEFH loaded: %d households × 5 imputations = %d obs\n", n_hh, nrow(efh)))

# ── Borrower share (Rbar target) ──────────────────────────────────────────────
# Fraction of households with negative net liquid assets, weighted by factor.
# All observations are used — no drops for this target.

borrower_by_imp <- efh |>
  group_by(imp) |>
  summarise(
    borrower_share = weighted.mean(as.numeric(borrower), w = factor, na.rm = TRUE),
    .groups = "drop"
  )

borrower_share <- mean(borrower_by_imp$borrower_share)

cat(sprintf("\nBorrower share (Rbar target):\n"))
cat(sprintf("  Per imputation:  %s\n",
            paste(sprintf("%.4f", borrower_by_imp$borrower_share), collapse = ", ")))
cat(sprintf("  MI average:      %.4f  (US BBL baseline: 0.022)\n", borrower_share))

# ── Portfolio liquidity (lambda target) ───────────────────────────────────────
# Aggregate portfolio liquidity = weighted mean of net_liquid / net_total.
# Drop rule: remove households where net_liquid_assets > 0 & net_total_assets <= 0.

efh_valid <- efh |>
  filter(!(net_liquid_assets > 0 & net_total_assets <= 0))

# Report drop rates using imp == 1 (so each household counted once)
efh_imp1       <- filter(efh,       imp == 1)
efh_imp1_valid <- filter(efh_valid, imp == 1)

n_hh_total   <- n_distinct(efh_imp1$id)
n_hh_dropped <- n_hh_total - n_distinct(efh_imp1_valid$id)
wt_total     <- sum(efh_imp1$factor,                                              na.rm = TRUE)
wt_dropped   <- sum(efh_imp1$factor[!(efh_imp1$id %in% efh_imp1_valid$id)],      na.rm = TRUE)

cat(sprintf("\nPortfolio liquidity — observation filter (positive liquid, non-positive total):\n"))
cat(sprintf("  Households dropped:       %d / %d  (%.1f%% unweighted)\n",
            n_hh_dropped, n_hh_total, 100 * n_hh_dropped / n_hh_total))
cat(sprintf("  Population weight dropped: %.1f%%\n", 100 * wt_dropped / wt_total))

liq_by_imp <- efh_valid |>
  filter(!is.na(portfolio_liquidity)) |>
  group_by(imp) |>
  summarise(
    portfolio_liq = weighted.mean(portfolio_liquidity, w = factor, na.rm = TRUE),
    .groups = "drop"
  )

portfolio_liquidity_target <- mean(liq_by_imp$portfolio_liq)

cat(sprintf("\nPortfolio liquidity (lambda target):\n"))
cat(sprintf("  Per imputation:  %s\n",
            paste(sprintf("%.4f", liq_by_imp$portfolio_liq), collapse = ", ")))
cat(sprintf("  MI average:      %.4f  (US BBL baseline: 0.065)\n", portfolio_liquidity_target))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Join and write
# ══════════════════════════════════════════════════════════════════════════════

out <- left_join(capital, annual_bcch, by = "year") |>
  arrange(year)

write_csv(out, out_csv)
saveRDS(out, out_rds)

cat(sprintf("\ncalib_series_chile: %d rows x %d cols  [%d-%d]\n",
            nrow(out), ncol(out), min(out$year), max(out$year)))

cat("\nSummary of key columns:\n")
print(summary(out[, c("capital_stock", "capital_consumption", "delta_annual",
                       "gdp_pib", "gdp_ddi", "gov_spending")]))

cat(sprintf("\nWrote:\n  %s\n  %s\n", out_csv, out_rds))
