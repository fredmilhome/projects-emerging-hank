# cleaning.R — Data Cleaning for WID and EFH Data
# =================================================
#
# Processes EFH raw data sources for HANK model calibration:
#
#
#   2. EFH — Encuesta Financiera de Hogares 2024 (imputed)
#      Input:  data/raw/EFH/Base imputada EFH 2024.dta
#      Output: data/clean/efh_households.csv
#
# Author:  Fred Milhome
# Project: hank-emerging
# Created: 2026

library(here)
library(haven)
library(readr)
library(dplyr)
library(tidyr)

# ── Paths ─────────────────────────────────────────────────────────────────────

clean_dir <- here("data", "clean")

efh_raw_file    <- here("data", "raw", "EFH", "Base imputada EFH 2024.dta")
efh_output_file <- here(clean_dir, "efh_households.csv")

# ══════════════════════════════════════════════════════════════════════════════
# 2. EFH — Household balance sheets
# ══════════════════════════════════════════════════════════════════════════════
#
# Variables selected for HANK calibration:
#
#   Identifiers / weights
#     id       — household identifier
#     imp      — imputation number (1–5; use with expansion weight)
#     factor   — household expansion weight
#
#   Income
#     ylabh    — monthly labor income
#     ypenh    — monthly pension income
#     ytoth    — total monthly household income (incl. imputed rent)
#     ytotef   — total effective monthly income (excl. imputed rent)
#
#   Assets
#     act_vp   — primary home value          } illiquid
#     act_otp  — other real estate value     }
#     act_vehic — vehicle value              }
#     act_ahcta — checking/sight account     } liquid
#     act_fin   — financial assets           }
#     act_toth  — total assets
#
#   Debt
#     d_toth   — total household debt
#     d_vp     — mortgage debt (primary home)
#     d_nhip   — total non-mortgage debt

read_efh <- function(filepath) {
  df <- read_dta(filepath)

  df |>
    select(
      id, imp, factor,
      ylabh, ypenh, ytoth, ytotef,
      act_vp, act_otp, act_vehic,
      act_ahcta, act_fin,
      act_toth,
      d_toth, d_vp, d_nhip
    ) |>
    # Following Weiss thesis cited in BBL, we abstract from vehicle assets
    # and define borrowers based on net liquid assets. To get aggregate portfolio liquidity,
    # households with positive liquid assets and non positive total should be deleted in the
    # calibration targets, but we keep this in the calibration targets calculation script.
    mutate(
      across(everything(), as.numeric),
      net_illiquid_assets  = act_vp + act_otp - d_vp,
      net_liquid_assets    = act_ahcta + act_fin - d_nhip,
      net_total_assets = net_illiquid_assets + net_liquid_assets,
      portfolio_liquidity = net_liquid_assets / net_total_assets,
      borrower = net_liquid_assets < 0
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════════════════

main <- function() {
  dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

  # EFH
  message("\n── EFH ──────────────────────────────────────────────────")
  message("Reading: ", efh_raw_file)
  efh <- read_efh(efh_raw_file)
  write_csv(efh, efh_output_file)
  message("Written: ", efh_output_file, " (", nrow(efh), " rows, ",
          n_distinct(efh$id), " households × 5 imputations)")
}

main()
