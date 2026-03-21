# cleaning.R — Data Cleaning for WID and EFH Data
# =================================================
#
# Processes two raw data sources for HANK model calibration:
#
#   1. WID — World Inequality Database wealth share series
#      Input:  data/raw/WID/WID_Data_26022026-181954.csv
#      Output: data/clean/wid_wealth_shares.csv
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

wid_raw_file    <- here("data", "raw", "WID", "WID_Data_26022026-181954.csv")
wid_output_file <- here(clean_dir, "wid_wealth_shares.csv")

efh_raw_file    <- here("data", "raw", "EFH", "Base imputada EFH 2024.dta")
efh_output_file <- here(clean_dir, "efh_households.csv")

# ══════════════════════════════════════════════════════════════════════════════
# 1. WID — Wealth share series
# ══════════════════════════════════════════════════════════════════════════════

percentile_labels <- c(
  p90p100 = "Top 10%",
  p99p100 = "Top 1%",
  p0p50   = "Bottom 50%"
)

# The WID export has:
#   - Row 1: "Downloaded from wid.world …" (skip)
#   - Row 2: header with embedded newlines inside quotes
#   - Row 3+: data
# We skip the banner and rename columns positionally.

read_wid_csv <- function(filepath) {
  df <- read_delim(
    filepath,
    delim          = ";",
    skip           = 1,
    name_repair    = "minimal",
    show_col_types = FALSE
  )
  names(df) <- c("percentile", "year", "Mexico", "Brazil", "Chile")
  df
}

clean_wid <- function(df) {
  df |>
    pivot_longer(
      cols      = c(Mexico, Brazil, Chile),
      names_to  = "country",
      values_to = "share"
    ) |>
    mutate(
      percentile_label = percentile_labels[percentile],
      year             = as.integer(year),
      share            = as.numeric(share)
    ) |>
    filter(!is.na(share)) |>
    arrange(country, percentile, year) |>
    select(year, percentile, percentile_label, country, share)
}

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
    mutate(
      across(everything(), as.numeric),
      net_wealth       = act_toth - d_toth,
      illiquid_assets  = act_vp + act_otp + act_vehic,
      liquid_assets    = act_ahcta + act_fin
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════════════════

main <- function() {
  dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

  # WID
  message("── WID ──────────────────────────────────────────────────")
  message("Reading: ", wid_raw_file)
  wid <- read_wid_csv(wid_raw_file) |> clean_wid()
  write_csv(wid, wid_output_file)
  message("Written: ", wid_output_file, " (", nrow(wid), " rows)")

  # EFH
  message("\n── EFH ──────────────────────────────────────────────────")
  message("Reading: ", efh_raw_file)
  efh <- read_efh(efh_raw_file)
  write_csv(efh, efh_output_file)
  message("Written: ", efh_output_file, " (", nrow(efh), " rows, ",
          n_distinct(efh$id), " households × 5 imputations)")
}

main()
