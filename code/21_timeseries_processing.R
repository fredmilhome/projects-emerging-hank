# 21_timeseries_processing.R
# Assemble BBL-format estimation data for Chile (RANK model).
#
# Inputs:
#   data/raw/bcch_raw/<latest>/bcch_main.rds  — BCCh macro series
#   data/clean/wid_chile_annual.csv           — WID wealth and income shares
#
# Output:
#   chile-RANK-estimation/Data/bbl_data_chile.csv
#
# BBL format: 12 columns, no date column, NaN as text for missing.
# Column names: Ygrowth Igrowth Cgrowth N wgrowth RB pi sigma2 w90share I90share Tgrowth tauprog
#
# TRANSFORM ONLY — no API calls, no raw file reads other than the two inputs above.
# Transformations applied here:
#   - Deflation (nominal / deflator)
#   - Per-capita (/ population, pchip-interpolated to quarterly)
#   - Log-differences for growth rates
#   - Monthly → quarterly averaging
#   - Annual WID shares assigned to Q4 of each year; all other quarters = NaN
#     (Kalman filter handles infrequent observations via missing-obs mechanism)
#   - Demeaning: subtract sample mean of finite values per column
#
# SAAR note: BCCh national accounts are SA but NOT annualized (x4). For growth
# rates (log-diffs) this distinction is irrelevant.
#
# Kalman filter demeaning: the BASEforHANK Kalman filter does NOT demean.
# Data must arrive as deviations from steady state -- we subtract sample means here.

library(here)
library(tidyverse)
library(zoo)

source(here("code", "config.R"))   # USE_INTERNAL_DEMAND, PERIOD_START, PERIOD_END

# ---------------------------------------------------------------------------- #
# 0. Configuration
# ---------------------------------------------------------------------------- #

BBL_COLS <- c("Ygrowth", "Igrowth", "Cgrowth", "N", "wgrowth", "RB", "pi",
              "sigma2", "w90share", "I90share", "Tgrowth", "tauprog")

OUT_PATH <- "chile-RANK-estimation/Data/bbl_data_chile.csv"

# ---------------------------------------------------------------------------- #
# 1. Load inputs
# ---------------------------------------------------------------------------- #

bcch_folder <- tail(sort(Sys.glob("data/raw/bcch_raw/*")), 1)
if (length(bcch_folder) == 0) stop("No BCCh data found. Run 11_cleaning_bcch.R first.")
cat(sprintf("Using BCCh pull: %s\n", bcch_folder))

bcch <- readRDS(file.path(bcch_folder, "bcch_main.rds"))

wid_path <- "data/clean/wid_chile_annual.csv"
if (!file.exists(wid_path)) stop("WID data not found. Run 10_cleaning_wid.R first.")
wid <- read_csv(wid_path, show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 2. Helpers
# ---------------------------------------------------------------------------- #

# Monthly series -> quarterly average. Returns tibble(yq, value_q).
monthly_to_quarterly <- function(df) {
  df |>
    mutate(yq = as.yearqtr(date)) |>
    group_by(yq) |>
    summarise(value_q = mean(value, na.rm = TRUE), .groups = "drop")
}

# Annual data -> quarterly vector via pchip interpolation.
# Places each annual observation at Q1 of its year for interpolation,
# then returns values aligned to qtr_grid.
# Uses zoo::na.spline(method = "monoH.FC") -- monotone cubic Hermite (pchip).
pchip_quarterly <- function(annual_df, qtr_grid) {
  year_as_qtr <- as.yearqtr(paste0(annual_df$year, " Q1"))
  all_qtrs    <- sort(as.numeric(unique(c(as.numeric(qtr_grid),
                                          as.numeric(year_as_qtr)))))
  all_qtrs    <- as.yearqtr(all_qtrs)
  values      <- rep(NA_real_, length(all_qtrs))
  idx         <- match(as.numeric(year_as_qtr), as.numeric(all_qtrs))
  values[idx] <- annual_df$value
  z_interp    <- zoo::na.spline(zoo(values, all_qtrs), method = "monoH.FC",
                                na.rm = FALSE)
  as.numeric(coredata(z_interp)[match(as.numeric(qtr_grid),
                                       as.numeric(time(z_interp)))])
}

# Wrap a column expression: return NaN vector on any error; log status.
safe_col <- function(expr, n, name) {
  result <- tryCatch(force(expr), error = function(e) {
    message(sprintf("  WARN  %-14s  failed: %s", name, e$message))
    rep(NaN, n)
  })
  n_fin <- sum(is.finite(result))
  if (n_fin == 0) {
    message(sprintf("  INFO  %-14s  all-NaN", name))
  } else {
    message(sprintf("  OK    %-14s  %d finite obs", name, n_fin))
  }
  result
}

# ---------------------------------------------------------------------------- #
# 3. Quarterly backbone
# ---------------------------------------------------------------------------- #

quarterly_dates <- bcch |>
  filter(series %in% c("pib", "deflator", "cpr", "fbkf", "cog", "vax")) |>
  pull(date) |> unique() |> sort()

if (length(quarterly_dates) == 0)
  stop("No quarterly BCCh series found -- check bcch_main.rds content.")

qtr_grid <- as.yearqtr(quarterly_dates)
n        <- length(qtr_grid)
cat(sprintf("Quarterly backbone: %d periods  %s - %s\n\n",
            n, format(min(qtr_grid)), format(max(qtr_grid))))

# Look up a named BCCh series; return numeric vector aligned to qtr_grid.
bcch_q <- function(series_name) {
  vals <- bcch |>
    filter(series == series_name) |>
    transmute(yq = as.yearqtr(date), value)
  tibble(yq = qtr_grid) |>
    left_join(vals, by = "yq") |>
    pull(value)
}

# ---------------------------------------------------------------------------- #
# 4. Deflator and population (needed by several columns)
# ---------------------------------------------------------------------------- #

deflator_idx <- bcch_q("deflator") / 100   # 2018 quarterly average approx 1.0

pop_annual <- bcch |>
  filter(series == "population") |>
  transmute(year = as.integer(format(date, "%Y")), value)

if (nrow(pop_annual) == 0)
  warning("Population series missing -- per-capita adjustment will fail.")

pop_q <- pchip_quarterly(pop_annual, qtr_grid)

# log of real per-capita level of a nominal quarterly series
log_rpc  <- function(x) log(x) - log(deflator_idx) - log(pop_q)
first_diff <- function(x) c(NaN, diff(x))

# ---------------------------------------------------------------------------- #
# 5. Build BBL columns
# ---------------------------------------------------------------------------- #

cat("Building BBL columns...\n")

Ygrowth <- safe_col({
  if (USE_INTERNAL_DEMAND) {
    # Y = internal demand (excl. net exports): prefer ddi series directly;
    # reconstruct as pib - xbs + ibs if ddi is missing.
    ddi_vals <- bcch_q("ddi")
    if (any(is.finite(ddi_vals))) {
      first_diff(log_rpc(ddi_vals))
    } else {
      message("  INFO  Ygrowth       ddi missing, reconstructing as pib - xbs + ibs")
      first_diff(log_rpc(bcch_q("pib") - bcch_q("xbs") + bcch_q("ibs")))
    }
  } else {
    # Y = full GDP (pib)
    first_diff(log_rpc(bcch_q("pib")))
  }
}, n, "Ygrowth")

Igrowth <- safe_col(first_diff(log_rpc(bcch_q("fbkf") + bcch_q("vax"))), n, "Igrowth")
Cgrowth <- safe_col(first_diff(log_rpc(bcch_q("cpr"))),  n, "Cgrowth")
Tgrowth <- safe_col(first_diff(log_rpc(bcch_q("cog"))),  n, "Tgrowth")

pi <- safe_col(first_diff(log(deflator_idx)), n, "pi")

N <- safe_col({
  hrs_q <- monthly_to_quarterly(filter(bcch, series == "hours_worked"))
  tibble(yq = qtr_grid) |>
    left_join(hrs_q, by = "yq") |>
    mutate(v = log(value_q)) |>
    pull(v)
}, n, "N")

wgrowth <- safe_col({
  w_q <- monthly_to_quarterly(filter(bcch, series == "wage_index"))
  tibble(yq = qtr_grid) |>
    left_join(w_q, by = "yq") |>
    mutate(v = c(NaN, diff(log(value_q)))) |>
    pull(v)
}, n, "wgrowth")

RB <- safe_col({
  # interbank_rate_1d is monthly; average to quarterly
  rate_q <- monthly_to_quarterly(filter(bcch, series == "interbank_rate_1d"))
  tibble(yq = qtr_grid) |>
    left_join(rate_q, by = "yq") |>
    pull(value_q)
}, n, "RB")

# ---- WID inequality shares -----------------------------------------------
# Annual observations placed at Q4 of each reference year; all other quarters NaN.
# The Kalman filter handles infrequent observation via the missing-obs mechanism
# (filter_smoother.jl:53-57): zeroes Data_slice and H_slice rows for NaN obs.
# DO NOT fill or interpolate these series across quarters.

assign_annual_to_q4 <- function(wid_df, pct, var_name) {
  annual <- wid_df |>
    filter(percentile == pct, variable == var_name) |>
    transmute(yq = as.yearqtr(paste0(year, " Q4")), value = share)
  tibble(yq = qtr_grid) |>
    left_join(annual, by = "yq") |>
    pull(value)
}

w90share <- safe_col(assign_annual_to_q4(wid, "p90p100", "shweal"), n, "w90share")
I90share <- safe_col(assign_annual_to_q4(wid, "p90p100", "sptinc"), n, "I90share")

# Not available as a time series for Chile -- all NaN.
# Income risk identified via structural Sshock only.
sigma2  <- rep(NaN, n)
tauprog <- rep(NaN, n)

# ---------------------------------------------------------------------------- #
# 6. Assemble wide tibble
# ---------------------------------------------------------------------------- #

bbl <- tibble(
  date    = as.Date(qtr_grid),
  Ygrowth, Igrowth, Cgrowth, N, wgrowth, RB, pi,
  sigma2, w90share, I90share, Tgrowth, tauprog
)

# Drop the first period created by first-difference operators.
bbl <- bbl |> slice(-1)

# ---------------------------------------------------------------------------- #
# 7. Demean (subtract sample mean of finite values per column)
# ---------------------------------------------------------------------------- #
# The BASEforHANK Kalman filter receives data as deviations from steady state
# and does NOT demean internally (confirmed in filter_smoother.jl).

bbl <- bbl |> mutate(across(all_of(BBL_COLS), \(x) {
  mu <- mean(x, na.rm = TRUE)
  if (is.finite(mu)) x - mu else x
}))

# ---------------------------------------------------------------------------- #
# 8. Coverage table
# ---------------------------------------------------------------------------- #

cat("\n=== BBL column coverage ===\n")
bbl |>
  select(all_of(BBL_COLS)) |>
  summarise(across(everything(), \(x) sum(is.finite(x)))) |>
  pivot_longer(everything(), names_to = "col", values_to = "n_obs") |>
  mutate(status = ifelse(n_obs == 0, "ALL-NaN", sprintf("%d obs", n_obs))) |>
  print()

cat(sprintf("\nSample period: %s - %s  (%d quarters)\n",
            format(min(bbl$date)), format(max(bbl$date)), nrow(bbl)))

# ---------------------------------------------------------------------------- #
# 9. Write output
# ---------------------------------------------------------------------------- #
# Format: no date column, 12 BBL columns, missing = "NaN" string.
# Julia reads with: CSV.File(path; missingstring = "NaN")

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)

bbl |>
  select(all_of(BBL_COLS)) |>
  write.csv(OUT_PATH, row.names = FALSE, na = "NaN")

cat(sprintf("\nWrote %d rows x %d cols -> %s\n",
            nrow(bbl), length(BBL_COLS), OUT_PATH))
