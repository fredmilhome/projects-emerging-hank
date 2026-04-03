# 10_cleaning_wid.R
# Parse updated WID (World Inequality Database) download for Chile.
# Saves annual income share (sptinc_z) and wealth share (shweal_z) series.
#
# Input:  data/raw/WID/WID_Data_*.csv  (latest file, semicolon-delimited)
# Output: data/clean/wid_chile_annual.csv
#
# PULL ONLY — no transformations, no logs, no diffs.
# All transformation choices live in code/21_timeseries_processing.R.
#
# WID export format:
#   Row 1: Banner ("Downloaded from wid.world on ...")
#   Rows 2-8: Multi-line header (columns: Percentile; Year; sptinc_z_CL; shweal_z_CL)
#   Rows 9+:  Data rows (semicolon-delimited)

library(tidyverse)

# ---------------------------------------------------------------------------- #
# 1. Read
# ---------------------------------------------------------------------------- #

wid_file <- tail(sort(Sys.glob("data/raw/WID/WID_Data_*.csv")), 1)
cat("Reading:", wid_file, "\n")

# Skip the banner + multi-line header (8 rows total); assign column names manually
raw <- read_delim(
  wid_file,
  delim          = ";",
  skip           = 8,
  col_names      = c("percentile", "year", "sptinc", "shweal"),
  col_types      = cols(
    percentile = col_character(),
    year       = col_integer(),
    sptinc     = col_double(),
    shweal     = col_double()
  ),
  show_col_types = FALSE
)

cat(sprintf("Read %d rows\n", nrow(raw)))

# ---------------------------------------------------------------------------- #
# 2. Tidy to long format
# ---------------------------------------------------------------------------- #

wid_long <- raw |>
  pivot_longer(
    cols      = c(sptinc, shweal),
    names_to  = "variable",
    values_to = "share"
  ) |>
  filter(!is.na(share), !is.na(year)) |>
  arrange(variable, percentile, year)

cat(sprintf("After cleaning: %d rows\n", nrow(wid_long)))
cat("\nCoverage by variable and percentile:\n")
wid_long |>
  group_by(variable, percentile) |>
  summarise(
    n_years    = n(),
    year_first = min(year),
    year_last  = max(year),
    .groups    = "drop"
  ) |>
  print()

# ---------------------------------------------------------------------------- #
# 3. Save
# ---------------------------------------------------------------------------- #

out_path <- "data/clean/wid_chile_annual.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(wid_long, out_path)
cat(sprintf("\nWrote %d rows -> %s\n", nrow(wid_long), out_path))
cat("Columns: year, percentile, variable, share\n")
cat("Variables: sptinc (Pre-tax national income share), shweal (Net personal wealth share)\n")
cat("Percentiles: p90p100 (Top 10%), p99p100 (Top 1%)\n")
