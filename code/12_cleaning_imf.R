# 12_cleaning_imf.R
# Parse haver_data.xlsx (LATAM sheet) → clean quarterly CSVs by country.
# Outputs:
#   data/clean/imf_latam_long.csv   — tidy long: date, country, series_id, series_name, value
#   data/clean/imf_brazil_wide.csv  — wide quarterly: date + named series columns
#   data/clean/imf_chile_wide.csv
#   data/clean/imf_mexico_wide.csv
#
# Source: Haver Analytics / IMF data exported via FRED-style workbook.
# No growth-rate transformations here — see code/21_timeseries_processing.R.

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)

# ---------------------------------------------------------------------------- #
# Series lookup — code prefix (before @) → country + clean name
# ---------------------------------------------------------------------------- #

SERIES_LOOKUP <- tribble(
  ~code_prefix,   ~country,  ~series_name,
  "N223RD",       "Brazil",  "interest_rate",   # Selic target rate, EOP, % p.a.
  "N223NCC",      "Brazil",  "consumption",     # Final consumption, NSA
  "S223NGPC",     "Brazil",  "gdp",             # GDP at market prices, SA
  "H223PCA",      "Brazil",  "cpi",             # IPCA, SA, Dec1993=100
  "H223PCXR",     "Brazil",  "core_cpi",        # Core IPCA ex-regular price & fuel, SA
  "C228RD",       "Chile",   "interest_rate",   # Monetary policy rate, EOP, %
  "S228NGPC",     "Chile",   "gdp",             # GDP, SA, bil. Ch.2018.CLP
  "N228NCC",      "Chile",   "consumption",     # Total consumption, NSA
  "H228PC",       "Chile",   "cpi",             # CPI, SA, 2023=100
  "H228NFBC",     "Chile",   "investment",      # Gross capital formation, SA
  "H228PCXG",     "Chile",   "core_cpi",        # CPI ex-food & energy, SA (ends Q3 2023)
  "N273RD",       "Mexico",  "interest_rate",   # Overnight interbank target, EOP, %
  "S273NGPC",     "Mexico",  "gdp",             # GDP at market prices, SA
  "N273NCC",      "Mexico",  "consumption",     # Total consumption, NSA
  "H273PC",       "Mexico",  "cpi",             # CPI, SA, Jul2018=100
  "H273PJXQ",     "Mexico",  "core_cpi"         # Core CPI, SA, Jul2018=100
)

# ---------------------------------------------------------------------------- #
# 1. Read raw sheet (col_names = FALSE to capture header row as data)
# ---------------------------------------------------------------------------- #

raw <- read_excel(
  here("data", "raw", "haver_data.xlsx"),
  sheet     = "LATAM",
  col_names = FALSE,
  col_types = "text"   # read everything as text; we parse dates and numerics manually
)

cat("Raw sheet dimensions:", nrow(raw), "rows x", ncol(raw), "cols\n")

# Row 1 = series code headers; rows 2-13 = metadata; rows 14+ = data
header_row <- as.character(raw[1, ])
data_raw   <- raw[-1, ]   # drop header row; rows 1-12 = metadata, 13+ = data

# ---------------------------------------------------------------------------- #
# 2. Parse series codes from header
# ---------------------------------------------------------------------------- #

# Extract base code prefix (strip @EMERGELA suffix and any ...N readxl dedup suffix)
series_codes <- header_row
series_codes[is.na(series_codes)] <- ""

# Flag: duplicate columns have "..." appended by readxl (e.g. "N228NCC@EMERGELA...14")
is_duplicate <- grepl("\\.\\.\\.", series_codes)

# Flag: Colombia series (country code 233)
is_colombia <- grepl("233", series_codes)

# Flag: Excel artifact column (.excel_last)
is_artifact <- grepl("excel_last", series_codes, ignore.case = TRUE) |
               series_codes == ""

# Column index to keep: not duplicate, not Colombia, not artifact, not col 1 (date col)
col_keep <- !is_duplicate & !is_colombia & !is_artifact
col_keep[1] <- FALSE   # col 1 is the date/row-id column; handled separately

cat(
  "Columns: total =", ncol(raw),
  "| kept =", sum(col_keep),
  "| dropped (dupes) =", sum(is_duplicate),
  "| dropped (Colombia) =", sum(is_colombia),
  "| dropped (artifact/blank) =", sum(is_artifact), "\n"
)

# ---------------------------------------------------------------------------- #
# 3. Extract metadata block (rows 1–12 of data_raw)
# ---------------------------------------------------------------------------- #

meta_labels <- c(".DESC", ".T1", ".TN", ".LSOURCE", ".AGG", ".DTLM",
                 ".FRQ", ".DATA_TYPE", ".MAG", ".GRP", ".GRPDESC", ".GEO")

meta_raw <- data_raw[1:12, ]

# Build a small reference tibble: series_code x metadata_field
meta_long <- meta_raw |>
  mutate(field = meta_labels) |>
  select(field, everything()) |>
  select(-1) |>   # drop the date-id column (col 1 of data_raw, now col 2 after mutate)
  pivot_longer(
    cols      = -field,
    names_to  = "col_idx",
    values_to = "value"
  )

# We won't use meta_long further in this script but it's available for debugging

# ---------------------------------------------------------------------------- #
# 4. Extract data rows and parse YYYYQ dates
# ---------------------------------------------------------------------------- #

data_rows <- data_raw[13:nrow(data_raw), ]   # rows 13+ are actual observations

# Col 1 of data_rows = YYYYQ date codes (e.g. "20041")
date_codes <- as.character(data_rows[[1]])

# Keep only rows that look like YYYYQ (5-digit numeric string)
is_data_row <- grepl("^[0-9]{5}$", date_codes)
date_codes  <- date_codes[is_data_row]
data_rows   <- data_rows[is_data_row, ]

parse_yyyyq <- function(code) {
  yr <- as.integer(substr(code, 1, 4))
  q  <- as.integer(substr(code, 5, 5))
  as.Date(paste0(yr, "-", (q - 1L) * 3L + 1L, "-01"))
}

dates <- as.Date(sapply(date_codes, parse_yyyyq), origin = "1970-01-01")

cat("Data rows:", length(dates), "| Date range:", format(range(dates)), "\n")

# ---------------------------------------------------------------------------- #
# 5. Build tidy long data frame
# ---------------------------------------------------------------------------- #

# Extract the data columns we want to keep
data_matrix <- data_rows[, col_keep]
kept_codes  <- series_codes[col_keep]

# Strip @EMERGELA suffix to get bare code prefix for lookup
code_prefixes <- sub("@.*", "", kept_codes)

long <- bind_cols(
  tibble(date = dates),
  as_tibble(data_matrix, .name_repair = ~ code_prefixes)
) |>
  pivot_longer(
    cols      = -date,
    names_to  = "code_prefix",
    values_to = "value"
  ) |>
  mutate(value = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(value)) |>
  left_join(SERIES_LOOKUP, by = "code_prefix") |>
  # Retain series_id (full code with @EMERGELA) for provenance
  left_join(
    tibble(code_prefix = code_prefixes,
           series_id   = kept_codes),
    by = "code_prefix"
  ) |>
  select(date, country, series_id, series_name, value) |>
  arrange(country, series_name, date)

# Warn about any series in lookup that produced zero rows
missing_series <- anti_join(SERIES_LOOKUP, long, by = c("country", "series_name"))
if (nrow(missing_series) > 0) {
  message("WARNING: The following expected series produced no data rows:")
  print(missing_series)
}

cat("Long table:", nrow(long), "obs,", n_distinct(long$series_id), "series\n")
cat("Countries:", paste(unique(long$country), collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------- #
# 6. Wide tables per country
# ---------------------------------------------------------------------------- #

make_wide <- function(country_name) {
  long |>
    filter(country == country_name) |>
    select(date, series_name, value) |>
    pivot_wider(names_from = series_name, values_from = value) |>
    arrange(date)
}

wide_brazil <- make_wide("Brazil")
wide_chile  <- make_wide("Chile")
wide_mexico <- make_wide("Mexico")

# ---------------------------------------------------------------------------- #
# 7. Save
# ---------------------------------------------------------------------------- #

dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)

write_csv(long,         here("data", "clean", "imf_latam_long.csv"))
write_csv(wide_brazil,  here("data", "clean", "imf_brazil_wide.csv"))
write_csv(wide_chile,   here("data", "clean", "imf_chile_wide.csv"))
write_csv(wide_mexico,  here("data", "clean", "imf_mexico_wide.csv"))

cat("Saved data/clean/imf_latam_long.csv\n")
cat("Saved data/clean/imf_brazil_wide.csv\n")
cat("Saved data/clean/imf_chile_wide.csv\n")
cat("Saved data/clean/imf_mexico_wide.csv\n\n")

# ---------------------------------------------------------------------------- #
# 8. Quick check — last 8 quarters for each country
# ---------------------------------------------------------------------------- #

cat("=== Brazil (last 8 quarters) ===\n")
print(tail(wide_brazil, 8))

cat("\n=== Chile (last 8 quarters) ===\n")
print(tail(wide_chile, 8))

cat("\n=== Mexico (last 8 quarters) ===\n")
print(tail(wide_mexico, 8))
