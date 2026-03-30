# 11_cleaning_bcch.R
# Pull macro time series from the Banco Central de Chile (BCCh) REST API.
# Outputs: data/clean/bcch_quarterly.csv, data/clean/bcch_monthly.csv
#
# API docs: https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx
# Register for credentials at: https://si3.bcentral.cl/siete/

library(rjson)
library(dplyr)
library(tidyr)
library(lubridate)

# ---------------------------------------------------------------------------- #
# CREDENTIALS — paste yours here
# ---------------------------------------------------------------------------- #

BCCH_USER <- "fred.coosta@hotmail.com"
BCCH_PASS <- "I$beg1mtBU"

# ---------------------------------------------------------------------------- #
# Series catalogue — BCCh series IDs for HANK estimation
# ---------------------------------------------------------------------------- #
# To find additional series IDs use bcch_search() below, or browse:
# https://si3.bcentral.cl/siete/

SERIES <- list(
  # National Accounts (quarterly, real, 2018 pesos)
  gdp               = "F032.PIB.FLU.R.CLP.EP18.Z.Z.0.T",   # GDP
  consumption       = "F032.PIB.FLU.R.CLP.EP18.Z.Z.4.T",   # Private consumption
  investment        = "F032.PIB.FLU.R.CLP.EP18.Z.Z.6.T",   # Gross fixed capital formation
  gov_spending      = "F032.PIB.FLU.R.CLP.EP18.Z.Z.5.T",   # Government consumption

  # Labor market (monthly)
  unemployment      = "F026.EME.TAD.2022.D10.LCI.TOT.10.M", # Unemployment rate
  employment        = "F026.EME.TAD.2022.D10.LCI.TOT.T.M",  # Employment (thousands)

  # Prices
  cpi               = "F073.IPC.V12.2018.M",                 # CPI, 12-month change (monthly)
  cpi_index         = "F073.IPC.VAR.2018.M",                 # CPI monthly change

  # Monetary policy
  tpm               = "F022.TPM.TIN.D001.NO.Z.D",            # Monetary policy rate (daily)

  # Exchange rate
  usd_clp           = "F073.TCO.PRE.Z.D",                    # USD/CLP (daily)

  # Wages
  wage_index        = "F026.IRM.IRM.2016.M"                  # Real wage index (monthly)
)

FIRST_DATE <- "1990-01-01"
LAST_DATE  <- format(Sys.Date(), "%Y-%m-%d")

# ---------------------------------------------------------------------------- #
# Helpers
# ---------------------------------------------------------------------------- #

bcch_url <- function(...) {
  args  <- list(...)
  base  <- "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"
  parts <- c(
    paste0("user=", BCCH_USER),
    paste0("pass=", BCCH_PASS),
    paste0(names(args), "=", unlist(args))
  )
  paste0(base, "?", paste(parts, collapse = "&"))
}

# Search available series by keyword and frequency
bcch_search <- function(keyword = NULL, frequency = "QUARTERLY") {
  url <- bcch_url(frequency = frequency, `function` = "SearchSeries")
  raw <- rjson::fromJSON(file = url)
  df  <- as.data.frame(do.call(rbind, lapply(raw$SeriesInfos, as.vector)),
                        stringsAsFactors = FALSE)
  if (!is.null(keyword)) {
    mask <- grepl(keyword, df$spanishTitle, ignore.case = TRUE) |
            grepl(keyword, df$englishTitle,  ignore.case = TRUE)
    df   <- df[mask, ]
  }
  df
}

# Fetch a single series by ID; returns a tidy data frame
bcch_fetch <- function(series_id, name,
                       first = FIRST_DATE, last = LAST_DATE) {
  url <- bcch_url(
    firstdate  = first,
    lastdate   = last,
    timeseries = series_id,
    `function` = "GetSeries"
  )
  raw <- tryCatch(rjson::fromJSON(file = url), error = function(e) {
    warning("Failed to fetch ", series_id, ": ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$Series$Obs)) return(NULL)

  df <- as.data.frame(
    do.call(rbind, lapply(raw$Series$Obs, as.vector)),
    stringsAsFactors = FALSE
  ) |>
    transmute(
      date       = dmy(indexDateString),
      value      = suppressWarnings(as.numeric(value)),
      status     = statusCode,
      series_id  = series_id,
      series     = name
    ) |>
    filter(status == "OK", !is.na(value))

  df
}

# ---------------------------------------------------------------------------- #
# 1. Fetch all series
# ---------------------------------------------------------------------------- #

raw_list <- mapply(bcch_fetch,
                   series_id = unlist(SERIES),
                   name      = names(SERIES),
                   SIMPLIFY  = FALSE)

raw_list <- Filter(Negate(is.null), raw_list)

all_series <- bind_rows(raw_list)

cat("Fetched", n_distinct(all_series$series), "series,",
    nrow(all_series), "observations\n")
cat("Date range:", format(range(all_series$date)), "\n\n")

# ---------------------------------------------------------------------------- #
# 2. Aggregate to quarterly
# ---------------------------------------------------------------------------- #

# Tag each observation with its frequency (daily/monthly/quarterly)
# High-frequency series → average within quarter
quarterly <- all_series |>
  mutate(
    year    = year(date),
    quarter = quarter(date),
    qdate   = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))
  ) |>
  group_by(series, series_id, qdate) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  rename(date = qdate) |>
  arrange(series, date)

# ---------------------------------------------------------------------------- #
# 3. Save
# ---------------------------------------------------------------------------- #

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

write.csv(all_series, "data/clean/bcch_raw.csv",       row.names = FALSE)
write.csv(quarterly,  "data/clean/bcch_quarterly.csv", row.names = FALSE)

cat("Saved data/clean/bcch_raw.csv\n")
cat("Saved data/clean/bcch_quarterly.csv\n\n")

# ---------------------------------------------------------------------------- #
# 4. Quick check — wide quarterly table for the main HANK series
# ---------------------------------------------------------------------------- #

hank_series <- c("gdp", "consumption", "investment", "gov_spending",
                 "cpi", "tpm", "unemployment")

quarterly |>
  filter(series %in% hank_series) |>
  select(date, series, value) |>
  pivot_wider(names_from = series, values_from = value) |>
  arrange(date) |>
  tail(12) |>
  print()
