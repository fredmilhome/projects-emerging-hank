# 11_cleaning_bcch.R
# Pull macro time series from the Banco Central de Chile (BCCh) REST API.
#
# Output (timestamped, non-idempotent — each run creates a new folder):
#   data/raw/bcch_raw/YYYYMMDD_HHMMSS/bcch_main.csv
#   data/raw/bcch_raw/YYYYMMDD_HHMMSS/bcch_main.rds
#
# PULL ONLY — no deflation, no per-capita, no aggregation, no interpolation.
# Series are stored at their native frequency (quarterly, monthly, or annual).
# All transformation choices live in code/21_timeseries_processing.R.
#
# To read the most recent pull in another script:
#   folder <- tail(sort(Sys.glob("data/raw/bcch_raw/*")), 1)
#   bcch   <- readRDS(file.path(folder, "bcch_main.rds"))
#
# API docs: https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx
# Register for credentials at: https://si3.bcentral.cl/siete/

library(httr)
library(rjson)
library(dplyr)
library(tidyr)
library(lubridate)

# ---------------------------------------------------------------------------- #
# CREDENTIALS — paste yours here
# ---------------------------------------------------------------------------- #

BCCH_USER <- "fred.COOSTA@hotmail.com"
BCCH_PASS <- "JtkyggDbQY7nkYxgkn6HKG2T"

# ---------------------------------------------------------------------------- #
# Series catalogue
# ---------------------------------------------------------------------------- #
# Each entry: name = BCCh series ID
#
# Native frequencies and properties:
#   National accounts (pib, ddi, …): Quarterly | SA | Nominal CLP billions
#   Deflator:                         Quarterly | SA | Index (2018 Q-avg = 100)
#   Population:                       Annual    | —  | Persons
#   Wage index:                       Monthly   | —  | Index (base yr 2023)
#   Hours worked:                     Monthly   | —  | Level (hours)
#
# NOTE: FRED/Haver US national accounts use SAAR (SA + annualized ×4).
#       BCCh national accounts are SA but NOT annualized — actual quarterly flows.
#       For growth rates (log-diffs) this distinction is irrelevant.
#       For level ratios (K/Y, G/Y), multiply BCCh quarterly flow ×4 for
#       annual-rate equivalent before dividing.

SERIES <- list(

  # ── GDP Deflator ───────────────────────────────────────────────────────────
  # SA, empalmado, referencia 2018. Index (2018 quarterly average = 100).
  deflator       = "F032.PIB.DEF.N.CLP.EP18.Z.Z.1.T",

  # ── National Accounts — expenditure-side GDP tree ─────────────────────────
  # All quarterly. Nominal CLP (billions of pesos), SA, empalmado ref 2018.
  # Note: export/import sub-series use "2018" suffix instead of "EP18" in the
  #       series code — same vintage and dataset.
  #
  # Accounting identities:
  #   pib  = ddi + xbs - ibs
  #   ddi  = cto + fbkf + vax
  #   cto  = cpr + cog
  #   cpr  = cdu + cnd + cse
  #   fbkf = fbkf_const + fbkf_maq
  #   xbs  = xbi + xse  ;  xbi = xap + xmi + xin
  #   ibs  = ibi + ise  ;  ibi = iap + imi + iin

  pib            = "F032.PIB.FLU.N.CLP.EP18.Z.Z.1.T",   # PIB
  ddi            = "F033.DDI.FLU.N.CLP.EP18.1.T",        # Demanda Interna
  cto            = "F033.CTO.FLU.N.CLP.EP18.1.T",        # Consumo Total
  cpr            = "F033.CPR.FLU.N.CLP.EP18.1.T",        # Consumo Hogares e IPSFL (incl. durables)
  cdu            = "F033.CDU.FLU.N.CLP.EP18.1.T",        # Bienes Durables
  cnd            = "F033.CND.FLU.N.CLP.EP18.1.T",        # Bienes No Durables
  cse            = "F033.CSE.FLU.N.CLP.EP18.1.T",        # Servicios
  cog            = "F033.COG.FLU.N.CLP.EP18.1.T",        # Consumo Gobierno
  fbkf           = "F033.FKF.FLU.N.CLP.EP18.1.T",        # Formación Bruta de Capital Fijo
  fbkf_const     = "F033.FKK.FLU.N.CLP.EP18.1.T",        # FBKF — Construcción y Otras Obras
  fbkf_maq       = "F033.FKO.FLU.N.CLP.EP18.1.T",        # FBKF — Maquinaria y Equipo
  vax            = "F033.VAX.FLU.N.CLP.EP18.1.T",        # Variación de Existencias
  xbs            = "F033.XBS.FLU.N.CLP.EP18.1.T",        # Exportaciones Bienes y Servicios
  xbi            = "F033.XBI.FLU.N.CLP.2018.1.T",        # Exportación Bienes
  xap            = "F033.XAP.FLU.N.CLP.2018.1.T",        #   Agropecuario-Silvícola-Pesca
  xmi            = "F033.XMI.FLU.N.CLP.2018.1.T",        #   Minería
  xin            = "F033.XIN.FLU.N.CLP.2018.1.T",        #   Industria
  xse            = "F033.XSE.FLU.N.CLP.2018.1.T",        # Exportación Servicios
  ibs            = "F033.IBS.FLU.N.CLP.EP18.1.T",        # Importaciones Bienes y Servicios
  ibi            = "F033.IBI.FLU.N.CLP.2018.1.T",        # Importación Bienes
  iap            = "F033.IAP.FLU.N.CLP.2018.1.T",        #   Agropecuario-Silvícola-Pesca
  imi            = "F033.IMI.FLU.N.CLP.2018.1.T",        #   Minería
  iin            = "F033.IIN.FLU.N.CLP.2018.1.T",        #   Industria
  ise            = "F033.ISE.FLU.N.CLP.2018.1.T",        # Importación Servicios

  # ── Population ────────────────────────────────────────────────────────────
  # Annual. One observation per year, stored as-is.
  # Frequency alignment is handled in 21_timeseries_processing.R.
  population     = "F049.POB.STO.INE1.01.A",             # Total population (INE projections)

  # ── Wage Index ────────────────────────────────────────────────────────────
  # Monthly. Índice de Remuneraciones Nominales (INE, base year 2023).
  wage_index     = "G049.RMM.IND.INE23.NE.M",

  # ── Hours Worked ──────────────────────────────────────────────────────────
  # Monthly. Total hours worked (INE).
  hours_worked   = "F049.HEO.PRO.INE1.01.M",

  # ── Interbank Rate ────────────────────────────────────────────────────────
  # Monthly (suffix .M).
  interbank_rate_1d = "F022.TIB.TIP.D001.NO.Z.M",

  # ── Debt to GDP ratio - IMF ───────────────────────────────────────────────
  # Annual. Ratio of total public debt to GDP.
  d_to_gdp_fmi    = "F019.DBPIB.PPIB.CH.A"
)

SERIES <- Filter(Negate(is.null), SERIES)   # remove sentinel

FIRST_DATE <- "1954-01-01"
LAST_DATE  <- format(Sys.Date(), "%Y-%m-%d")

# ---------------------------------------------------------------------------- #
# Helpers
# ---------------------------------------------------------------------------- #

BASE_URL <- "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"

bcch_url <- function(...) {
  args  <- list(...)
  parts <- c(
    paste0("user=", BCCH_USER),
    paste0("pass=", BCCH_PASS),
    paste0(names(args), "=", unlist(args))
  )
  paste0(BASE_URL, "?", paste(parts, collapse = "&"))
}

# GET via httr, parse JSON — avoids rjson HTTPS issues on Windows
bcch_get <- function(...) {
  url  <- bcch_url(...)
  resp <- tryCatch(GET(url, timeout(30)), error = function(e) {
    warning("Connection error: ", e$message); NULL
  })
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  txt    <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(rjson::fromJSON(txt), error = function(e) {
    warning("JSON parse error: ", e$message); NULL
  })
  parsed
}

# Search available series by keyword
bcch_search <- function(keyword = NULL, frequency = "QUARTERLY") {
  raw <- bcch_get(frequency = frequency, `function` = "SearchSeries")
  if (is.null(raw) || is.null(raw$SeriesInfos)) return(data.frame())
  df <- as.data.frame(do.call(rbind, lapply(raw$SeriesInfos, as.vector)),
                      stringsAsFactors = FALSE)
  if (!is.null(keyword)) {
    mask <- grepl(keyword, df$spanishTitle, ignore.case = TRUE) |
            grepl(keyword, df$englishTitle,  ignore.case = TRUE)
    df   <- df[mask, ]
  }
  df
}

# Fetch a single series at its native frequency; returns tidy data frame
bcch_fetch <- function(series_id, name,
                       first = FIRST_DATE, last = LAST_DATE) {
  raw <- bcch_get(
    `function` = "GetSeries",
    timeseries = series_id,
    firstdate  = first,
    lastdate   = last
  )
  if (is.null(raw) || is.null(raw$Series$Obs)) {
    warning("No data for ", series_id,
            if (!is.null(raw$Message)) paste0(": ", raw$Message))
    return(NULL)
  }

  df <- do.call(rbind, lapply(raw$Series$Obs, function(obs) {
    data.frame(
      indexDateString = as.character(obs$indexDateString),
      value           = as.character(obs$value),
      statusCode      = as.character(obs$statusCode),
      stringsAsFactors = FALSE
    )
  })) |>
    transmute(
      date      = dmy(indexDateString),
      value     = suppressWarnings(as.numeric(value)),
      status    = statusCode,
      series_id = series_id,
      series    = name
    ) |>
    filter(status == "OK", !is.na(value))

  df
}

# Safe wrapper: logs result or failure; never aborts the script
safe_fetch <- function(series_id, name) {
  result <- tryCatch(
    bcch_fetch(series_id, name),
    error = function(e) {
      message(sprintf("  ERROR  '%s' (%s): %s", name, series_id, e$message))
      NULL
    }
  )
  if (is.null(result)) {
    message(sprintf("  SKIP   '%s' (%s) — absent from output", name, series_id))
  } else {
    message(sprintf("  OK     %-16s  %4d obs  %s – %s",
                    paste0("'", name, "'"),
                    nrow(result),
                    format(min(result$date)),
                    format(max(result$date))))
  }
  result
}

# ---------------------------------------------------------------------------- #
# 1. Fetch all series
# ---------------------------------------------------------------------------- #

cat(sprintf("Fetching %d series from BCCh REST API...\n\n", length(SERIES)))

raw_list <- mapply(safe_fetch,
                   series_id = unlist(SERIES),
                   name      = names(SERIES),
                   SIMPLIFY  = FALSE)

raw_list   <- Filter(Negate(is.null), raw_list)
all_series <- bind_rows(raw_list)

n_ok   <- n_distinct(all_series$series)
n_tot  <- length(SERIES)
n_skip <- n_tot - n_ok

cat(sprintf("\nFetched %d of %d series", n_ok, n_tot))
if (n_skip > 0) {
  skipped <- setdiff(names(SERIES), unique(all_series$series))
  cat(sprintf(" | %d skipped: %s", n_skip, paste(skipped, collapse = ", ")))
}
cat("\n")
cat("Full date range:", format(range(all_series$date)), "\n")

# ---------------------------------------------------------------------------- #
# 2. Quick checks
# ---------------------------------------------------------------------------- #

# Coverage by series
coverage <- all_series |>
  group_by(series, series_id) |>
  summarise(
    n_obs      = n(),
    date_first = min(date),
    date_last  = max(date),
    .groups    = "drop"
  ) |>
  arrange(series)

cat("\n=== Series coverage ===\n")
print(as.data.frame(coverage))

# GDP accounting identity check
# Monthly / annual series produce NAs when pivoting wide — expected.
wide_check <- all_series |>
  select(date, series, value) |>
  pivot_wider(names_from = series, values_from = value)

required_ids <- c("pib", "cpr", "cog", "fbkf", "vax", "xbs", "ibs")

if (all(required_ids %in% names(wide_check))) {
  id_check <- wide_check |>
    filter(!is.na(pib), !is.na(cpr), !is.na(fbkf)) |>
    mutate(residual = pib - (cpr + cog + fbkf + vax + xbs - ibs))
  cat(sprintf(
    "\nGDP identity — max absolute residual: %.3f billion CLP\n",
    max(abs(id_check$residual), na.rm = TRUE)
  ))
  cat("(Near 0 expected; non-zero reflects SA rounding across sub-series)\n")
} else {
  missing_ids <- setdiff(required_ids, names(wide_check))
  cat(sprintf("\nGDP identity check skipped — missing series: %s\n",
              paste(missing_ids, collapse = ", ")))
}

# ---------------------------------------------------------------------------- #
# 3. Save — timestamped folder, fixed filenames inside
# ---------------------------------------------------------------------------- #
# Each run creates a new folder: data/raw/bcch_raw/YYYYMMDD_HHMMSS/
# Inside: 2 files with stable names (no timestamp in filename).
#
# To read the most recent pull in another script:
#   folder <- tail(sort(Sys.glob("data/raw/bcch_raw/*")), 1)
#   bcch   <- readRDS(file.path(folder, "bcch_main.rds"))

ts      <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("data", "raw", "bcch_raw", ts)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(all_series, file.path(out_dir, "bcch_main.csv"), row.names = FALSE)
saveRDS(all_series,   file.path(out_dir, "bcch_main.rds"))

cat(sprintf("\nSaved to %s/\n", out_dir))
cat("  bcch_main.csv  — all series (quarterly / monthly / annual)\n")
cat("  bcch_main.rds\n")