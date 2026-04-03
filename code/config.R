# config.R
# Shared settings sourced by all code/*.R scripts.
# Edit here to change settings globally — do NOT define these in individual scripts.
#
# Usage: source(here("code", "config.R"))   (from project root via here)
#
# Author:  Fred Milhome
# Project: hank-emerging

# ── GDP / output definition ───────────────────────────────────────────────────
# TRUE  = Y = internal demand (pib − net exports)   [BBL definition]
# FALSE = Y = full GDP (pib)
# Chile: net exports are large and volatile; keep consistent across all scripts.
USE_INTERNAL_DEMAND <- TRUE

# ── Calibration / estimation sample period ────────────────────────────────────
# Integer years; NULL = use all available data.
# Used by compute_capital_targets() in calibration_targets.R.
# Set once estimation sample is finalised.
PERIOD_START <- NULL
PERIOD_END   <- NULL
