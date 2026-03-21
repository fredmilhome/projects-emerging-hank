# check_env.R — Verify renv environment matches the lockfile
# ===========================================================
#
# Run this at the start of a session to confirm the project environment
# is in sync. If packages are missing or out of date, it prints a clear
# message and suggests the fix.
#
# Usage:
#   Rscript code/R/check_env.R
#
# Author:  Fred Milhome
# Project: hank-emerging

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv is not installed. Install it with: install.packages('renv')")
}

cat("Checking renv environment...\n\n")

status <- renv::status()

if (isTRUE(status$synchronized)) {
  cat("Environment is in sync with the lockfile.\n")
} else {
  cat("Environment is OUT OF SYNC with the lockfile.\n\n")
  cat("To restore the environment to match the lockfile, run:\n")
  cat("  renv::restore()\n\n")
  cat("Or from the terminal:\n")
  cat("  Rscript -e \"renv::restore()\"\n")
}
