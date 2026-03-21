# add_packages.R — Install new packages and update the lockfile
# ==============================================================
#
# Installs one or more packages into the renv library and snapshots
# the lockfile to record them.
#
# Usage (from terminal):
#   Rscript code/R/add_packages.R <pkg1> [pkg2 ...]
#
# Examples:
#   Rscript code/R/add_packages.R haven
#   Rscript code/R/add_packages.R haven labelled stringr
#
# Author:  Fred Milhome
# Project: hank-emerging

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv is not installed. Install it with: install.packages('renv')")
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop(
    "No packages specified.\n",
    "Usage: Rscript code/R/add_packages.R <pkg1> [pkg2 ...]\n",
    "Example: Rscript code/R/add_packages.R haven labelled"
  )
}

cat("Installing packages:", paste(args, collapse = ", "), "\n\n")
renv::install(args)

cat("\nUpdating lockfile...\n")
renv::snapshot()

cat("\nDone. Commit renv.lock to record the new dependencies.\n")
