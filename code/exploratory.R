# exploratory.R — Exploratory Plots for WID Wealth Inequality Data
# ==================================================================
#
# Produces publication-quality time-series plots (since 1980) for each
# wealth-share measure across Mexico, Brazil, and Chile.
#
# Input:   data/clean/wid_wealth_shares.csv  (produced by cleaning.R)
# Output:  output/figures/top10_share.{pdf,png}
#          output/figures/top1_share.{pdf,png}
#          output/figures/bottom50_share.{pdf,png}
#
# Author:  Fred Milhome
# Project: hank-emerging
# Created: 2026

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# ── Constants ─────────────────────────────────────────────────────────────────

data_file  <- here("data", "clean", "wid_wealth_shares.csv")
fig_dir    <- here("output", "figures")
start_year <- 1980L

# Colour-blind friendly palette — one colour per country
country_colours <- c(
  Mexico = "#E69F00",  # amber
  Brazil = "#009E73",  # teal
  Chile  = "#0072B2"   # blue
)

plot_specs <- list(
  list(percentile = "p90p100", title = "Top 10% Wealth Share",    filename = "top10_share"),
  list(percentile = "p99p100", title = "Top 1% Wealth Share",     filename = "top1_share"),
  list(percentile = "p0p50",   title = "Bottom 50% Wealth Share", filename = "bottom50_share")
)

# ── Theme ─────────────────────────────────────────────────────────────────────

theme_journal <- function() {
  theme_classic(base_size = 11, base_family = "serif") +
    theme(
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.6),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(1.5, "cm")
    )
}

# ── Plotting ──────────────────────────────────────────────────────────────────

plot_measure <- function(df, percentile, title, filename) {
  subset_df <- df |> filter(.data$percentile == .env$percentile)

  p <- ggplot(subset_df, aes(x = year, y = share, colour = country)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = country_colours) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title, x = "Year", y = "Share of net personal wealth") +
    theme_journal()

  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(fig_dir, paste0(filename, ".pdf")), p, width = 8, height = 4.5)
  ggsave(file.path(fig_dir, paste0(filename, ".png")), p, width = 8, height = 4.5, dpi = 150)

  message("  Saved -> ", file.path(fig_dir, filename), ".{pdf,png}")
}

# ── Main ──────────────────────────────────────────────────────────────────────

main <- function() {
  df <- read_csv(data_file, show_col_types = FALSE) |>
    filter(year >= start_year)

  message("Loaded ", nrow(df), " observations (years >= ", start_year, ").\n")
  message("Generating figures:")

  for (spec in plot_specs) {
    plot_measure(df, spec$percentile, spec$title, spec$filename)
  }

  message("\nAll figures saved.")
}

main()
