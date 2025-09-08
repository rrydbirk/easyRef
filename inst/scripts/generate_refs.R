#!/usr/bin/env Rscript
# Command-line script for generating package citations
# This script provides a command-line interface to the easyRef package functions

# Load the easyRef package
library(easyRef)

print_help_and_quit <- function() {
  cat("Usage:\n")
  cat("  Rscript inst/scripts/generate_refs.R [--format ris|bibtex|both] [--out PATH] [--split] [--overwrite] pkg1 [pkg2...]\n\n")
  cat("Options:\n")
  cat("  --format       ris|bibtex|both (default: ris)\n")
  cat("  --out          Output file path or directory. Default: stdout\n")
  cat("  --split        Write one file per package when multiple packages or format=both\n")
  cat("  --overwrite    Allow overwriting existing files\n")
  cat("  --help         Show this help\n\n")
  cat("Supported packages:\n")
  cat("  - CRAN packages (e.g., ggplot2, dplyr)\n")
  cat("  - Bioconductor packages (e.g., Biobase, limma, edgeR)\n")
  cat("  - Automatic DOI retrieval from GitHub repositories\n")
  cat("  - Published paper DOI detection\n")
  quit(status = 0)
}

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
opts <- list(format = "ris", out = "", split = FALSE, overwrite = FALSE, pkgs = character(0))

i <- 1
while (i <= length(args)) {
  a <- args[[i]]
  if (a == "--help") {
    print_help_and_quit()
  } else if (a == "--format") {
    if (i == length(args)) { stop("--format requires an argument") }
    i <- i + 1
    opts$format <- tolower(args[[i]])
    if (!opts$format %in% c("ris","bibtex","both")) stop("Invalid --format; use ris|bibtex|both")
  } else if (a == "--out") {
    if (i == length(args)) { stop("--out requires an argument") }
    i <- i + 1
    opts$out <- args[[i]]
  } else if (a == "--split") {
    opts$split <- TRUE
  } else if (a == "--overwrite") {
    opts$overwrite <- TRUE
  } else if (startsWith(a, "--")) {
    stop(paste0("Unknown option: ", a))
  } else {
    opts$pkgs <- c(opts$pkgs, a)
  }
  i <- i + 1
}

if (length(opts$pkgs) == 0) {
  cat("Error: Provide at least one package name.\n\n")
  print_help_and_quit()
}

# Main execution using new function names
if (!nzchar(opts$out)) {
  # stdout mode: use createRef and display results
  results <- createRef(opts$pkgs, format = opts$format)
  if (opts$format == "both") {
    for (r in results) {
      cat(r$ris, "\n")
      cat(r$bibtex, "\n\n")
    }
  } else if (opts$format == "ris") {
    for (r in results) {
      cat(r$ris, "\n")
    }
  } else if (opts$format == "bibtex") {
    for (r in results) {
      cat(r$bibtex, "\n\n")
    }
  }
} else {
  # File mode: use createRef with filename parameter
  if (opts$split || dir.exists(opts$out)) {
    # Per-package files inside a directory
    ensure_dir(opts$out)
    for (pkg in opts$pkgs) {
      if (opts$format %in% c("ris", "both")) {
        createRef(pkg, format = "ris", filename = file.path(opts$out, paste0(pkg, ".ris")), overwrite = opts$overwrite)
      }
      if (opts$format %in% c("bibtex", "both")) {
        createRef(pkg, format = "bibtex", filename = file.path(opts$out, paste0(pkg, ".bib")), overwrite = opts$overwrite)
      }
    }
  } else {
    # Single combined file
    if (opts$format == "both") {
      # For both formats, create separate files
      ris_file <- paste0(tools::file_path_sans_ext(opts$out), ".ris")
      bib_file <- paste0(tools::file_path_sans_ext(opts$out), ".bib")
      createRef(opts$pkgs, format = "ris", filename = ris_file, overwrite = opts$overwrite)
      createRef(opts$pkgs, format = "bibtex", filename = bib_file, overwrite = opts$overwrite)
    } else {
      createRef(opts$pkgs, format = opts$format, filename = opts$out, overwrite = opts$overwrite)
    }
  }
}