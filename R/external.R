#' Create reference citation for R packages
#'
#' This is the main function to collect information about R packages and generate
#' citations in RIS or BibTeX format. Supports both CRAN and Bioconductor packages
#' with automatic DOI retrieval from GitHub repositories and published papers.
#'
#' @param pkg Character vector of package names to process
#' @param format Output format: "ris", "bib", "bibtex", or "both" (default: "ris")
#' @param filename Output file path. If NULL, creates a default filename based on package name(s).
#'   If no file extension is provided, one will be added based on the format.
#' @param overwrite Allow overwriting existing files (default: TRUE)
#' @param verbose Logical. If TRUE, prints detailed information about each step (default: FALSE)
#' @param database Repository to search: "auto" (default), "cran", or "bioconductor".
#'   "auto" automatically detects the repository, "cran" searches only CRAN,
#'   "bioconductor" searches only Bioconductor.
#' @return Invisible list of results with package information and formatted citations. Always writes to file.
#' @export
#' @examples
#' \dontrun{
#' # Generate RIS citation for a CRAN package
#' result <- createRef("ggplot2", filename = tempfile()) # Omit filename
#'
#' # Generate citation for a Bioconductor package (requires BiocManager)
#' if (requireNamespace("BiocManager", quietly = TRUE)) {
#'   result <- createRef("Biobase")
#' }
#'
#' # Generate both RIS and BibTeX for multiple packages
#' results <- createRef(c("ggplot2", "dplyr"), format = "both", filename = tempfile())
#'
#' # Write to file (extension will be added automatically)
#' createRef("ggplot2", filename = tempfile()) # Replace with e.g. "ggplot2_citation"
#'
#' # Verbose output showing each step (writes to default file)
#' createRef("ggplot2", verbose = TRUE, filename = tempfile()) # Omit filename
#'
#' # Force search in specific repository
#' createRef("ggplot2", database = "cran", filename = tempfile()) # Omit filename
#' }
createRef <- function(pkg, format = "ris", filename = NULL, overwrite = TRUE, verbose = FALSE, database = "auto") {
  # Normalize database parameter
  database <- tolower(database)
  if (!database %in% c("auto", "cran", "bioconductor")) {
    stop("database parameter must be 'auto', 'cran', or 'bioconductor'")
  }

  # Normalize and validate format parameter
  format <- tolower(format)
  if (!format %in% c("ris", "bibtex", "bib", "both")) {
    stop("format parameter must be 'ris', 'bib', 'bibtex', or 'both'. Default is 'ris'.")
  }

  # Convert "bib" to "bibtex" for internal processing
  if (format == "bib") {
    format <- "bibtex"
  }

  if (verbose) {
    cat("Starting createRef process...\n")
    cat("Packages to process:", paste(pkg, collapse = ", "), "\n")
    cat("Output format:", format, "\n")
    cat("Database search:", database, "\n")
    if (!is.null(filename)) {
      cat("Output filename:", filename, "\n")
    }
    cat("Overwrite existing files:", overwrite, "\n")
    cat("\n")
  }

  # Add file extension if none provided and filename is given
  if (!is.null(filename)) {
    # Check if filename has an extension
    has_extension <- grepl("\\.[a-zA-Z0-9]+$", filename)

    if (!has_extension) {
      # Determine extension based on format
      if (format == "ris") {
        filename <- paste0(filename, ".ris")
      } else if (format == "bibtex") {
        filename <- paste0(filename, ".bib")
      } else if (format == "both") {
        # For "both" format, we'll use .ris as default, but the emit_outputs_internal
        # function will handle creating separate files if needed
        filename <- paste0(filename, ".ris")
      }

      if (verbose) {
        cat("Added file extension based on format. New filename:", filename, "\n")
      }
    }

  }

  # Process single or multiple packages
  if (verbose) {
    cat("Collecting package information...\n")
  }

  if (length(pkg) == 1) {
    if (verbose) {
      cat("Processing single package:", pkg, "\n")
    }
    results <- list(collect_for_package_internal(pkg, database = database, verbose = verbose))
  } else {
    if (verbose) {
      cat("Processing", length(pkg), "packages...\n")
    }
    results <- lapply(pkg, function(p) {
      if (verbose) {
        cat("  - Processing package:", p, "\n")
      }
      collect_for_package_internal(p, database = database, verbose = verbose)
    })
  }

  if (verbose) {
    cat("Package information collection completed.\n")
    cat("Found", length(results), "package(s) with citation data.\n")

    # Display author information for each package
    for (i in seq_along(results)) {
      result <- results[[i]]
      cat("Package:", result$pkg, "\n")
      cat("  Title:", result$title, "\n")
      if (length(result$authors) > 0) {
        cat("  Authors:", paste(result$authors, collapse = "; "), "\n")
      } else {
        cat("  Authors: Not available\n")
      }
      cat("  Year:", result$year, "\n")
      cat("  Repository:", result$repository, "\n")
      if (!is.null(result$version) && nzchar(result$version)) {
        cat("  Version:", result$version, "\n")
      }
      if (!is.null(result$doi) && nzchar(result$doi)) {
        cat("  DOI:", result$doi, "\n")
      }
      cat("\n")
    }
  }

  # Always write to file - if no filename provided, create default filename
  if (is.null(filename)) {
    # Create default filename based on package names
    if (length(pkg) == 1) {
      filename <- pkg[1]
    } else {
      filename <- "packages"
    }

    # Add appropriate extension
    if (format == "ris") {
      filename <- paste0(filename, ".ris")
    } else if (format == "bibtex") {
      filename <- paste0(filename, ".bib")
    } else if (format == "both") {
      filename <- paste0(filename, ".ris")
    }

    if (verbose) {
      cat("No filename provided - using default filename:", filename, "\n")
    }
  }

  if (verbose) {
    # Get absolute path, handling both existing and non-existing files
    if (file.exists(filename)) {
      full_path <- normalizePath(filename)
    } else {
      # For non-existing files, get the absolute path of the current directory
      # and combine with the filename
      current_dir <- getwd()
      full_path <- file.path(current_dir, filename)
      # Normalize the path to handle any ".." or "." components
      full_path <- normalizePath(full_path, mustWork = FALSE)
    }
    cat("Writing output to file:", full_path, "\n")
  }
  emit_outputs_internal(results, format, filename, FALSE, overwrite)
  if (verbose) {
    cat("File writing completed.\n")
  }

  return(invisible(results))
}

#' Create RIS format citation for software
#'
#' Creates a properly formatted RIS citation entry for software packages.
#'
#' @param title Software title
#' @param authors Character vector of author names
#' @param year Publication year
#' @param url Software URL (optional)
#' @param version Software version (optional)
#' @param doi DOI (optional)
#' @param notes Additional notes (optional)
#' @param publisher Publisher name (default: "Comprehensive R Archive Network (CRAN)")
#' @param filename Output file path. If NULL, returns the RIS string without writing to file
#' @param overwrite Allow overwriting existing files (default: TRUE)
#' @return Character string with RIS formatted citation, or writes to file if filename provided
#' @export
#' @examples
#' createRis(
#'   title = "ggplot2: Create Elegant Data Visualisations",
#'   authors = c("Wickham, Hadley"),
#'   year = "2016",
#'   url = "https://ggplot2.tidyverse.org",
#'   filename = tempfile() # Omit filename
#' )
#'
#' # Write to file
#' createRis(
#'   title = "ggplot2: Create Elegant Data Visualisations",
#'   authors = c("Wickham, Hadley"),
#'   year = "2016",
#'   filename = tempfile() # Replace with e.g. "ggplot2.ris"
#' )
createRis <- function(title, authors, year, url = NULL, version = NULL, doi = NULL, notes = NULL, publisher = "Comprehensive R Archive Network (CRAN)", filename = NULL, overwrite = TRUE) {
  lines <- c()
  lines <- c(lines, "TY  - COMP")
  if (length(authors) > 0) {
    lines <- c(lines, paste0("AU  - ", vapply(authors, ris_sanitize, character(1))))
  }
  if (!is.null(title) && nzchar(title)) {
    lines <- c(lines, paste0("TI  - ", ris_sanitize(title)))
  }
  if (!is.null(year) && nzchar(year)) {
    lines <- c(lines, paste0("PY  - ", ris_sanitize(year)))
    lines <- c(lines, paste0("Y1  - ", ris_sanitize(year)))
  }
  if (!is.null(publisher) && nzchar(publisher)) {
    lines <- c(lines, paste0("PB  - ", ris_sanitize(publisher)))
  }
  if (!is.null(url) && nzchar(url)) {
    lines <- c(lines, paste0("UR  - ", ris_sanitize(url)))
  }
  if (!is.null(doi) && nzchar(doi)) {
    lines <- c(lines, paste0("DO  - ", ris_sanitize(doi)))
  }
  # Type-of-work and version/notes
  lines <- c(lines, "M3  - Computer software")
  if (!is.null(version) && nzchar(version)) {
    lines <- c(lines, paste0("N1  - R package version ", ris_sanitize(version)))
  }
  if (!is.null(notes) && nzchar(notes)) {
    lines <- c(lines, paste0("N2  - ", ris_sanitize(notes)))
  }
  lines <- c(lines, "ER  - ")
  ris_text <- paste0(lines, collapse = "\n")

  # If filename provided, write to file
  if (!is.null(filename)) {
    write_text(filename, paste0(ris_text, "\n"), overwrite)
    return(invisible(ris_text))
  }

  return(ris_text)
}

#' Create BibTeX format citation for software
#'
#' Creates a properly formatted BibTeX citation entry for software packages.
#'
#' @param key BibTeX key for the entry
#' @param title Software title
#' @param authors Character vector of author names
#' @param year Publication year
#' @param url Software URL (optional)
#' @param version Software version (optional)
#' @param filename Output file path. If NULL, returns the BibTeX string without writing to file
#' @param overwrite Allow overwriting existing files (default: TRUE)
#' @return Character string with BibTeX formatted citation, or writes to file if filename provided
#' @export
#' @examples
#' createBibtex(
#'   key = "ggplot2_2016",
#'   title = "ggplot2: Create Elegant Data Visualisations",
#'   authors = c("Wickham, Hadley"),
#'   year = "2016",
#'   url = "https://ggplot2.tidyverse.org"
#'   filename = tempfile() # Omit filename
#' )
#'
#' # Write to file
#' createBibtex(
#'   key = "ggplot2_2016",
#'   title = "ggplot2: Create Elegant Data Visualisations",
#'   authors = c("Wickham, Hadley"),
#'   year = "2016",
#'   filename = tempfile() # Replace with e.g. "ggplot2.bib"
#' )
createBibtex <- function(key, title, authors, year, url = NULL, version = NULL, filename = NULL, overwrite = TRUE) {
  # Minimal @software entry; many BibTeX styles accept @misc
  author_field <- if (length(authors) > 0) paste(authors, collapse = " and ") else ""
  fields <- c(
    if (nzchar(author_field)) paste0("  author = {", author_field, "}") else NULL,
    if (!is.null(title) && nzchar(title)) paste0("  title = {", title, "}") else NULL,
    if (!is.null(year) && nzchar(year)) paste0("  year = {", year, "}") else NULL,
    if (!is.null(url) && nzchar(url)) paste0("  url = {", url, "}") else NULL,
    if (!is.null(version) && nzchar(version)) paste0("  note = {R package version ", version, "}") else NULL
  )
  entry <- c(paste0("@misc{", key, ","), paste0(fields, ","), "}")
  bibtex_text <- paste0(entry, collapse = "\n")

  # If filename provided, write to file
  if (!is.null(filename)) {
    write_text(filename, paste0(bibtex_text, "\n"), overwrite)
    return(invisible(bibtex_text))
  }

  return(bibtex_text)
}

#' Create reference citation for Bioconductor packages
#'
#' Convenience function specifically for Bioconductor packages. Automatically
#' detects Bioconductor packages and retrieves metadata from Bioconductor repositories.
#'
#' @param pkg Character vector of Bioconductor package names to process
#' @param format Output format: "ris", "bib", "bibtex", or "both" (default: "ris")
#' @param filename Output file path. If NULL, creates a default filename based on package name(s).
#'   If no file extension is provided, one will be added based on the format.
#' @param overwrite Allow overwriting existing files (default: TRUE)
#' @param verbose Logical. If TRUE, prints detailed information about each step (default: FALSE)
#' @param database Repository to search: "auto" (default), "cran", or "bioconductor".
#'   For Bioconductor packages, "bioconductor" is recommended.
#' @return Invisible list of results with package information and formatted citations. Always writes to file.
#' @export
#' @examples
#' \dontrun{
#' # Generate citation for Bioconductor packages (requires BiocManager)
#' if (requireNamespace("BiocManager", quietly = TRUE)) {
#'   result <- createBiocRef("Biobase")
#'
#'   # Generate citations for multiple Bioconductor packages
#'   bioc_packages <- c("Biobase", "limma", "edgeR")
#'   results <- createBiocRef(bioc_packages, format = "both", filename = tempfile()) # Omit filename
#'
#'   # Write Bioconductor package citations to file (extension added automatically)
#'   createBiocRef("Biobase", filename = tempfile()) # Replace with e.g. "biobase_citation"
#'
#'   # Verbose output for Bioconductor packages (writes to default file)
#'   createBiocRef("Biobase", verbose = TRUE, filename = tempfile()) # Omit filename = tempfile()
#'
#'   # Force search in Bioconductor repository
#'   createBiocRef("Biobase", database = "bioconductor", verbose = TRUE, filename = tempfile()) # Omit filename = tempfile()
#' }
#' }
createBiocRef <- function(pkg, format = "ris", filename = NULL, overwrite = TRUE, verbose = FALSE, database = "auto") {
  # Normalize and validate format parameter
  format <- tolower(format)
  if (!format %in% c("ris", "bibtex", "bib", "both")) {
    stop("format parameter must be 'ris', 'bib', 'bibtex', or 'both'")
  }

  # Convert "bib" to "bibtex" for internal processing
  if (format == "bib") {
    format <- "bibtex"
  }

  # Check if BiocManager is available
  if (!is_installed("BiocManager")) {
    warning("BiocManager not available. Install with: install.packages('BiocManager')")
  }

  # Use the main createRef function - it will automatically detect Bioconductor packages
  createRef(pkg, format = format, filename = filename, overwrite = overwrite, verbose = verbose, database = database)
}
