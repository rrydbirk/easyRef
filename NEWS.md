# easyRef News

## easyRef 0.1.0

### Initial Release

* **New Features:**
  - Generate citations and references for R packages from CRAN and Bioconductor
  - Support for RIS and BibTeX output formats
  - Automatic DOI retrieval from GitHub repositories and published papers
  - Command-line interface for batch processing
  - Support for both CRAN and Bioconductor packages
  - Automatic repository detection

* **Functions:**
  - `createRef()`: Main function for generating package citations
  - `createRis()`: Generate RIS format citations
  - `createBibtex()`: Generate BibTeX format citations
  - `createBiocRef()`: Convenience function for Bioconductor packages

* **Command-line Interface:**
  - `inst/scripts/generate_refs.R`: Command-line script for batch processing
  - Support for multiple output formats and file options
  - Help system and error handling

* **Dependencies:**
  - Base R packages (utils, tools)
  - Optional: BiocManager for Bioconductor support
  - Optional: stringr for enhanced DOI extraction