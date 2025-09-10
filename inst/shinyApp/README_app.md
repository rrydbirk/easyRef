# EasyRef Shiny App

A web-based interface for generating citations and references for R packages from CRAN and Bioconductor.

## Features

- **User-friendly interface**: Enter package names in a simple text area
- **Multiple output formats**: Support for RIS, BibTeX, or both formats
- **Enhanced BioC support**: Includes author information for Bioconductor packages even when not installed locally
- **Web scraping**: Automatically retrieves author information from BioC package websites
- **Progress tracking**: Shows status messages like "Finding X references" and "Created Y of X files"
- **Error handling**: Shows warnings instead of errors for missing packages
- **File downloads**: Download generated citations as files
- **Batch processing**: Process multiple packages at once

## How to Run

1. Make sure you have R and the required packages installed:
   ```r
   install.packages(c("shiny", "easyRef", "xml2", "rvest"))
   ```

2. Navigate to the package directory and run the app:
   ```r
   library(shiny)
   library(easyRef)
   runApp("inst/shinyApp/app.R")
   ```

3. Open your web browser and go to `http://localhost:3838`

## How to Use

1. **Enter package names**: In the text area, enter one package name per line (e.g., `ggplot2`, `dplyr`, `GRaNIE`, `ropls`)

2. **Select output format**: Choose from:
   - RIS format (default)
   - BibTeX format
   - Both formats
  
3. **Optional filename**: Specify a custom filename for downloads (without extension)

4. **Generate citations**: Click the "Generate Citations" button

5. **View results**: The app will show:
   - Status messages during processing
   - Generated citations in the main panel
   - Download buttons for the files

## Example Usage

### Input:
```
ggplot2
dplyr
GRaNIE
ropls
nonexistentpackage
```

## Supported Packages

- **CRAN packages**: All packages available on CRAN (e.g., ggplot2, dplyr, shiny)
- **Bioconductor packages**: All packages available on Bioconductor (e.g., Biobase, limma, edgeR, GRaNIE, ropls)
- **Automatic detection**: The app automatically detects whether a package is from CRAN or Bioconductor

## Error Handling

- **Invalid package names**: Gracefully handles typos and non-existent packages
- **Network issues**: Handles connection problems when fetching package metadata

## File Downloads

- **RIS files**: Download as `.ris` files for reference managers like Zotero, Mendeley
- **BibTeX files**: Download as `.bib` files for LaTeX documents
- **Custom filenames**: Use the filename input to specify custom output names

## Technical Details

- Built with R Shiny framework
- Uses the `easyRef` package for citation generation
- Supports both installed and uninstalled packages
- **Web scraping**: Automatically retrieves author information from BioC package websites using `xml2` and `rvest`
- Automatically retrieves DOIs from GitHub repositories and published papers
- Handles case-insensitive package name matching
- **Hybrid approach**: Combines web scraping with curated citation data for optimal accuracy
