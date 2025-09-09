# EasyRef Shiny App

A web-based interface for generating citations and references for R packages from CRAN and Bioconductor.

## Features

- **User-friendly interface**: Enter package names in a simple text area
- **Multiple output formats**: Support for RIS, BibTeX, or both formats
- **Progress tracking**: Shows status messages like "Finding X references" and "Created Y of X files"
- **Error handling**: Shows warnings instead of errors for missing packages
- **File downloads**: Download generated citations as files
- **Batch processing**: Process multiple packages at once

## How to Run

1. Make sure you have R and the required packages installed:
   ```r
   install.packages(c("shiny", "easyRef"))
   ```

2. Navigate to the package directory and run the app:
   ```r
   library(shiny)
   library(easyRef)
   runApp("app.R")
   ```

3. Open your web browser and go to `http://localhost:3838`

## How to Use

1. **Enter package names**: In the text area, enter one package name per line (e.g., `ggplot2`, `dplyr`, `Biobase`)

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
Biobase
nonexistentpackage
```

### Output:
- Status: "Finding 4 references..."
- Status: "Processing package: ggplot2"
- Status: "Created 1 of 4 files"
- Status: "Processing package: dplyr"
- Status: "Created 2 of 4 files"
- Status: "Processing package: Biobase"
- Status: "Created 3 of 4 files"
- Status: "Processing package: nonexistentpackage"
- Warning: "Could not find package 'nonexistentpackage'. Skipping this package."
- Status: "Successfully generated 3 citations"

## Supported Packages

- **CRAN packages**: All packages available on CRAN (e.g., ggplot2, dplyr, shiny)
- **Bioconductor packages**: All packages available on Bioconductor (e.g., Biobase, limma, edgeR)
- **Automatic detection**: The app automatically detects whether a package is from CRAN or Bioconductor

## Error Handling

- **Missing packages**: Shows warnings instead of stopping the entire process
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
- Automatically retrieves DOIs from GitHub repositories and published papers
- Handles case-insensitive package name matching