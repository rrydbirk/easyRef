# easyRef: Easy Reference Generation for R Packages

`easyRef` is an R package that generates citations and references for R packages from both CRAN and Bioconductor repositories. It supports RIS and BibTeX formats with automatic DOI retrieval from GitHub repositories and published papers.

## Features

- **Multi-repository support**: Works with both CRAN and Bioconductor packages
- **Automatic DOI detection**: Retrieves DOIs from package citations, GitHub repositories, and published papers
- **Multiple output formats**: RIS and BibTeX citation formats
- **Command-line interface**: Batch processing via command-line script
- **GitHub integration**: Automatically detects and uses GitHub repository information
- **Published paper support**: Finds DOIs from associated research papers

## Installation

```r
# Install from source (development version)
install.packages("devtools")
devtools::install_github("your-username/easyRef")

# Or install locally
install.packages(".", repos = NULL, type = "source")
```

## Dependencies

- **Required**: Base R packages
- **Suggested**: `BiocManager` (for Bioconductor package support)

## Usage

### Basic Usage

```r
library(easyRef)

# Generate RIS citation for a CRAN package
result <- createRef("ggplot2")

# Generate citation for a Bioconductor package
result <- createRef("Biobase")

# Generate both RIS and BibTeX formats
results <- createRef(c("ggplot2", "dplyr", "Biobase"), format = "both")

# Write to file
createRef("ggplot2", filename = "ggplot2_citation.ris")
```

### Bioconductor-specific Function

```r
# Convenience function for Bioconductor packages
result <- createBiocRef("Biobase")

# Multiple Bioconductor packages
bioc_packages <- c("Biobase", "limma", "edgeR", "DESeq2")
results <- createBiocRef(bioc_packages, format = "both")
```

### Command-line Interface

```bash
# Generate RIS citation for a single package
Rscript inst/scripts/generate_refs.R ggplot2

# Generate both formats for multiple packages
Rscript inst/scripts/generate_refs.R --format both --out citations/ ggplot2 dplyr Biobase

# Generate separate files for each package
Rscript inst/scripts/generate_refs.R --format both --split --out citations/ ggplot2 dplyr Biobase

# Show help
Rscript inst/scripts/generate_refs.R --help
```

## Supported Package Types

### CRAN Packages
- Standard R packages from the Comprehensive R Archive Network
- Examples: `ggplot2`, `dplyr`, `tidyr`

### Bioconductor Packages
- Bioinformatics packages from Bioconductor
- Examples: `Biobase`, `limma`, `edgeR`, `DESeq2`, `GenomicRanges`
- Requires `BiocManager` package for full functionality

## DOI Retrieval

The package automatically attempts to retrieve DOIs from multiple sources:

1. **Package citations**: Built-in citation information
2. **Package metadata**: DESCRIPTION file DOI fields
3. **GitHub repositories**: README and CITATION files
4. **Published papers**: Associated research publications

## Output Formats

### RIS Format
```
TY  - COMP
AU  - Wickham, Hadley
TI  - ggplot2: Create Elegant Data Visualisations
PY  - 2016
PB  - Comprehensive R Archive Network (CRAN)
UR  - https://ggplot2.tidyverse.org
DO  - 10.1000/182
M3  - Computer software
N1  - R package version 3.4.0
ER  - 
```

### BibTeX Format
```bibtex
@misc{ggplot2_2016,
  author = {Wickham, Hadley},
  title = {ggplot2: Create Elegant Data Visualisations},
  year = {2016},
  url = {https://ggplot2.tidyverse.org},
  note = {R package version 3.4.0}
}
```

## Examples

### Academic Citation
```r
# Generate citation for a research paper's associated package
result <- createRef("DESeq2")
cat(result$ris)
```

### Batch Processing
```r
# Generate citations for all packages used in a project
packages <- c("ggplot2", "dplyr", "Biobase", "limma", "edgeR")
results <- createRef(packages, format = "both", filename = "project_citations.ris")
```

### Command-line Batch Processing
```bash
# Generate citations for all packages in a requirements file
cat requirements.txt | xargs Rscript inst/scripts/generate_refs.R --format both --out citations/
```

## Configuration

### Repository Settings
The package automatically detects the appropriate repository (CRAN or Bioconductor) for each package. For Bioconductor packages, ensure `BiocManager` is installed:

```r
install.packages("BiocManager")
```

### GitHub Integration
The package automatically detects GitHub repositories from package URLs and attempts to retrieve DOI information from:
- README.md files
- inst/CITATION files
- Repository topics and descriptions

## Troubleshooting

### Common Issues

1. **Bioconductor packages not found**: Install `BiocManager`
   ```r
   install.packages("BiocManager")
   ```

2. **DOI not found**: The package will work without DOIs, but for better citations, ensure packages have proper citation information

3. **GitHub access issues**: The package uses public GitHub URLs, so no authentication is required

### Error Handling
The package includes comprehensive error handling and will gracefully fall back to available information if some sources are unavailable.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

MIT License - see LICENSE file for details.

## Citation

If you use `easyRef` in your research, please cite it:

```r
citation("easyRef")
```