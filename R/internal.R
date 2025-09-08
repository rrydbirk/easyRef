# Internal helper functions for easyRef package
# These functions are not exported and are used internally by the package

#' Safely get element from list with default value
#' @param x List or object to access
#' @param name Name of element to get
#' @param default Default value if element doesn't exist
#' @return Element value or default
safely_get <- function(x, name, default = NULL) {
  if (is.null(x)) return(default)
  tryCatch({
    if (name %in% names(x)) {
      x[[name]]
    } else {
      default
    }
  }, error = function(e) default)
}

#' Check if a package is installed
#' @param pkg Package name to check
#' @return Logical indicating if package is installed
is_installed <- function(pkg) {
  suppressWarnings(requireNamespace(pkg, quietly = TRUE))
}

#' Convert person object to string format
#' @param p Person object
#' @return Character string with formatted name
person_to_string <- function(p) {
  if (is.null(p)) return("")
  
  # Handle case where p is a list of person objects
  if (is.list(p) && length(p) > 0 && is.list(p[[1]])) {
    # Multiple person objects
    names <- character(0)
    for (person in p) {
      names <- c(names, person_to_string(person))
    }
    return(paste(names, collapse = "; "))
  }
  
  # Single person object
  given <- if (!is.null(p$given)) paste(p$given, collapse = " ") else ""
  family <- if (!is.null(p$family)) paste(p$family, collapse = " ") else ""
  
  if (nchar(family) > 0 && nchar(given) > 0) {
    name <- paste(family, given, sep = ", ")
  } else if (nchar(family) > 0) {
    name <- family
  } else if (nchar(given) > 0) {
    name <- given
  } else {
    # Fall back to 'email' or 'comment' or 'role' if really empty
    fallback <- c(p$email, p$role, p$comment)
    fallback <- fallback[!is.null(fallback) & nzchar(fallback)]
    name <- if (length(fallback) > 0) paste(fallback, collapse = " ") else ""
  }
  
  # Clean any bracketed content from the name
  name <- clean_author_name(name)
  trimws(name)
}

#' Extract authors from bibentry object
#' @param be Bibentry object
#' @return Character vector of author names
authors_from_bibentry <- function(be) {
  if (is.null(be)) return(character(0))
  
  # Try to extract author information from the deeply nested structure
  tryCatch({
    # Navigate through the nested structure: be -> [[1]] -> [[1]] -> author
    if (length(be) > 0 && length(be[[1]]) > 0) {
      author_field <- be[[1]][[1]]$author
      if (!is.null(author_field) && length(author_field) > 0) {
        # Extract all authors from the author field
        all_authors <- character(0)
        for (i in seq_along(author_field)) {
          person_obj <- author_field[[i]]
          
          # Handle the nested structure: person_obj -> [[1]] -> given/family
          if (length(person_obj) > 0 && is.list(person_obj[[1]])) {
            person_data <- person_obj[[1]]
            
            # Extract given and family names
            given <- if (!is.null(person_data$given)) person_data$given else ""
            family <- if (!is.null(person_data$family)) person_data$family else ""
            
            # Handle multiple given names (join with space)
            if (is.vector(given) && length(given) > 1) {
              given <- paste(given, collapse = " ")
            }
            
            # Format the name
            if (nchar(family) > 0 && nchar(given) > 0) {
              name <- paste(family, given, sep = ", ")
            } else if (nchar(family) > 0) {
              name <- family
            } else if (nchar(given) > 0) {
              name <- given
            } else {
              # Skip this author if no name found
              next
            }
            
            # Clean any bracketed content
            name <- clean_author_name(name)
            all_authors <- c(all_authors, trimws(name))
          }
        }
        return(all_authors)
      }
    }
    # If we get here, direct access failed
    stop("Direct access failed")
  }, error = function(e) {
    # Fallback: try to extract from text representation
    tryCatch({
      text_repr <- as.character(be)
      # Look for author patterns in the text representation
      if (grepl('author.*=.*list\\(list\\(given = "([^"]+)", family = "([^"]+)"', text_repr)) {
        author_match <- regmatches(text_repr, regexec('author.*=.*list\\(list\\(given = "([^"]+)", family = "([^"]+)"', text_repr))[[1]]
        if (length(author_match) >= 3) {
          given <- author_match[2]
          family <- author_match[3]
          author_name <- paste(family, given, sep = ", ")
          author_name <- clean_author_name(author_name)
          return(trimws(author_name))
        }
      }
      return(character(0))
    }, error = function(e2) character(0))
  })
}

#' Extract year from bibentry object
#' @param be Bibentry object
#' @return Character string with year
year_from_bibentry <- function(be) {
  if (is.null(be)) return(as.character(format(Sys.Date(), "%Y")))
  
  y <- tryCatch({
    # Try direct access first (works even when names() is NULL)
    be$year
  }, error = function(e) {
    # Fallback to names check
    tryCatch({
      if ("year" %in% names(be)) {
        be$year
      } else {
        NULL
      }
    }, error = function(e2) NULL)
  })
  
  if (is.null(y)) {
    d <- tryCatch({
      # Try direct access first
      be$date
    }, error = function(e) {
      # Fallback to names check
      tryCatch({
        if ("date" %in% names(be)) {
          be$date
        } else {
          NULL
        }
      }, error = function(e2) NULL)
    })
    
    if (!is.null(d) && nzchar(d)) {
      y <- substr(as.character(d), 1, 4)
    }
  }
  
  if (is.null(y)) {
    y <- as.character(format(Sys.Date(), "%Y"))
  }
  as.character(y)
}

#' Sanitize text for RIS format (single-line)
#' @param x Text to sanitize
#' @return Sanitized text
ris_sanitize <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  trimws(x)
}

#' Get CRAN metadata for a package without installing it
#' @param pkg Package name
#' @return List with package metadata or NULL if not found
cran_meta_for <- function(pkg) {
  # Try to get fast cache; if fails, fall back to CRAN snapshot
  repos <- getOption("repos")
  if (length(repos) == 0 || isTRUE(is.na(repos)) || repos == "@CRAN@") {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  
  ap <- tryCatch(utils::available.packages(repos = repos), error = function(e) NULL)
  if (is.null(ap) || nrow(ap) == 0 || !(pkg %in% rownames(ap))) return(NULL)
  
  # Safely extract package information (only basic info available from available.packages)
  tryCatch({
    list(
      Title    = pkg,  # Use package name as title since Title not available
      Version  = ap[pkg, "Version"],
      URL      = NULL,  # Not available from available.packages
      Author   = NULL,  # Not available from available.packages
      Maintainer = NULL  # Not available from available.packages
    )
  }, error = function(e) NULL)
}

#' Get Bioconductor metadata for a package without installing it
#' @param pkg Package name
#' @return List with package metadata or NULL if not found
bioc_meta_for <- function(pkg) {
  # Check if BiocManager is available
  if (!is_installed("BiocManager")) {
    return(NULL)
  }
  
  # Get Bioconductor repositories
  bioc_repos <- tryCatch({
    BiocManager::repositories()
  }, error = function(e) NULL)
  
  if (is.null(bioc_repos)) return(NULL)
  
  # Try to get package info from Bioconductor
  ap <- tryCatch(utils::available.packages(repos = bioc_repos), error = function(e) NULL)
  if (is.null(ap) || nrow(ap) == 0 || !(pkg %in% rownames(ap))) return(NULL)
  
  # Safely extract package information (only basic info available from available.packages)
  tryCatch({
    list(
      Title    = pkg,  # Use package name as title since Title not available
      Version  = ap[pkg, "Version"],
      URL      = paste0("https://bioconductor.org/packages/", pkg),  # Bioconductor package URL
      Author   = NULL,  # Not available from available.packages
      Maintainer = NULL,  # Not available from available.packages
      Repository = "Bioconductor"
    )
  }, error = function(e) NULL)
}

#' Check if a package exists on CRAN or Bioconductor repositories
#' @param pkg Package name
#' @return Logical indicating if package exists on any repository
package_exists_on_repos <- function(pkg) {
  # Direct check using available.packages
  repos <- getOption("repos")
  if (length(repos) == 0 || isTRUE(is.na(repos)) || repos == "@CRAN@") {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  
  # Check CRAN
  ap <- tryCatch(utils::available.packages(repos = repos), error = function(e) NULL)
  if (!is.null(ap) && nrow(ap) > 0 && pkg %in% rownames(ap)) {
    return(TRUE)
  }
  
  # Check Bioconductor if available
  if (is_installed("BiocManager")) {
    bioc_repos <- tryCatch({
      BiocManager::repositories()
    }, error = function(e) NULL)
    
    if (!is.null(bioc_repos)) {
      # Check only Bioconductor-specific repositories (exclude CRAN)
      bioc_only_repos <- bioc_repos[!names(bioc_repos) %in% c("CRAN", "CRANextra")]
      
      if (length(bioc_only_repos) > 0) {
        ap_bioc <- tryCatch(utils::available.packages(repos = bioc_only_repos), error = function(e) NULL)
        if (!is.null(ap_bioc) && nrow(ap_bioc) > 0 && pkg %in% rownames(ap_bioc)) {
          return(TRUE)
        }
      }
    }
  }
  
  return(FALSE)
}

#' Check if a package is from Bioconductor
#' @param pkg Package name
#' @return Logical indicating if package is from Bioconductor
is_bioc_package <- function(pkg) {
  if (!is_installed("BiocManager")) return(FALSE)
  
  bioc_repos <- tryCatch({
    BiocManager::repositories()
  }, error = function(e) NULL)
  
  if (is.null(bioc_repos)) return(FALSE)
  
  # Check only Bioconductor-specific repositories (exclude CRAN)
  bioc_only_repos <- bioc_repos[!names(bioc_repos) %in% c("CRAN", "CRANextra")]
  
  if (length(bioc_only_repos) == 0) return(FALSE)
  
  ap <- tryCatch(utils::available.packages(repos = bioc_only_repos), error = function(e) NULL)
  if (is.null(ap)) return(FALSE)
  
  pkg %in% rownames(ap)
}

#' Get GitHub repository information and DOI
#' @param pkg Package name
#' @param url Package URL (optional)
#' @return List with GitHub info and DOI or NULL if not found
get_github_info <- function(pkg, url = NULL) {
  github_info <- list(repo = NULL, doi = NULL, github_url = NULL)
  
  # Try to extract GitHub URL from package URL
  if (!is.null(url) && nzchar(url)) {
    urls <- strsplit(url, "[ ,]")[[1]]
    for (u in urls) {
      if (grepl("github\\.com", u, ignore.case = TRUE)) {
        github_info$github_url <- u
        # Extract repo owner/name
        repo_match <- regmatches(u, regexec("github\\.com/([^/]+)/([^/]+)", u, ignore.case = TRUE))[[1]]
        if (length(repo_match) >= 3) {
          github_info$repo <- paste0(repo_match[2], "/", gsub("\\.git$", "", repo_match[3]))
        }
        break
      }
    }
  }
  
  # If no GitHub URL found, try common patterns
  if (is.null(github_info$repo)) {
    # Try to find GitHub URL from package description or citation
    if (is_installed(pkg)) {
      desc <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
      if (!is.null(desc)) {
        bug_reports <- safely_get(desc, "BugReports", default = NULL)
        if (!is.null(bug_reports) && grepl("github\\.com", bug_reports, ignore.case = TRUE)) {
          repo_match <- regmatches(bug_reports, regexec("github\\.com/([^/]+)/([^/]+)", bug_reports, ignore.case = TRUE))[[1]]
          if (length(repo_match) >= 3) {
            github_info$repo <- paste0(repo_match[2], "/", gsub("\\.git$", "", repo_match[3]))
            github_info$github_url <- paste0("https://github.com/", github_info$repo)
          }
        }
      }
    }
  }
  
  # Try to get DOI from GitHub repository
  if (!is.null(github_info$repo)) {
    github_info$doi <- get_doi_from_github(github_info$repo)
  }
  
  github_info
}

#' Extract DOI from reference sections in README content
#' @param readme_content Character vector of README lines
#' @return DOI string or NULL if not found
extract_doi_from_reference_sections <- function(readme_content) {
  if (is.null(readme_content) || length(readme_content) == 0) return(NULL)
  
  # Look for reference/citation section headers
  ref_section_patterns <- c(
    "^#+\\s*references?\\s*$",
    "^#+\\s*citation[s]?\\s*$", 
    "^#+\\s*citing\\s*$",
    "^references?\\s*$",
    "^citation[s]?\\s*$",
    "^citing\\s*$"
  )
  
  # Find reference sections
  ref_section_indices <- integer(0)
  for (pattern in ref_section_patterns) {
    matches <- grep(pattern, readme_content, ignore.case = TRUE)
    ref_section_indices <- c(ref_section_indices, matches)
  }
  
  if (length(ref_section_indices) == 0) return(NULL)
  
  # Extract content from reference sections (next 20 lines after each section header)
  ref_content <- character(0)
  for (idx in ref_section_indices) {
    end_idx <- min(idx + 20, length(readme_content))
    ref_content <- c(ref_content, readme_content[idx:end_idx])
  }
  
  # Look for DOI patterns in reference sections - simplified and robust patterns
  doi_patterns <- c(
    "doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)",
    "DOI:\\s*([0-9]+\\.[0-9]+/[^\\s]+)",
    "https://doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)",
    "doi:([0-9]+\\.[0-9]+/[^\\s]+)",
    "DOI:([0-9]+\\.[0-9]+/[^\\s]+)",
    "\\bdoi\\s*[:=]\\s*([0-9]+\\.[0-9]+/[^\\s]+)",
    "\\bDOI\\s*[:=]\\s*([0-9]+\\.[0-9]+/[^\\s]+)"
  )
  
  # Collect DOIs from reference sections using multiple approaches
  ref_dois <- character(0)
  
  # Try stringr approach if available
  if (requireNamespace("stringr", quietly = TRUE)) {
    for (line in ref_content) {
      # Look for DOI patterns using stringr
      doi_match <- stringr::str_extract(line, "doi:([0-9]+\\.[0-9]+/[^\\s]+)")
      if (!is.na(doi_match)) {
        doi <- gsub("doi:", "", doi_match, ignore.case = TRUE)
        doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
        if (!grepl("^10\\.", doi)) {
          doi <- paste0("10.", doi)
        }
        ref_dois <- c(ref_dois, doi)
      }
      
      # Also try https://doi.org patterns
      doi_match2 <- stringr::str_extract(line, "https://doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)")
      if (!is.na(doi_match2)) {
        doi <- gsub("https://doi\\.org/", "", doi_match2)
        doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
        if (!grepl("^10\\.", doi)) {
          doi <- paste0("10.", doi)
        }
        ref_dois <- c(ref_dois, doi)
      }
    }
  }
  
  # Fallback to base R approach
  if (length(ref_dois) == 0) {
    for (pattern in doi_patterns) {
      matches <- regmatches(ref_content, regexec(pattern, ref_content, ignore.case = TRUE))
      for (match in matches) {
        if (length(match) >= 2 && nzchar(match[2])) {
          doi <- match[2]
          # Clean up the DOI (remove trailing punctuation)
          doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
          if (!grepl("^10\\.", doi)) {
            doi <- paste0("10.", doi)
          }
          ref_dois <- c(ref_dois, doi)
        }
      }
    }
  }
  
  # Return the first DOI found in reference sections
  if (length(ref_dois) > 0) {
    return(ref_dois[1])
  }
  
  NULL
}

#' Get DOI from GitHub repository
#' @param repo GitHub repository in format "owner/repo"
#' @return DOI string or NULL if not found
get_doi_from_github <- function(repo) {
  # Try to get DOI from GitHub repository using web scraping
  # This is a basic implementation that looks for DOI patterns in README
  
  if (is.null(repo) || !nzchar(repo)) return(NULL)
  
  # Try to fetch README content from GitHub
  readme_url <- paste0("https://raw.githubusercontent.com/", repo, "/main/README.md")
  readme_alt_url <- paste0("https://raw.githubusercontent.com/", repo, "/master/README.md")
  
  # Try main branch first, then master (suppress warnings for missing files)
  readme_content <- tryCatch({
    suppressWarnings(readLines(readme_url, warn = FALSE))
  }, error = function(e) {
    tryCatch({
      suppressWarnings(readLines(readme_alt_url, warn = FALSE))
    }, error = function(e2) NULL)
  })
  
  if (!is.null(readme_content)) {
    # First, try to find References/Citation sections and prioritize them
    reference_sections <- extract_doi_from_reference_sections(readme_content)
    if (!is.null(reference_sections)) {
      return(reference_sections)
    }
    
    # Fallback to general DOI patterns throughout the document
    doi_patterns <- c(
      # Standard DOI patterns
      "doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)",
      "DOI:\\s*([0-9]+\\.[0-9]+/[^\\s]+)",
      "https://doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)",
      # Additional patterns for various formats
      "doi:([0-9]+\\.[0-9]+/[^\\s]+)",
      "DOI:([0-9]+\\.[0-9]+/[^\\s]+)",
      "\\bdoi\\s*[:=]\\s*([0-9]+\\.[0-9]+/[^\\s]+)",
      "\\bDOI\\s*[:=]\\s*([0-9]+\\.[0-9]+/[^\\s]+)"
    )
    
    # Collect all DOIs found using multiple approaches
    all_dois <- character(0)
    
    # Try stringr approach if available
    if (requireNamespace("stringr", quietly = TRUE)) {
      for (line in readme_content) {
        # Look for DOI patterns using stringr
        doi_match <- stringr::str_extract(line, "doi:([0-9]+\\.[0-9]+/[^\\s]+)")
        if (!is.na(doi_match)) {
          doi <- gsub("doi:", "", doi_match, ignore.case = TRUE)
          doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
          if (!grepl("^10\\.", doi)) {
            doi <- paste0("10.", doi)
          }
          all_dois <- c(all_dois, doi)
        }
        
        # Also try https://doi.org patterns
        doi_match2 <- stringr::str_extract(line, "https://doi\\.org/([0-9]+\\.[0-9]+/[^\\s]+)")
        if (!is.na(doi_match2)) {
          doi <- gsub("https://doi\\.org/", "", doi_match2)
          doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
          if (!grepl("^10\\.", doi)) {
            doi <- paste0("10.", doi)
          }
          all_dois <- c(all_dois, doi)
        }
      }
    }
    
    # Fallback to base R approach
    if (length(all_dois) == 0) {
      for (pattern in doi_patterns) {
        matches <- regmatches(readme_content, regexec(pattern, readme_content, ignore.case = TRUE))
        for (match in matches) {
          if (length(match) >= 2 && nzchar(match[2])) {
            doi <- match[2]
            # Clean up the DOI (remove trailing punctuation)
            doi <- gsub("[^0-9./a-zA-Z-]+$", "", doi)
            # Ensure DOI starts with proper prefix
            if (!grepl("^10\\.", doi)) {
              doi <- paste0("10.", doi)
            }
            all_dois <- c(all_dois, doi)
          }
        }
      }
    }
    
    # Return the most relevant DOI (first one found, or prioritize certain patterns)
    if (length(all_dois) > 0) {
      return(all_dois[1])
    }
  }
  
  # Try to get DOI from CITATION file (suppress warnings for missing files)
  citation_url <- paste0("https://raw.githubusercontent.com/", repo, "/main/inst/CITATION")
  citation_alt_url <- paste0("https://raw.githubusercontent.com/", repo, "/master/inst/CITATION")
  
  citation_content <- tryCatch({
    suppressWarnings(readLines(citation_url, warn = FALSE))
  }, error = function(e) {
    tryCatch({
      suppressWarnings(readLines(citation_alt_url, warn = FALSE))
    }, error = function(e2) NULL)
  })
  
  if (!is.null(citation_content)) {
    # Look for DOI in CITATION file
    doi_patterns <- c(
      "doi\\s*=\\s*[\"']([0-9]+\\.[0-9]+/[^\"'\\s]+)[\"']",
      "DOI\\s*=\\s*[\"']([0-9]+\\.[0-9]+/[^\"'\\s]+)[\"']"
    )
    
    for (pattern in doi_patterns) {
      matches <- regmatches(citation_content, regexec(pattern, citation_content, ignore.case = TRUE))
      for (match in matches) {
        if (length(match) >= 2 && nzchar(match[2])) {
          doi <- match[2]
          if (!grepl("^10\\.", doi)) {
            doi <- paste0("10.", doi)
          }
          return(doi)
        }
      }
    }
  }
  
  NULL
}

#' Get DOI from package citation or metadata
#' @param pkg Package name
#' @return DOI string or NULL if not found
get_doi_from_package <- function(pkg) {
  if (!is_installed(pkg)) return(NULL)
  
  # Try to get DOI from citation
  cit <- tryCatch(utils::citation(pkg), error = function(e) NULL)
  if (!is.null(cit)) {
    for (entry in cit) {
      doi <- safely_get(entry, "doi", default = NULL)
      if (!is.null(doi) && nzchar(doi)) {
        return(as.character(doi))
      }
    }
  }
  
  # Try to get DOI from package description
  desc <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
  if (!is.null(desc)) {
    doi <- safely_get(desc, "DOI", default = NULL)
    if (!is.null(doi) && nzchar(doi)) {
      return(as.character(doi))
    }
  }
  
  NULL
}

#' Generate CRAN/Bioconductor DOI for a package
#' @param pkg Package name
#' @param repository Repository name ("CRAN" or "Bioconductor")
#' @return DOI string or NULL if not found
get_repository_doi <- function(pkg, repository) {
  if (repository == "CRAN") {
    # CRAN packages have DOIs in the format: 10.32614/CRAN.package.PACKAGENAME
    return(paste0("10.32614/CRAN.package.", pkg))
  } else if (repository == "Bioconductor") {
    # Bioconductor packages typically have DOIs in the format: 10.18129/B9.bioc.PACKAGENAME
    return(paste0("10.18129/B9.bioc.", pkg))
  }
  return(NULL)
}

#' Clean author names by removing bracketed content
#' @param x Author name string
#' @return Cleaned author name string
clean_author_name <- function(x) {
  if (is.null(x) || !nzchar(x)) return(x)
  # Remove content in square brackets like [aut, cre], [aut], etc.
  gsub("\\s*\\[[^]]*\\]\\s*", " ", x)
}

#' Parse author text from DESCRIPTION file
#' @param x Author text string
#' @return Character vector of author names
parse_author_text <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  # A crude split; better than nothing for freeform Author fields
  y <- unlist(strsplit(x, "( and |,|;)", perl = TRUE))
  y <- trimws(y)
  
  # Filter out funding authors (those with [fnd] role)
  y <- y[!grepl("\\[fnd\\]", y, ignore.case = TRUE)]
  
  y <- vapply(y, clean_author_name, character(1))
  y <- trimws(y)
  y[nzchar(y)]
}

#' Convert bibentry to BibTeX string
#' @param be Bibentry object
#' @param key_hint Optional key hint (not used currently)
#' @return BibTeX string or NULL if conversion fails
bibtex_from_bibentry <- function(be, key_hint = NULL) {
  if (is.null(be)) return(NULL)
  
  # toBibtex returns class 'Bibtex'; collapse to string
  bt <- tryCatch(toBibtex(be), error = function(e) NULL)
  if (is.null(bt)) return(NULL)
  paste0(bt, collapse = "\n")
}

#' Normalize string for use as BibTeX key
#' @param x String to normalize
#' @return Normalized string
normalize_key <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  gsub("_+", "_", x)
}

#' Try case-insensitive search for package name
#' @param pkg Package name to search for
#' @param database Repository to search: "auto", "cran", or "bioconductor"
#' @param verbose Logical. If TRUE, prints detailed information
#' @return List with origin and correct_name if found, NULL otherwise
try_case_insensitive_search <- function(pkg, database, verbose = FALSE) {
  if (database == "cran" || database == "auto") {
    # Search CRAN case-insensitively
    repos <- getOption("repos")
    if (length(repos) == 0 || isTRUE(is.na(repos)) || repos == "@CRAN@") {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    ap <- tryCatch(utils::available.packages(repos = repos), error = function(e) NULL)
    
    if (!is.null(ap) && nrow(ap) > 0) {
      # Find case-insensitive match
      cran_packages <- rownames(ap)
      match_idx <- which(tolower(cran_packages) == tolower(pkg))
      
      if (length(match_idx) > 0) {
        correct_name <- cran_packages[match_idx[1]]
        if (verbose) {
          cat("    Case-insensitive match found in CRAN:", correct_name, "\n")
        }
        return(list(origin = "CRAN", correct_name = correct_name))
      }
    }
  }
  
  if (database == "bioconductor" || database == "auto") {
    # Search Bioconductor case-insensitively
    if (is_installed("BiocManager")) {
      bioc_repos <- tryCatch({
        BiocManager::repositories()
      }, error = function(e) NULL)
      
      if (!is.null(bioc_repos)) {
        bioc_only_repos <- bioc_repos[!names(bioc_repos) %in% c("CRAN", "CRANextra")]
        
        if (length(bioc_only_repos) > 0) {
          ap_bioc <- tryCatch(utils::available.packages(repos = bioc_only_repos), error = function(e) NULL)
          
          if (!is.null(ap_bioc) && nrow(ap_bioc) > 0) {
            # Find case-insensitive match
            bioc_packages <- rownames(ap_bioc)
            match_idx <- which(tolower(bioc_packages) == tolower(pkg))
            
            if (length(match_idx) > 0) {
              correct_name <- bioc_packages[match_idx[1]]
              if (verbose) {
                cat("    Case-insensitive match found in Bioconductor:", correct_name, "\n")
              }
              return(list(origin = "Bioconductor", correct_name = correct_name))
            }
          }
        }
      }
    }
  }
  
  return(NULL)
}

#' Ensure directory exists
#' @param path Directory path
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

#' Write text to file with overwrite control
#' @param path File path
#' @param text Text to write
#' @param overwrite Whether to allow overwriting existing files
write_text <- function(path, text, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) stop(paste0("File exists: ", path, " (use --overwrite)"))
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, sep = "\n")
}

#' Collect package information and generate citations (internal)
#' @param pkg Package name
#' @param database Repository to search: "auto", "cran", or "bioconductor"
#' @param verbose Logical. If TRUE, prints detailed information about each step
#' @return List with package information and formatted citations
collect_for_package_internal <- function(pkg, database = "auto", verbose = FALSE) {
  # Determine package origin based on database parameter
  if (database == "cran") {
    # Check only CRAN
    if (verbose) {
      cat("    Searching in CRAN repository...\n")
    }
    repos <- getOption("repos")
    if (length(repos) == 0 || isTRUE(is.na(repos)) || repos == "@CRAN@") {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    ap <- tryCatch(utils::available.packages(repos = repos), error = function(e) NULL)
    package_found <- !is.null(ap) && nrow(ap) > 0 && pkg %in% rownames(ap)
    package_origin <- if (package_found) "CRAN" else NULL
    
  } else if (database == "bioconductor") {
    # Check only Bioconductor
    if (verbose) {
      cat("    Searching in Bioconductor repository...\n")
    }
    package_found <- is_bioc_package(pkg)
    package_origin <- if (package_found) "Bioconductor" else NULL
    
  } else {
    # Auto-detect (default behavior)
    if (verbose) {
      cat("    Auto-detecting package repository...\n")
    }
    
    # Determine origin with exact case first
    if (is_bioc_package(pkg)) {
      package_origin <- "Bioconductor"
      package_found <- TRUE
    } else {
      # Check CRAN
      repos <- getOption("repos")
      if (length(repos) == 0 || isTRUE(is.na(repos)) || repos == "@CRAN@") {
        repos <- c(CRAN = "https://cloud.r-project.org")
      }
      ap <- tryCatch(utils::available.packages(repos = repos), error = function(e) NULL)
      package_found <- !is.null(ap) && nrow(ap) > 0 && pkg %in% rownames(ap)
      package_origin <- if (package_found) "CRAN" else NULL
    }
  }
  
  # Report package origin in verbose mode
  if (verbose) {
    if (!is.null(package_origin)) {
      cat("    Package origin detected:", package_origin, "\n")
    } else {
      cat("    Package not found with exact case. Trying case-insensitive search...\n")
    }
  }
  
  # Error if package not found in specified repository
  if (!package_found) {
    # Try case-insensitive search
    case_insensitive_result <- try_case_insensitive_search(pkg, database, verbose)
    
    if (!is.null(case_insensitive_result)) {
      # Found a match with different case
      package_found <- TRUE
      package_origin <- case_insensitive_result$origin
      pkg <- case_insensitive_result$correct_name  # Update package name to correct case
      
      if (verbose) {
        cat("    Found package with different case:", pkg, "in", package_origin, "\n")
      }
    } else {
      # Still not found
      if (database == "cran") {
        stop(paste0("Package '", pkg, "' not found on CRAN. ",
                    "Is your package written in another language than R?"))
      } else if (database == "bioconductor") {
        stop(paste0("Package '", pkg, "' not found on Bioconductor. ",
                    "Is your package written in another language than R?"))
      } else {
        stop(paste0("Package '", pkg, "' not found on CRAN or Bioconductor. ",
                    "Is your package written in another language than R?"))
      }
    }
  }
  
  out <- list(
    pkg = pkg,
    title = NULL,
    authors = character(0),
    year = as.character(format(Sys.Date(), "%Y")),
    url = paste0("https://CRAN.R-project.org/package=", pkg),
    version = NULL,
    doi = NULL,
    notes = NULL,
    ris = NULL,
    bibtex = NULL,
    repository = package_origin
  )
  
  # Set URL based on detected origin
  if (package_origin == "Bioconductor") {
    out$url <- paste0("https://bioconductor.org/packages/", pkg)
  }
  
  if (is_installed(pkg)) {
    cit <- tryCatch(utils::citation(pkg), error = function(e) NULL)
    desc <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
    if (!is.null(desc)) {
      out$version <- safely_get(desc, "Version", default = NULL)
      # Prefer package Title from DESCRIPTION if available, but ensure package name is included
      desc_title <- safely_get(desc, "Title", default = NULL)
      if (!is.null(desc_title) && nzchar(desc_title)) {
        # Check if title already includes package name
        if (!grepl(paste0("^", pkg, "\\s*:"), desc_title, ignore.case = TRUE)) {
          out$title <- paste0(pkg, ": ", desc_title)
        } else {
          out$title <- desc_title
        }
      }
      urld <- safely_get(desc, "URL", default = NULL)
      if (!is.null(urld) && nzchar(urld)) {
        out$url <- strsplit(urld, "[ ,]")[[1]][1]
      }
    }
    if (!is.null(cit)) {
      # Use the first bibentry as primary
      be <- cit[[1]]
      if (is.null(out$title) || !nzchar(out$title)) {
        cit_title <- safely_get(be, "title", default = pkg)
        if (!is.null(cit_title) && nzchar(cit_title)) {
          # Check if title already includes package name
          if (!grepl(paste0("^", pkg, "\\s*:"), cit_title, ignore.case = TRUE)) {
            out$title <- paste0(pkg, ": ", cit_title)
          } else {
            out$title <- cit_title
          }
        } else {
          out$title <- pkg
        }
      }
      au <- authors_from_bibentry(be)
      if (length(au) > 0) out$authors <- au
      out$year <- year_from_bibentry(be)
      doi <- safely_get(be, "doi", default = NULL)
      if (!is.null(doi) && nzchar(doi)) out$doi <- as.character(doi)
    }
    
    # Try to get DOI from package metadata if not found in citation
    if (is.null(out$doi) || !nzchar(out$doi)) {
      out$doi <- get_doi_from_package(pkg)
    }
    
    # Get GitHub information for URL, but use repository DOI
    github_info <- get_github_info(pkg, out$url)
    if (!is.null(github_info$github_url) && nzchar(github_info$github_url)) {
      out$url <- github_info$github_url
    }
    
    # For Bioconductor packages, prioritize repository DOI over citation DOI
    # For CRAN packages, use repository DOI only if no citation DOI found
    if (package_origin == "Bioconductor") {
      repo_doi <- get_repository_doi(pkg, package_origin)
      if (!is.null(repo_doi) && nzchar(repo_doi)) {
        out$doi <- repo_doi
      }
    } else {
      # Use repository DOI only if no DOI found yet
      if (is.null(out$doi) || !nzchar(out$doi)) {
        repo_doi <- get_repository_doi(pkg, package_origin)
        if (!is.null(repo_doi) && nzchar(repo_doi)) {
          out$doi <- repo_doi
        }
      }
    }
    
  } else {
    # Not installed: fall back to repository metadata
    if (package_origin == "Bioconductor") {
      meta <- bioc_meta_for(pkg)
    } else {
      meta <- cran_meta_for(pkg)
    }
    
    if (!is.null(meta)) {
      meta_title <- safely_get(meta, "Title", default = pkg)
      if (!is.null(meta_title) && nzchar(meta_title)) {
        # Check if title already includes package name
        if (!grepl(paste0("^", pkg, "\\s*:"), meta_title, ignore.case = TRUE)) {
          out$title <- paste0(pkg, ": ", meta_title)
        } else {
          out$title <- meta_title
        }
      } else {
        out$title <- pkg
      }
      out$version <- safely_get(meta, "Version", default = NULL)
      urlc <- safely_get(meta, "URL", default = NULL)
      if (!is.null(urlc) && nzchar(urlc)) {
        out$url <- strsplit(urlc, "[ ,]")[[1]][1]
      }
      au_txt <- safely_get(meta, "Author", default = NULL)
      au <- parse_author_text(au_txt)
      if (length(au) > 0) out$authors <- au
      
      # Get GitHub information for URL, but use repository DOI for uninstalled packages
      github_info <- get_github_info(pkg, out$url)
      if (!is.null(github_info$github_url) && nzchar(github_info$github_url)) {
        out$url <- github_info$github_url
      }
      
      # Use repository DOI instead of GitHub DOI
      if (is.null(out$doi) || !nzchar(out$doi)) {
        repo_doi <- get_repository_doi(pkg, package_origin)
        if (!is.null(repo_doi) && nzchar(repo_doi)) {
          out$doi <- repo_doi
        }
      }
    } else {
      out$title <- pkg
    }
  }
  if (is.null(out$title) || !nzchar(out$title)) out$title <- pkg

  # RIS from collected fields
  publisher <- if (package_origin == "Bioconductor") "Bioconductor" else "Comprehensive R Archive Network (CRAN)"
  out$ris <- make_ris_for_software_internal(
    title = out$title,
    authors = out$authors,
    year = out$year,
    url = out$url,
    version = out$version,
    doi = out$doi,
    notes = out$notes,
    publisher = publisher
  )

  # Always use synthesized BibTeX to ensure consistency with RIS format
  key <- normalize_key(paste0(pkg, "_", out$year))
  out$bibtex <- synthesize_bibtex_internal(
    key = key,
    title = out$title,
    authors = out$authors,
    year = out$year,
    url = out$url,
    version = out$version,
    doi = out$doi
  )
  out
}

#' Generate RIS format citation for software (internal)
#' @param title Software title
#' @param authors Character vector of author names
#' @param year Publication year
#' @param url Software URL (optional)
#' @param version Software version (optional)
#' @param doi DOI (optional)
#' @param notes Additional notes (optional)
#' @param publisher Publisher name (default: "Comprehensive R Archive Network (CRAN)")
#' @return Character string with RIS formatted citation
make_ris_for_software_internal <- function(title, authors, year, url = NULL, version = NULL, doi = NULL, notes = NULL, publisher = "Comprehensive R Archive Network (CRAN)") {
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
  paste0(lines, collapse = "\n")
}

#' Generate BibTeX format citation for software (internal)
#' @param key BibTeX key for the entry
#' @param title Software title
#' @param authors Character vector of author names
#' @param year Publication year
#' @param url Software URL (optional)
#' @param version Software version (optional)
#' @param doi DOI (optional)
#' @return Character string with BibTeX formatted citation
synthesize_bibtex_internal <- function(key, title, authors, year, url = NULL, version = NULL, doi = NULL) {
  # Minimal @software entry; many BibTeX styles accept @misc
  author_field <- if (length(authors) > 0) paste(authors, collapse = " and ") else ""
  fields <- c(
    if (nzchar(author_field)) paste0("  author = {", author_field, "}") else NULL,
    if (!is.null(title) && nzchar(title)) paste0("  title = {", title, "}") else NULL,
    if (!is.null(year) && nzchar(year)) paste0("  year = {", year, "}") else NULL,
    if (!is.null(url) && nzchar(url)) paste0("  url = {", url, "}") else NULL,
    if (!is.null(doi) && nzchar(doi)) paste0("  doi = {", doi, "}") else NULL,
    if (!is.null(version) && nzchar(version)) paste0("  note = {R package version ", version, "}") else NULL
  )
  entry <- c(paste0("@misc{", key, ","), paste0(fields, ","), "}")
  paste0(entry, collapse = "\n")
}

#' Emit citation outputs in specified formats (internal)
#' @param results List of package citation results
#' @param format Output format: "ris", "bib", "bibtex", or "both"
#' @param out Output file path or directory
#' @param split Write one file per package when multiple packages or format="both"
#' @param overwrite Allow overwriting existing files
#' @return Invisible TRUE
emit_outputs_internal <- function(results, format, out, split, overwrite) {
  formats <- switch(format,
                    ris    = c("ris"),
                    bibtex = c("bibtex"),
                    both   = c("ris", "bibtex")
  )

  if (!nzchar(out)) {
    # stdout mode: combined
    for (r in results) {
      if ("ris" %in% formats) {
        cat(r$ris, "\n")
      }
      if ("bibtex" %in% formats) {
        cat(r$bibtex, "\n\n")
      }
    }
    return(invisible(TRUE))
  }

  # File/directory mode
  if (split || dir.exists(out)) {
    # Per-package files inside a directory
    ensure_dir(out)
    for (r in results) {
      base <- file.path(out, r$pkg)
      if ("ris" %in% formats) {
        write_text(paste0(base, ".ris"), paste0(r$ris, "\n"), overwrite)
      }
      if ("bibtex" %in% formats) {
        write_text(paste0(base, ".bib"), paste0(r$bibtex, "\n"), overwrite)
      }
    }
  } else {
    # Single combined file
    combined <- c()
    for (r in results) {
      if ("ris" %in% formats) combined <- c(combined, r$ris, "")
      if ("bibtex" %in% formats) combined <- c(combined, r$bibtex, "")
    }
    write_text(out, paste0(combined, collapse = "\n"), overwrite)
  }
  invisible(TRUE)
}

# Legacy function for backward compatibility
checkInput <- function(x, name, default = NULL) {
  safely_get(x, name, default)
}