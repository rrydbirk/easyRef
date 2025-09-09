library(shiny)
library(easyRef)

# UI
ui <- fluidPage(
  titlePanel("EasyRef - R Package Citation Generator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Package Input"),
      textAreaInput("packages", 
                   "Enter package names (one per line):", 
                   placeholder = "ggplot2\ndplyr\nBiobase",
                   rows = 5),
      
      h4("Output Options"),
      selectInput("format", 
                  "Output Format:", 
                  choices = list("RIS" = "ris", 
                                "BibTeX" = "bibtex", 
                                "Both" = "both"),
                  selected = "ris"),
      
      textInput("filename", 
                "Output filename (optional):", 
                placeholder = "my_citations"),
      
      checkboxInput("overwrite", 
                    "Overwrite existing files", 
                    value = TRUE),
      
      br(),
      actionButton("generate", 
                   "Generate Citations", 
                   class = "btn-primary btn-lg"),
      
      br(), br(),
      h4("Instructions"),
      p("1. Enter R package names (one per line)"),
      p("2. Select output format"),
      p("3. Optionally specify a filename"),
      p("4. Click 'Generate Citations'"),
      p("5. Download the generated files")
    ),
    
    mainPanel(
      h4("Status"),
      verbatimTextOutput("status"),
      
      br(),
      h4("Generated Citations"),
      verbatimTextOutput("citations"),
      
      br(),
      h4("Download Files"),
      uiOutput("download_buttons")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store results and status
  results <- reactiveValues(data = NULL, status_messages = character(0))
  
  # Status output
  output$status <- renderText({
    if (length(results$status_messages) == 0) {
      "Ready to generate citations..."
    } else {
      paste(results$status_messages, collapse = "\n")
    }
  })
  
  # Citations output
  output$citations <- renderText({
    if (is.null(results$data)) {
      "No citations generated yet."
    } else {
      citation_text <- character(0)
      for (i in seq_along(results$data)) {
        result <- results$data[[i]]
        citation_text <- c(citation_text, 
                          paste0("=== ", result$pkg, " ==="),
                          if (input$format %in% c("ris", "both")) result$ris else NULL,
                          if (input$format %in% c("bibtex", "both")) result$bibtex else NULL,
                          "")
      }
      paste(citation_text, collapse = "\n")
    }
  })
  
  # Download buttons
  output$download_buttons <- renderUI({
    if (is.null(results$data)) {
      return(NULL)
    }
    
    buttons <- list()
    
    if (input$format %in% c("ris", "both")) {
      buttons <- c(buttons, list(
        downloadButton("download_ris", "Download RIS File", class = "btn-success")
      ))
    }
    
    if (input$format %in% c("bibtex", "both")) {
      buttons <- c(buttons, list(
        downloadButton("download_bibtex", "Download BibTeX File", class = "btn-info")
      ))
    }
    
    if (length(buttons) > 0) {
      do.call(tagList, buttons)
    }
  })
  
  # Generate citations when button is clicked
  observeEvent(input$generate, {
    # Clear previous results
    results$data <- NULL
    results$status_messages <- character(0)
    
    # Get package names
    pkg_text <- input$packages
    if (is.null(pkg_text) || nchar(trimws(pkg_text)) == 0) {
      results$status_messages <- "Please enter at least one package name."
      return()
    }
    
    # Parse package names
    packages <- trimws(unlist(strsplit(pkg_text, "\n")))
    packages <- packages[nchar(packages) > 0]
    
    if (length(packages) == 0) {
      results$status_messages <- "Please enter at least one package name."
      return()
    }
    
    # Update status
    results$status_messages <- paste("Finding", length(packages), "references...")
    
    # Generate citations with progress updates
    tryCatch({
      # Create a modified version of createRef that shows warnings instead of errors
      citation_results <- list()
      successful_count <- 0
      
      for (i in seq_along(packages)) {
        pkg <- packages[i]
        
        # Update status for current package
        results$status_messages <- c(results$status_messages, 
                                   paste("Processing package:", pkg))
        
        # Try to get citation for this package
        tryCatch({
          # Use the main createRef function for a single package
          result_list <- createRef(pkg, format = input$format, verbose = FALSE)
          # createRef returns a list, so we take the first (and only) element
          result <- result_list[[1]]
          citation_results <- c(citation_results, list(result))
          successful_count <- successful_count + 1
          
          # Update status
          results$status_messages <- c(results$status_messages, 
                                     paste("Created", successful_count, "of", length(packages), "files"))
          
        }, error = function(e) {
          # Show warning instead of error
          warning_msg <- paste("Warning: Could not find package '", pkg, 
                              "'. Skipping this package. Error: ", e$message)
          results$status_messages <- c(results$status_messages, warning_msg)
          warning(warning_msg)
        })
      }
      
      # Store results
      results$data <- citation_results
      
      # Final status update
      if (length(citation_results) > 0) {
        results$status_messages <- c(results$status_messages, 
                                    paste("Successfully generated", length(citation_results), "citations"))
      } else {
        results$status_messages <- c(results$status_messages, 
                                    "No citations were generated. Please check package names.")
      }
      
    }, error = function(e) {
      results$status_messages <- c(results$status_messages, 
                                 paste("Error:", e$message))
    })
  })
  
  # Download handlers
  output$download_ris <- downloadHandler(
    filename = function() {
      if (nchar(input$filename) > 0) {
        paste0(input$filename, ".ris")
      } else {
        "citations.ris"
      }
    },
    content = function(file) {
      if (!is.null(results$data)) {
        ris_content <- character(0)
        for (result in results$data) {
          ris_content <- c(ris_content, result$ris, "")
        }
        writeLines(ris_content, file)
      }
    }
  )
  
  output$download_bibtex <- downloadHandler(
    filename = function() {
      if (nchar(input$filename) > 0) {
        paste0(input$filename, ".bib")
      } else {
        "citations.bib"
      }
    },
    content = function(file) {
      if (!is.null(results$data)) {
        bib_content <- character(0)
        for (result in results$data) {
          bib_content <- c(bib_content, result$bibtex, "")
        }
        writeLines(bib_content, file)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)