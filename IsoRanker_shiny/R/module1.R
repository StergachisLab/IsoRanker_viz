module1UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      h4("Expression Outliers Analysis (LOE/GOE)"),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("select_criteria"),
            label = "Select Criteria:",
            choices = c("Loss Of Expression (LOE)" = "LOE",
                        "Gain Of Expression (GOE)" = "GOE"),
            selected = "LOE"
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("expression_outliers"),
            label = "Expression Outliers:",
            choices = c("Gene", "Isoform"),
            selected = "Gene"
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("z_score_quantile"),
            label = "Z-score Quantile:",
            choices = c("95%" = "rank_top_95_percentile",
                        "98%" = "rank_top_98_percentile",
                        "99%" = "rank_top_99_percentile",
                        "99.5%" = "rank_top_99_5_percentile",
                        "99.9%" = "rank_top_99_9_percentile"),
            selected = "rank_top_99_percentile"
          )
        )
      ),

      # Second row: Sample selection, gene search and render button
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("select_sample"),
            label = "Select Sample:",
            choices = c("All" = "All"),
            selected = "All"
          )
        ),
        column(
          width = 4,
          textInput(
            ns("find_gene"),
            label = "Find Gene in Plot:",
            placeholder = "Enter gene name..."
          )
        ),
        column(
          width = 4,
          actionButton(ns("render_button"), "Render", class = "btn-primary")
        )
      ),

      br(),

      # Plots Tabset: It will be built dynamically based on available data
      uiOutput(ns("plots_ui")),

      br(),

      # Tables Tabset: Built dynamically as well
      uiOutput(ns("tables_ui"))
    )
  )
}


module1Server <- function(input, output, session, data) {
  ns <- session$ns

  # Restore input states for bookmarking
  onRestore(function(state) {
    updateSelectInput(session, ns("select_criteria"), selected = state$input$select_criteria)
    updateSelectInput(session, ns("expression_outliers"), selected = state$input$expression_outliers)
    updateSelectInput(session, ns("z_score_quantile"), selected = state$input$z_score_quantile)
    updateSelectInput(session, ns("select_sample"), selected = state$input$select_sample)
    updateTextInput(session, ns("find_gene"), value = state$input$find_gene)
  })


  # Update sample choices based on the files that match the criteria
  observe({
    req(data())
    # Build a pattern from criteria and outlier type.
    crit <- input$select_criteria       # "LOE" or "GOE"
    outlier <- input$expression_outliers  # "Gene" or "Isoform"
    pattern <- paste0("^(Cyclo|Noncyclo)_", crit, "_", tolower(outlier), "_top_ranked_data")
    matches <- grep(pattern, names(data()), value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) {
      samples <- unique(unlist(lapply(matches, function(name) {
        df <- data()[[name]]
        if ("Sample" %in% names(df)) df$Sample else NULL
      })))
      samples <- samples[!is.na(samples)]
      samples <- sort(samples)
      updateSelectInput(session, "select_sample", choices = c("All" = "All", samples), selected = "All")
    } else {
      updateSelectInput(session, "select_sample", choices = c("All" = "All"), selected = "All")
    }
  })

  # Helper function to prepare subset for a given prefix ("Cyclo" or "Noncyclo")
  prepareSubset <- function(prefix) {
    req(data())
    crit <- input$select_criteria
    outlier <- input$expression_outliers
    # Build filename pattern, e.g., "Cyclo_GOE_gene_data" or "Noncyclo_LOE_isoform_data"
    pattern <- paste0("^", prefix, "_", crit, "_", tolower(outlier), "_top_ranked_data")
    matched_names <- grep(pattern, names(data()), value = TRUE, ignore.case = TRUE)
    if (length(matched_names) == 0) return(NULL)

    df_list <- lapply(matched_names, function(nm) {
      data()[[nm]]
    })
    combined <- do.call(rbind, df_list)

    # Filter by sample if not "All"
    if (input$select_sample != "All") {
      combined <- combined[combined$Sample == input$select_sample, ]
    }
    # Use the selected rank column to filter rows with non-NA values.
    rank_col <- input$z_score_quantile
    combined <- combined[!is.na(combined[[rank_col]]), ]
    # Apply gene search filter if provided.
    if (nzchar(input$find_gene)) {
      combined <- combined[grepl(input$find_gene, combined$associated_gene, ignore.case = TRUE), ]
    }
    return(combined)
  }

  # Use eventReactive to capture a snapshot of user inputs along with the data subsets.
  render_data <- eventReactive(input$render_button, {
    list(
      # Snapshot input values:
      select_criteria = input$select_criteria,
      expression_outliers = input$expression_outliers,
      z_score_quantile = input$z_score_quantile,
      select_sample = input$select_sample,
      find_gene = input$find_gene,
      # Data subsets:
      cyclo = if (input$select_criteria == "GOE") prepareSubset("Cyclo") else NULL,
      noncyclo = prepareSubset("Noncyclo")
    )
  })

  # Render the plots UI dynamically.
  output$plots_ui <- renderUI({
    pd <- render_data()
    tabs <- list()
    if (!is.null(pd$cyclo)) {
      tabs <- c(tabs, list(tabPanel("Cyclo Plot", plotlyOutput(ns("cyclo_plot"), height = "400px"))))
    }
    if (!is.null(pd$noncyclo)) {
      tabs <- c(tabs, list(tabPanel("Noncyclo Plot", plotlyOutput(ns("noncyclo_plot"), height = "400px"))))
    }
    if (length(tabs) == 0) {
      return(h4("No data available for the selected options.", style = "color: red;"))
    }
    do.call(tabsetPanel, tabs)
  })

  # Render the tables UI 
  output$tables_ui <- renderUI({
    pd <- render_data()
    tabs <- list()
    if (!is.null(pd$cyclo)) {
      tabs <- c(tabs, list(tabPanel("Cyclo Table", DT::DTOutput(ns("cyclo_table")))))
    }
    if (!is.null(pd$noncyclo)) {
      tabs <- c(tabs, list(tabPanel("Noncyclo Table", DT::DTOutput(ns("noncyclo_table")))))
    }
    if (length(tabs) == 0) {
      return(h4("No data available for the selected options.", style = "color: red;"))
    }
    do.call(tabsetPanel, tabs)
  })

  # Reusable function to generate plot
  render_zscore_plot <- function(df, title_prefix, select_criteria, expression_outliers, color) {
    req(!is.null(df))
    req("z_score_of_test_stat" %in% names(df))
    
    df$X_Value <- df[["z_score_of_test_stat"]]
    
    plot_ly(
      data = df,
      x = ~X_Value,
      y = ~test_statistic,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, color = color),
      text = ~paste("Sample:", Sample, "<br>Gene:", associated_gene)
    ) %>%
      layout(
        title = paste(title_prefix, "-", select_criteria, expression_outliers),
        xaxis = list(title = "Z Score"),
        yaxis = list(title = "Test Statistic")
      )
  }
  
  # Cyclo Plot
  output$cyclo_plot <- renderPlotly({
    pd <- render_data()
    render_zscore_plot(
      df = pd$cyclo,
      title_prefix = "Cyclo Plot",
      select_criteria = pd$select_criteria,
      expression_outliers = pd$expression_outliers,
      color = 'rgba(255, 99, 132, 0.6)'
    )
  })
  
  # Noncyclo Plot
  output$noncyclo_plot <- renderPlotly({
    pd <- render_data()
    render_zscore_plot(
      df = pd$noncyclo,
      title_prefix = "Noncyclo Plot",
      select_criteria = pd$select_criteria,
      expression_outliers = pd$expression_outliers,
      color = 'rgba(54, 162, 235, 0.6)'
    )
  })
  

  # Render Cyclo Table
  output$cyclo_table <- DT::renderDT({
    pd <- render_data()
    req(!is.null(pd$cyclo))
    df <- pd$cyclo
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25,
                     scrollX = TRUE,
                     scrollY = "200px",
                     paging = TRUE,
                     searching = TRUE,
                     ordering = TRUE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel'))
    )
  })

  # Render Noncyclo Table
  output$noncyclo_table <- DT::renderDT({
    pd <- render_data()
    req(!is.null(pd$noncyclo))
    df <- pd$noncyclo
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25,
                     scrollX = TRUE,
                     scrollY = "200px",
                     paging = TRUE,
                     searching = TRUE,
                     ordering = TRUE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel'))
    )
  })
}

