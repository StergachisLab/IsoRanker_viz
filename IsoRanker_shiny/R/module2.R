module2UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      h4("NMD Decay Analysis"),

      # Row 1: Transcript type and criteria selection
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("nmd_transcripts"),
            label = "Select transcript type:",
            choices = c("Gene", "Isoform"),
            selected = "Gene"
          )
        ),
        column(
          width = 4,
          # When transcript type is Gene, two criteria are available;
          # when Isoform is selected, only "Uniquely NMD‑subjected" is available.
          conditionalPanel(
            condition = paste0("input['", ns("nmd_transcripts"), "'] == 'Gene'"),
            selectInput(
              ns("select_criteria"),
              label = "Select criteria:",
              choices = c("Uniquely NMD-subjected" = "Uniquely",
                          "Rare steady-state transcripts" = "Rare"),
              selected = "Uniquely"
            )
          ),
          conditionalPanel(
            condition = paste0("input['", ns("nmd_transcripts"), "'] == 'Isoform'"),
            selectInput(
              ns("select_criteria"),
              label = "Select criteria:",
              choices = c("Uniquely NMD-subjected" = "Uniquely"),
              selected = "Uniquely"
            )
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("z_score_quantile"),
            label = "Z-score quantile:",
            choices = c("95%" = "rank_top_95_percentile",
                        "98%" = "rank_top_98_percentile",
                        "99%" = "rank_top_99_percentile",
                        "99.5%" = "rank_top_99_5_percentile",
                        "99.9%" = "rank_top_99_9_percentile"),
            selected = "rank_top_99_percentile"
          )
        )
      ),

      # Row 2: Sample selection, gene search, and render button
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            ns("select_sample"),
            label = "Select sample:",
            choices = c("All" = "All"),
            selected = "All",
            options = list(placeholder = 'Select sample:')
          )
        ),
        column(
          width = 4,
          textInput(
            ns("find_gene"),
            label = "Find gene in plot:",
            placeholder = "Enter gene name..."
          )
        ),
        column(
          width = 4,
          actionButton(ns("plot_button"), "Render", class = "btn-primary btn-sm")
        )
      ),

      br(),

      # Plot output: one plot for NMD Decay
      tabsetPanel(
        tabPanel("NMD Decay Plot", plotlyOutput(ns("nmd_plot"), height = "400px"))
      ),

      br(),

      # Table output: one table for NMD Decay
      tabsetPanel(
        tabPanel("NMD Decay Table", DT::DTOutput(ns("nmd_table")))
      )
    )
  )
}

module2Server <- function(input, output, session, data) {
  ns <- session$ns

  # Restore inputs when a bookmark is used
  onRestore(function(state) {
    updateSelectInput(session, ns("nmd_transcripts"), selected = state$input$nmd_transcripts)
    updateSelectInput(session, ns("select_criteria"), selected = state$input$select_criteria)
    updateSelectInput(session, ns("z_score_quantile"), selected = state$input$z_score_quantile)
    updateSelectizeInput(session, ns("select_sample"), selected = state$input$select_sample)
    updateTextInput(session, ns("find_gene"), value = state$input$find_gene)
  })

  # Update the 'Select Sample' input based on the matching NMD Decay file.
  observe({
    req(data())
    # Determine transcript type: "Gene" or "Isoform"
    trans <- input$nmd_transcripts
    # Define file choices based on transcript type:
    if (trans == "Gene") {
      matches <- c("NMD_gene_top_ranked_data.", "NMD_rare_steady_state_transcript_gene_top_ranked_data")
    } else {
      matches <- c("NMD_isoform_top_ranked_data")
    }
    # Only consider those matches that exist in the loaded data.
    matches <- intersect(matches, names(data()))
    if (length(matches) > 0) {
      samples <- unique(unlist(lapply(matches, function(name) {
        df <- data()[[name]]
        if ("Sample" %in% names(df)) df$Sample else NULL
      })))
      samples <- samples[!is.na(samples)]
      samples <- sort(samples)
      updateSelectizeInput(session, "select_sample", choices = c("All" = "All", samples), selected = "All", server = TRUE)
    } else {
      updateSelectizeInput(session, "select_sample", choices = c("All" = "All"), selected = "All", server = TRUE)
    }
  })

  # Snapshot inputs when the Render button is pressed.
  render_data <- eventReactive(input$plot_button, {
    list(
      nmd_transcripts = input$nmd_transcripts,
      select_criteria = input$select_criteria,
      z_score_quantile = input$z_score_quantile,
      select_sample = input$select_sample,
      find_gene = input$find_gene
    )
  })

  # Choose the appropriate data frame based on user selections.
  # For transcript type Gene:
  #   - If criteria is "Uniquely NMD-subjected" then use "NMD_gene_data"
  #   - If criteria is "Rare steady-state transcripts" then use "NMD_rare_steady_state_transcript_gene_data"
  # For transcript type Isoform: use "NMD_isoform_data"
  selected_data <- reactive({
    rd <- render_data()
    req(rd)
    if (rd$nmd_transcripts == "Gene") {
      if (rd$select_criteria == "Uniquely") {
        return(data()[["NMD_gene_top_ranked_data"]])
      } else if (rd$select_criteria == "Rare") {
        return(data()[["NMD_rare_steady_state_transcript_gene_top_ranked_data"]])
      }
    } else if (rd$nmd_transcripts == "Isoform") {
      return(data()[["NMD_isoform_top_ranked_data"]])
    }
    NULL
  })

  # Filter the selected data frame based on sample and gene search.
  filtered_data <- reactive({
    df <- selected_data()
    req(df)
    rd <- render_data()
    if (rd$select_sample != "All") {
      df <- df[df$Sample == rd$select_sample, ]
    }
    # Remove rows with NA in the selected rank column.
    rank_col <- rd$z_score_quantile
    df <- df[!is.na(df[[rank_col]]), ]
    if (nzchar(rd$find_gene)) {
      df <- df[grepl(rd$find_gene, df$associated_gene, ignore.case = TRUE), ]
    }
    df
  })

  # Determine the x-axis column based on the selected data frame:
  # For NMD_gene_data and NMD_isoform_data, x-axis = "NormalizedFractionDifference"
  # For NMD_rare_steady_state_transcript_gene_data, x-axis = "bin_proportion_difference"
  x_axis_column <- reactive({
    rd <- render_data()
    req(rd)
    if (rd$nmd_transcripts == "Gene" && rd$select_criteria == "Rare") {
      "bin_proportion_difference"
    } else {
      "z_score_of_test_stat"
    }
  })

  # Render the NMD Decay plot.
  output$nmd_plot <- renderPlotly({
    rd <- render_data()  # Snapshot inputs from Render button
    df <- filtered_data()
    req(df)
    x_col <- x_axis_column()
    if (is.null(x_col) || !(x_col %in% names(df))) {
      showNotification("X-axis column not found in the selected data.", type = "error")
      return(NULL)
    }
    df$X_Value <- df[[x_col]]
    plot_ly(
      data = df,
      x = ~X_Value,
      y = ~test_statistic,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, color = 'rgba(54, 162, 235, 0.6)'),
      text = ~paste("Sample:", Sample, "<br>Gene:", associated_gene)
    ) %>%
      layout(
        title = paste("NMD Decay Plot -", rd$select_criteria, rd$nmd_transcripts),
        xaxis = list(title = paste("X-axis (", x_col, ")", sep = "")),
        yaxis = list(title = "Test Statistic")
      )
  })

  # Render the NMD Decay table.
  output$nmd_table <- DT::renderDT({
    df <- filtered_data()
    req(df)
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
