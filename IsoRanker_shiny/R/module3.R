module3UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      h4("Allelic Imbalance Analysis"),
      
      fluidRow(
        column(6,
               selectizeInput(
                 ns("sample_select"),
                 label = "Select sample:",
                 choices = NULL,
                 options = list(placeholder = "Choose sample")
               )
        ),
        column(6,
               textInput(
                 ns("gene_query"),
                 "Find gene:",
                 placeholder = "Enter gene name..."
               )
        )
      ),
      
      fluidRow(
        column(4,
               actionButton(ns("render_button"), "Render", class = "btn-primary btn-sm")
        )
      ),
      
      br(),
      
      tabBox(
        width = 12,
        tabPanel("Cyclo Plot", plotlyOutput(ns("cyclo_plot"), height = "400px")),
        tabPanel("Noncyclo Plot", plotlyOutput(ns("noncyclo_plot"), height = "400px"))
      ),
      
      br(),
      
      tabBox(
        width = 12,
        tabPanel("Cyclo Data Table", DT::DTOutput(ns("cyclo_table"))),
        tabPanel("Noncyclo Data Table", DT::DTOutput(ns("noncyclo_table")))
      )
    )
  )
}

module3Server <- function(input, output, session, data) {
  ns <- session$ns
  

  # Update sample choices (combined from both datasets)
  observe({
    req(data())
    samples <- unique(c(
      data()[["Cyclo_Allelic_Imbalance_gene_top_ranked_data"]]$Sample,
      data()[["Noncyclo_Allelic_Imbalance_gene_top_ranked_data"]]$Sample
    ))
    updateSelectizeInput(session, "sample_select", choices = sort(samples), server = TRUE)
  })
  
  filters <- eventReactive(input$render_button, {
    list(
      sample = input$sample_select,
      gene = input$gene_query
    )
  })
  
  filter_df <- function(df, sample, gene) {
    if (!is.null(sample) && sample != "") {
      df <- df[df$Sample == sample, ]
    }
    if (!is.null(gene) && nzchar(gene)) {
      df <- df[grepl(gene, df$associated_gene, ignore.case = TRUE), ]
    }
    df
  }
  
  filtered_cyclo <- reactive({
    req(data(), filters())
    df <- data()[["Cyclo_Allelic_Imbalance_gene_top_ranked_data"]]
    filter_df(df, filters()$sample, filters()$gene)
  })
  
  filtered_noncyclo <- reactive({
    req(data(), filters())
    df <- data()[["Noncyclo_Allelic_Imbalance_gene_top_ranked_data"]]
    filter_df(df, filters()$sample, filters()$gene)
  })
  
  
  observeEvent(filtered_noncyclo(), {
    message("Filtered noncyclo rows: ", nrow(filtered_noncyclo()))
  })
  
  observeEvent(filtered_noncyclo(), {
    df <- filtered_noncyclo()
    message("[Noncyclo] Rows: ", nrow(df))
    message("[Noncyclo] Cols: ", paste(colnames(df), collapse = ", "))
    message("[Noncyclo] First 1 row:\n", capture.output(print(head(df, 1))))
  })
  
  
  output$cyclo_plot <- renderPlotly({
    df <- filtered_cyclo()
    req(df)
    plot_ly(
      data = df,
      x = ~CycloHaplotypeDifference,
      y = ~z_score_of_test_stat,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, color = 'rgba(54, 162, 235, 0.6)'),
      text = ~paste("Gene:", associated_gene, "<br>Sample:", Sample)
    ) %>%
      layout(
        title = "Cyclo Allelic Imbalance",
        xaxis = list(title = "CycloHaplotypeDifference"),
        yaxis = list(title = "Z-score")
      )
  })
  
  output$noncyclo_plot <- renderPlotly({
    df <- filtered_noncyclo()
    req(df)
    plot_ly(
      data = df,
      x = ~NoncycloHaplotypeDifference,
      y = ~z_score_of_test_stat,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, color = 'rgba(255, 99, 132, 0.6)'),
      text = ~paste("Gene:", associated_gene, "<br>Sample:", Sample)
    ) %>%
      layout(
        title = "Noncyclo Allelic Imbalance",
        xaxis = list(title = "NoncycloHaplotypeDifference"),
        yaxis = list(title = "Z-score")
      )
  })
  
  output$cyclo_table <- DT::renderDT({
    req(filtered_cyclo())
    datatable(filtered_cyclo(), options = list(scrollX = TRUE, pageLength = 20))
  })
  
  output$noncyclo_table <- DT::renderDT({
    req(filtered_noncyclo())
    datatable(filtered_noncyclo(), options = list(scrollX = TRUE, pageLength = 20))
  })
}
