#' The application Server
#'
#' This function sets up the data loading and integrates the modules for
#' the IsoRanker Shiny application. It is called internally by run_app.
#' @param input   Shiny input object
#' @param output  Shiny output object
#' @param session Shiny session object
#' @return A Shiny Server object
#'
#' @importFrom shiny observeEvent showNotification reactive reactiveVal renderText renderUI req observe callModule conditionalPanel eventReactive onRestore updateSelectInput updateSelectizeInput updateTextInput
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom DT datatable renderDT
#' @importFrom skimr skim
#' @importFrom utils head read.csv
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly renderPlotly plotlyOutput layout
#' @importFrom skimr skim
#' @importFrom config get
#' @export
app_server <- function(input, output, session) {

  # --- Data Loader Logic ---
  output_dir_path <- Sys.getenv("ISORANKER_OUTPUT_DIR", unset = "/")
  roots <- c(Output = output_dir_path)

  shinyDirChoose(input, "dir", roots = roots, session = session)

  selected_dir <- reactive({
    parseDirPath(roots, input$dir)
  })

  output$selected_dir <- renderText({
    dir <- selected_dir()
    if (length(dir) == 0) {
      "No directory selected."
    } else {
      paste("Selected Directory:", dir)
    }
  })

  loadedData <- reactiveVal(NULL)

  load_data_from_directory <- function(dir_path) {
    file_paths <- list.files(path = dir_path,
                             pattern = "\\.tsv\\.gz$",
                             full.names = TRUE,
                             recursive = TRUE,
                             ignore.case = TRUE)
    if (length(file_paths) == 0) {
      showModal(modalDialog(
        title = "No Files Found",
        "No TSV files were found in the selected directory.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    data_list <- lapply(file_paths, function(file) {
      tryCatch({
        readr::read_tsv(file)
      }, error = function(e) {
        showModal(modalDialog(
          title = "File Read Error",
          paste("Error reading", basename(file), ":", e$message),
          easyClose = TRUE
        ))
        NULL
      })
    })
    data_list <- data_list[!sapply(data_list, is.null)]
    #names(data_list) <- tools::file_path_sans_ext(basename(file_paths))[!sapply(data_list, is.null)]
    names(data_list) <- sub("\\.tsv\\.gz$", "", basename(file_paths))
    loadedData(data_list)
  }

  observeEvent(selected_dir(), {
    dir_path <- selected_dir()
    if (length(dir_path) > 0 && dir_path != "") {
      load_data_from_directory(dir_path)
    }
  })

  observeEvent(input$refresh_data, {
    dir_path <- selected_dir()
    if (length(dir_path) > 0 && dir_path != "") {
      load_data_from_directory(dir_path)
    } else {
      showModal(modalDialog(
        title = "No Directory Selected",
        "Please select a directory first.",
        easyClose = TRUE
      ))
    }
  })

  output$df_select_ui <- renderUI({
    req(loadedData())
    df_names <- names(loadedData())
    if (length(df_names) == 0) {
      tags$div("No data frames available.")
    } else {
      tags$div(
        class = "form-group shiny-input-container",
        tags$label("Select Data Frame"),
        selectInput("df_choice", label = NULL, choices = df_names, selected = df_names[1])
      )
    }
  })

  output$skim_summary <- renderDT({
    req(loadedData(), input$df_choice)
    df <- loadedData()[[input$df_choice]]
    sk_df <- skim(as.data.frame(df))
    datatable(
      sk_df,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25,
                     scrollX = TRUE,
                     scrollY = "600px",
                     autowidth = TRUE,
                     paging = TRUE,
                     searching = TRUE,
                     ordering = TRUE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel'))
    )
  })

  output$data_preview <- renderDT({
    req(loadedData(), input$df_choice)
    df <- loadedData()[[input$df_choice]]
    datatable(
      head(df, 50),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25,
                     scrollX = TRUE,
                     scrollY = "600px",
                     autowidth = TRUE,
                     paging = TRUE,
                     searching = TRUE,
                     ordering = TRUE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel'))
    )
  })

  # --- Module Integration ---
  callModule(module1Server, "mod1", data = reactive(loadedData()))
  callModule(module2Server, "mod2", data = reactive(loadedData()))
  callModule(module3Server, "mod3", data = reactive(loadedData()))
  
}

# Run the app
golem::with_golem_options(
  app = shiny::shinyApp(ui = app_ui, server = app_server),
  golem_opts = list()
)
