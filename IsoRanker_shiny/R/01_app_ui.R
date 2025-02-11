#' The Application User-Interface
#'
#' @param request Internal parameter for shiny
#' @return A Shiny UI object.
#' @importFrom shiny icon tabPanel tabsetPanel conditionalPanel eventReactive observe observeEvent reactive reactiveVal renderText renderUI req callModule actionButton verbatimTextOutput uiOutput fluidRow column h4 br selectInput selectizeInput textInput shinyApp runApp tags
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem box tabBox
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom plotly plotlyOutput renderPlotly plot_ly layout
#' @importFrom skimr skim
#' @importFrom magrittr %>%
#' @importFrom config get
#' @importFrom htmltools tags HTML tagList
#' @export
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Transcript Analysis App"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data Summary & Preview", tabName = "summary", icon = icon("table")),
        menuItem("Expression Outliers", tabName = "expr", icon = icon("chart-line")),
        menuItem("NMD Decay Analysis", tabName = "nmd", icon = icon("chart-area"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "summary",
          fluidRow(
            box(width = 12, title = "Data Loader",
                fluidRow(
                  column(4,
                         shinyDirButton("dir", "Choose Directory", "Select directory", icon = icon("folder-open"))
                  ),
                  column(4,
                         actionButton("refresh_data", "Refresh Data", class = "btn-secondary")
                  ),
                  column(4,
                         verbatimTextOutput("selected_dir")
                  )
                )
            )
          ),
          fluidRow(
            box(width = 12, uiOutput("df_select_ui"))
          ),
          fluidRow(
            box(width = 12,
                tabBox(
                  width = 12,
                  tabPanel("Skimr Summary",
                           h4("Skimr Summary"),
                           DT::DTOutput("skim_summary", width = "800px")
                  ),
                  tabPanel("Data Preview",
                           h4("Data Preview"),
                           DT::DTOutput("data_preview", width = "800px")
                  )
                )
            )
          )
        ),
        tabItem(
          tabName = "expr",
          module1UI("mod1")
        ),
        tabItem(
          tabName = "nmd",
          module2UI("mod2")
        )
      )
    )
  )
}
