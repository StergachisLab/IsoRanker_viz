options(shiny.maxRequestSize = 5 * 1024^3)  # 5 GB

# function used in your UI for info icons
info_icon_with_tooltip <- function(text) {
  tags$span(
    bsicons::bs_icon("info-circle"),
    title = text,
    style = "cursor: pointer; margin-left: 5px;",
    `data-toggle` = "tooltip",
    `data-placement` = "top"
  )
}

# The objects below are used in the module code.
# In practice, you'll load/assign them dynamically after the user provides a path.
isoforms <- list()           # placeholder
labeling_hank <- data.frame(
  name = character(),
  initial = character(),
  new_label = character()
)
sorted_samples <- c()        # placeholder
custom_colors1 <- c("red", "blue", "green", "orange")
initial <- ""
