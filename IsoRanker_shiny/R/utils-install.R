install_missing_packages <- function() {
  if (!requireNamespace("config", quietly = TRUE)) utils::install.packages("config")
  deps <- config::get("dependencies", file = system.file("golem-config.yml", package = "IsoRankershiny"))
  
  install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing missing package:", pkg))
      utils::install.packages(pkg, dependencies = TRUE)
    }
  }
  
  sapply(deps, install_if_missing)
}
