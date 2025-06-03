# IsoRanker_viz

1. **Shiny App (R Package)** - A Shiny-based interactive visualization tool provided as an R package.

## Installation and Usage

### **R Shiny App**
Install the R package from the provided tar.gz file in the repository:

```r
install.packages("remotes")
remotes::install_local("IsoRankershiny_1.0.tar.gz", dependencies = TRUE)
```

After installation, set up an environment variable *ISORANKER_OUTPUT_DIR*, which is the path to the [IsoRanker](https://github.com/yhhc2/IsoRanker/tree/main) Output directory where the separated_results (tsv files) reside.

In the R console:

```r
Sys.setenv(ISORANKER_OUTPUT_DIR = "C:/Users/IsoRanker/examples/Output/Output")
```

Then, launch the Shiny app using:

```r
IsoRankershiny::run_app()
```

Feel free to contribute or report issues via GitHub! 🚀
