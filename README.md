# IsoRanker_viz

IsoRanker_viz provides two versions for visualizing data:
1. **Shiny App (R Package)** - A Shiny-based interactive visualization tool provided as an R package.
2. **Dash App (Python)** - A Dash-based web application for interactive data visualization.

## Installation and Usage

### **R Shiny App**
To install the R package from the provided tar.gz file in the repository:

```r
install.packages("../IsoRankershiny_1.0.tar.gz", repos = NULL, type = "source")
```

After installation, set up an environment variable *ISORANKER_OUTPUT_DIR*, which is the path to the IsoRanker Output directory where the csv files reside.

In the R console:

```r
Sys.setenv(ISORANKER_OUTPUT_DIR = "C:/Users/IsoRanker/examples/Output/Output")
```

Then, launch the Shiny app using:

```r
IsoRankershiny::run_app()
```

### **Dash App (Python)**
To run the Dash version, first install the required dependencies:

```bash
pip install dash dash-bootstrap-components pandas plotly dash-table
```
Then, run the app:

```bash
python app.py
```
It will point out to an URL that you can access in the Web Browser. Once it's open, please provide the path to the IsoRanker Output directory where the csv files reside. 

```
Example: "C:/Users/IsoRanker/examples/Output/Output"
```

Both versions provide powerful interactive visualizations for exploring IsoRanker data.

Feel free to contribute or report issues via GitHub! 🚀
