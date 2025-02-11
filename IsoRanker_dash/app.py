import os
import glob
import re
import pandas as pd

from dash import Dash, dcc, html, Input, Output, State, dash_table
import dash_bootstrap_components as dbc
import plotly.express as px

###############################################################################
# 1) Initialize app with Dash Bootstrap theme
###############################################################################
app = Dash(
    __name__, 
    external_stylesheets=[dbc.themes.PULSE],
    suppress_callback_exceptions=True
)
server = app.server  # for deployment if needed

###############################################################################
# 2) Define the layout with bootstrap Container, Rows, and Columns
###############################################################################
app.layout = dbc.Container([
    # Title row
    dbc.Row([
        dbc.Col(
            html.H1("IsoRanker Browser", className="text-center text-primary mt-4 mb-4"),
            width=12
        )
    ]),
    
    # Directory input and file load row
    dbc.Row([
        dbc.Col([
            html.Label("Output Directory:", className="fw-bold"),
            dcc.Input(
                id="output-dir",
                type="text",
                placeholder="Enter output directory path...",
                style={"width": "100%"},
                className="mb-2"
            ),
            dbc.Button("Load Data", id="load-data-btn", color="primary", className="mb-2"),
            html.Div(id="dir-status", className="text-info")
        ], width=6),
        
        dbc.Col([
            html.Label("Select CSV File:", className="fw-bold"),
            dcc.Dropdown(id="file-dropdown", options=[], value=None, style={"width": "100%"})
        ], width=6)
    ], className="mb-4"),
    
    # Tabs
    dcc.Tabs(id="tabs", value="summary", children=[
        dcc.Tab(label="Data Summary & Preview", value="summary"),
        dcc.Tab(label="Expression Outliers", value="expr"),
        dcc.Tab(label="NMD Decay Analysis", value="nmd")
    ]),
    
    html.Br(),
    
    # Content area
    html.Div(id="tab-content", className="mt-3"),
    
], fluid=True)  # fluid container for full-width layout

###############################################################################
# 3) Callback: Load CSV file list from directory
###############################################################################
@app.callback(
    [Output("file-dropdown", "options"), Output("file-dropdown", "value"), Output("dir-status", "children")],
    [Input("load-data-btn", "n_clicks")],
    [State("output-dir", "value")]
)
def update_file_list(n_clicks, output_dir):
    if not n_clicks or not output_dir:
        return [], None, "Please enter a valid directory and click Load Data."
    
    if not os.path.isdir(output_dir):
        return [], None, "Invalid directory path."
    
    # Search for CSV files (case-insensitive) in that directory
    pattern = os.path.join(output_dir, "**", "*.csv")
    files = glob.glob(pattern, recursive=True)
    if not files:
        pattern2 = os.path.join(output_dir, "**", "*.CSV")
        files = glob.glob(pattern2, recursive=True)
    if not files:
        return [], None, "No CSV files found in this directory."
    
    file_options = [{"label": os.path.basename(f), "value": f} for f in files]
    return file_options, None, f"Found {len(files)} CSV file(s)."

###############################################################################
# 4) Main callback to render tab content when a tab or file is selected
###############################################################################
@app.callback(
    Output("tab-content", "children"),
    [Input("tabs", "value"), Input("file-dropdown", "value")]
)
def render_tab(selected_tab, selected_file):
    """Renders the content area based on the current tab and selected file."""
    if not selected_file:
        return html.Div("Please select a file above.", className="text-danger")
    
    # Try reading the file
    try:
        df = pd.read_csv(selected_file)
    except Exception as e:
        return html.Div(f"Error reading file: {e}", className="text-danger")
    
    # Summary & Preview tab
    if selected_tab == "summary":
        return render_summary_tab(df)
    
    # Expression Outliers tab
    elif selected_tab == "expr":
        return render_expr_tab(df, selected_file)
    
    # NMD Decay Analysis tab
    elif selected_tab == "nmd":
        return render_nmd_tab(df, selected_file)
    
    return html.Div("Invalid tab selection.", className="text-danger")

###############################################################################
# 5) Render Functions for each tab
###############################################################################
def render_summary_tab(df):
    """Render the data summary & preview tables."""
    if df.empty:
        return html.Div("This file is empty.", className="text-warning")
    
    summary_df = df.describe().reset_index()
    preview_df = df.head(20)
    
    return html.Div([
        html.H4("Data Summary (Numeric)", className="text-primary"),
        dash_table.DataTable(
            data=summary_df.to_dict("records"),
            columns=[{"name": i, "id": i} for i in summary_df.columns],
            filter_action="native",  # Allows user to filter/search
            fixed_rows={'headers': True},
            fixed_columns={'headers': True},
            sort_action="native",
            style_table={"overflowX": "auto", "height": "400px", "overflowY": "auto"},
            style_header={"backgroundColor": "#17141f", "fontWeight": "bold", "color": "white"},
            style_cell={"padding": "8px", "whiteSpace": "normal", "textAlign": "left", "width": "250px"},
            style_data={"whiteSpace": "normal"}
        ),
        
        html.H4("Data Preview (First 20 Rows)", className="text-primary mt-3"),
        dash_table.DataTable(
            data=preview_df.to_dict("records"),
            columns=[{"name": i, "id": i} for i in df.columns],
            filter_action="native",
            sort_action="native",
            fixed_rows={'headers': True},
            fixed_columns={'headers': True},
            style_table={"overflowX": "auto", "height": "400px", "overflowY": "auto"},
            style_header={"backgroundColor": "#17141f", "fontWeight": "bold", "color": "white"},
            style_cell={"padding": "8px", "whiteSpace": "normal", "textAlign": "left", "width": "250px"},
            style_data={"whiteSpace": "normal"}
        )
    ])

def render_expr_tab(df, file_path):
    """Return layout for Expression Outliers tab with dropdowns for z-score, sample, find gene, plus 'Render' button."""
    # We'll build a small UI within the tab. We'll have the user choose a z-score quantile, sample, gene search,
    # then we display a render button. Clicking it triggers a callback that creates the plot & table.
    
    # Create sub-layout
    return html.Div([
        html.H4("Expression Outliers Analysis", className="text-primary"),
        
        # Z-score quantile, sample, gene search, and Render button
        dbc.Row([
            dbc.Col([
                html.Label("Z-score Quantile:", className="fw-bold"),
                dcc.Dropdown(
                    id="expr-z-score-quantile",
                    options=[
                        {"label": "95%", "value": "rank_top_95_percentile"},
                        {"label": "98%", "value": "rank_top_98_percentile"},
                        {"label": "99%", "value": "rank_top_99_percentile"},
                        {"label": "99.5%", "value": "rank_top_99_5_percentile"},
                        {"label": "99.9%", "value": "rank_top_99_9_percentile"}
                    ],
                    value="rank_top_99_percentile",
                    style={"width": "100%"}
                )
            ], width=3),
            
            dbc.Col([
                html.Label("Select Sample:", className="fw-bold"),
                dcc.Dropdown(
                    id="expr-sample",
                    options=build_sample_options(df),
                    value="All",
                    style={"width": "100%"}
                )
            ], width=3),
            
            dbc.Col([
                html.Label("Gene Search:", className="fw-bold"),
                dcc.Input(id="expr-find-gene", type="text", placeholder="Enter gene name...", style={"width": "100%"})
            ], width=3),
            
            dbc.Col([
                html.Br(),
                dbc.Button("Render Plot", id="expr-render-btn", color="primary", className="mt-2")
            ], width=3)
        ], className="mb-3"),
        
        # Placeholder for the rendered plot and table
        html.Div(id="expr-content")
    ])

def render_nmd_tab(df, file_path):
    """Similar UI for the NMD Decay Analysis tab: z-score quantile, sample, gene search, plus render."""
    return html.Div([
        html.H4("NMD Decay Analysis", className="text-primary"),
        
        dbc.Row([
            dbc.Col([
                html.Label("Z-score Quantile:", className="fw-bold"),
                dcc.Dropdown(
                    id="nmd-z-score-quantile",
                    options=[
                        {"label": "95%", "value": "rank_top_95_percentile"},
                        {"label": "98%", "value": "rank_top_98_percentile"},
                        {"label": "99%", "value": "rank_top_99_percentile"},
                        {"label": "99.5%", "value": "rank_top_99_5_percentile"},
                        {"label": "99.9%", "value": "rank_top_99_9_percentile"}
                    ],
                    value="rank_top_99_percentile",
                    style={"width": "100%"}
                )
            ], width=3),
            
            dbc.Col([
                html.Label("Select Sample:", className="fw-bold"),
                dcc.Dropdown(
                    id="nmd-sample",
                    options=build_sample_options(df),
                    value="All",
                    style={"width": "100%"}
                )
            ], width=3),
            
            dbc.Col([
                html.Label("Gene Search:", className="fw-bold"),
                dcc.Input(id="nmd-find-gene", type="text", placeholder="Enter gene name...", style={"width": "100%"})
            ], width=3),
            
            dbc.Col([
                html.Br(),
                dbc.Button("Render Plot", id="nmd-render-btn", color="primary", className="mt-2")
            ], width=3)
        ], className="mb-3"),
        
        html.Div(id="nmd-content")
    ])

def build_sample_options(df):
    """Helper to build sample dropdown options from the CSV file."""
    if "Sample" in df.columns:
        samples = sorted(df["Sample"].dropna().unique().tolist())
        return [{"label": "All", "value": "All"}] + [{"label": s, "value": s} for s in samples]
    return [{"label": "All", "value": "All"}]

###############################################################################
# 6) Separate callbacks for Expression Outliers and NMD 'Render' buttons
###############################################################################

@app.callback(
    Output("expr-content", "children"),
    Input("expr-render-btn", "n_clicks"),
    State("file-dropdown", "value"),
    State("expr-z-score-quantile", "value"),
    State("expr-sample", "value"),
    State("expr-find-gene", "value")
)
def update_expr_content(n_clicks, file_path, z_quantile, sample, find_gene):
    """Callback that runs when the user clicks 'Render Plot' in the Expression Outliers tab."""
    if not n_clicks:
        return ""
    try:
        df = pd.read_csv(file_path)
    except Exception as e:
        return html.Div(f"Error reading file: {e}", className="text-danger")
    
    base_name = os.path.basename(file_path).lower()
    # Determine criteria (GOE or LOE)
    if "goe" in base_name:
        criteria = "GOE"
    elif "loe" in base_name:
        criteria = "LOE"
    else:
        return html.Div("Unable to determine GOE/LOE from file name.", className="text-danger")
    
    # Determine outlier type (Gene or Isoform) from file name
    if "gene" in base_name:
        outlier_type = "Gene"
    elif "isoform" in base_name:
        outlier_type = "Isoform"
    else:
        return html.Div("Unable to determine Gene/Isoform from file name.", className="text-danger")
    
    # Check for cyclo or noncyclo
    if "noncyclo" in base_name:
        x_col = "Noncyclo_Z_Score"
    elif "cyclo" in base_name:
        x_col = "Cyclo_Z_Score"
    else:
        return html.Div("Unable to determine Cyclo/Noncyclo from file name.", className="text-danger")
    
    if x_col not in df.columns:
        return html.Div(f"Column '{x_col}' not found.", className="text-danger")

    # Filter by sample
    if sample != "All" and "Sample" in df.columns:
        df = df[df["Sample"] == sample]
    
    # Filter by gene search
    if find_gene and "associated_gene" in df.columns:
        df = df[df["associated_gene"].str.contains(find_gene, case=False, na=False)]
    
    n_rows = df.shape[0]
    
    # Plot
    fig = px.scatter(
        df, x=x_col, y="test_statistic",
        color="Sample" if "Sample" in df.columns else None,
        hover_data=["associated_gene"] if "associated_gene" in df.columns else [],
        title=f"Expression Outliers: {criteria} - {outlier_type} (n={n_rows})",
        template="plotly_white"
    )
    fig.update_layout(
        xaxis_title="Z-Score",
        yaxis_title="Test Statistic",
        title_font=dict(size=20, color="#17141f"),
        margin=dict(l=50, r=50, t=50, b=50),
        plot_bgcolor="#f8f9fa",
        paper_bgcolor="#f8f9fa",
    )
    
    # DataTable with filtering
    return html.Div([
        dcc.Graph(figure=fig),
        html.Hr(),
        html.H5("Filtered Data Table", className="text-primary"),
        dash_table.DataTable(
            data=df.to_dict("records"),
            columns=[{"name": i, "id": i} for i in df.columns],
            filter_action="native",  # Allows user to filter/search
            fixed_rows={'headers': True},
            fixed_columns={'headers': True},
            sort_action="native",
            style_table={"overflowX": "auto", "height": "400px", "overflowY": "auto"},
            style_header={"backgroundColor": "#17141f", "fontWeight": "bold", "color": "white"},
            style_cell={"padding": "8px", "whiteSpace": "normal", "textAlign": "left", "width": "250px"},
            style_data={"whiteSpace": "normal"}
        )
    ])

@app.callback(
    Output("nmd-content", "children"),
    Input("nmd-render-btn", "n_clicks"),
    State("file-dropdown", "value"),
    State("nmd-z-score-quantile", "value"),
    State("nmd-sample", "value"),
    State("nmd-find-gene", "value")
)
def update_nmd_content(n_clicks, file_path, z_quantile, sample, find_gene):
    """Callback for the NMD Decay Analysis tab's 'Render Plot' button."""
    if not n_clicks:
        return ""
    try:
        df = pd.read_csv(file_path)
    except Exception as e:
        return html.Div(f"Error reading file: {e}", className="text-danger")
    
    base_name = os.path.basename(file_path).lower()
    
    # Check for known NMD patterns
    if "nmd_gene_data" in base_name:
        transcript_type = "Gene"
        criteria = "Uniquely"
    elif "nmd_isoform_data" in base_name:
        transcript_type = "Isoform"
        criteria = "Uniquely"
    elif "nmd_rare_steady_state_transcript_gene_data" in base_name:
        transcript_type = "Gene"
        criteria = "Rare"
    else:
        return html.Div("Unable to determine NMD criteria from file name.", className="text-danger")
    
    x_col = "NormalizedFractionDifference"
    if x_col not in df.columns:
        return html.Div(f"Column '{x_col}' not found in the data.", className="text-danger")
    
    # Filter by sample
    if sample != "All" and "Sample" in df.columns:
        df = df[df["Sample"] == sample]
    
    # Filter by gene search
    if find_gene and "associated_gene" in df.columns:
        df = df[df["associated_gene"].str.contains(find_gene, case=False, na=False)]
    
    n_rows = df.shape[0]
    
    fig = px.scatter(
        df, x=x_col, y="test_statistic",
        color="Sample" if "Sample" in df.columns else None,
        hover_data=["associated_gene"] if "associated_gene" in df.columns else [],
        title=f"NMD Decay Analysis: {transcript_type} - {criteria} (n={n_rows})",
        template="plotly_white"
    )
    fig.update_layout(
        xaxis_title="Fraction Diff.",
        yaxis_title="Test Statistic",
        title_font=dict(size=20, color="#17141f"),
        margin=dict(l=50, r=50, t=50, b=50),
        plot_bgcolor="#f8f9fa",
        paper_bgcolor="#f8f9fa"
    )
    
    return html.Div([
        dcc.Graph(figure=fig),
        html.Hr(),
        html.H5("Filtered Data Table", className="text-primary"),
        dash_table.DataTable(
            data=df.to_dict("records"),
            columns=[{"name": i, "id": i} for i in df.columns],
            filter_action="native",
            sort_action="native",
            fixed_rows={'headers': True},
            fixed_columns={'headers': True},
            style_table={"overflowX": "auto", "height": "400px", "overflowY": "auto"},
            style_header={"backgroundColor": "#17141f", "fontWeight": "bold", "color": "white"},
            style_cell={"padding": "8px", "whiteSpace": "normal", "textAlign": "left", "width": "250px"},
            style_data={"whiteSpace": "normal"}
        )
    ])

###############################################################################
# 7) Run the app
###############################################################################
if __name__ == "__main__":
    app.run_server(debug=True)
