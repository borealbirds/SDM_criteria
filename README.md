# SDM Criteria Selector

This project contains a Shiny app for filtering species distribution model (SDM)
evaluation criteria by application context, model type, and omission/commission
error risk.

## Main App

- `applications_criteria.qmd` contains the Shiny app.
- `applications_criteria.html` is a rendered HTML output.
- `SDM_criteria.Rproj` is the RStudio project file.

Open `applications_criteria.qmd` in RStudio and run the Shiny document to use the
app.

## Data Files

- `model_applications.csv` defines application categories and subcategories,
  suggested omission/commission risk levels, practitioner needs, and examples.
- `model_criteria.csv` defines the SDM criteria inventory, including model-cycle
  stage, assumption descriptions, violation risks, risk levels, potential
  solutions, and model-type flags.
- `glossary_app.csv` defines glossary terms used for clickable definitions in
  the app.
- `tab1_landing_page_intro.docx` provides the landing-page guidance text.

## Expected Model-Type Columns

The app expects these model-type columns in the data:

- `Explanation (Hypothesis testing)`
- `Predictions (spatially explicit)`
- `Predictions (Non-spatially explicit)`
- `Projection (Extrapolation)`

The app standardizes the older `Predictions (Spatially explicit)` capitalization
in `model_criteria.csv` at startup.

## Filtering Logic

The app first filters criteria by selected model type. Criteria are retained when
at least one selected model-type column has a value of `1`.

The app then filters by omission and/or commission error risk. If both risks are
selected, users can choose whether criteria must match either selected risk or
both selected risks.

Suggested risk levels are initialized from the selected application/subcategory
in `model_applications.csv`, but users can adjust them in the sidebar.

## Required R Packages

The app uses:

- `shiny`
- `dplyr`
- `DT`
- `purrr`
- `plotly`
- `stringr`
- `officer`
- `bslib`

## Notes

The app validates required CSV columns at startup and performs a light repair of
common text-encoding artifacts when files contain mojibake.
