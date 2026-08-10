# SDM Criteria Selector

This folder contains a Quarto Shiny app for identifying species distribution
model (SDM) evaluation criteria for a selected biodiversity conservation
use-case scenario.

The use-case scenario combines:

- model application category or specific model application
- model type, either `Prediction` or `Projection`
- error severity tolerance
- error certainty tolerance

The app applies filters only after the user clicks `Filter Criteria`.

## Files

In the `app` folder, the `global.R`, `ui.R`, and `server.R` files contain the
Shiny app code.

The `app/data` folder contains the following files:

- `tab1_landing_page_intro.docx` provides the landing-page text for the
  `Overview & Instructions` tab.
- `Applications - Application_inventory.csv` lists specific model applications,
  their application categories, descriptions, and optional example citations.
- `Applications - Application_categories.csv` defines each application category,
  including conservation decision text, SDM use description, and underprediction
  and overprediction error effects.
- `Applications - Application_error.csv` maps application error effects to
  severity ranks.
- `Criteria - Criteria.csv` defines the SDM criteria inventory, including model
  stage, model step, criterion text, descriptions, violation effects, model-type
  certainty values, justifications, and possible solutions.
- `Criteria - Criteria_error.csv` maps criterion error certainty values to
  certainty ranks.
- `glossary_app.csv` defines glossary terms, definitions, and examples shown in
  the `Glossary` tab.

## Running the App

Before running the app, make sure you have the required packages installed. You can install them using the following command:

```R
install.packages(
    c("bslib", "dplyr", "DT", "officer", "plotly", "purrr", "rmarkdown", 
    "shiny", "stringr"))
```

To run the app, you can use the following command in your R console:

```R
shiny::runApp("app")
```

This will launch the Shiny application in your default web browser.

The app expects the CSV and DOCX files listed above to be in the `app/data` folder.

To validate the lookup tables before running the app, use the functions defined in `validate_app_data.R`:

```r
source("validate_app_data.R")
```

## App Workflow

1. Use the sidebar to select a model application category, a specific model
   application, or both. The category and application dropdowns update each
   other.
2. Select whether the model is being used for prediction or projection.
3. Select error severity and error certainty tolerances.
4. Click `Filter Criteria`.

Before filters are applied, all result tabs except `Overview & Instructions`
show a prompt asking the user to select a use-case scenario and click
`Filter Criteria`.

The `Selected use-case scenario` tab summarizes the selected application
category, selected model application if provided, model type, optional user
details, and other applications in the selected category.

The `Applicable Criteria` tab shows the selected error type logic, the list of
selected criteria, criterion details for the selected row, and radar summaries
of selected criteria along the model cycle.

The `Generate a Report` tab provides report metadata inputs and exports the
selected criteria as DOCX or CSV.

The `Glossary` tab displays glossary terms as a searchable table. The
`References` column is hidden, and `Some examples` is shown as `Example`.

## Filtering Logic

Application category and error severity tolerance determine which error types
are considered:

- `Correctness (high tolerance)` includes only error effects ranked `High`.
- `Quality (medium tolerance)` includes error effects ranked `High` or
  `Medium`.
- `Efficiency (low tolerance)` includes error effects ranked `High`, `Medium`,
  or `Low`.

Model type determines which criterion certainty columns are used:

- `Underprediction_prediction_error_certainty`
- `Overprediction_prediction_error_certainty`
- `Underprediction_projection_error_certainty`
- `Overprediction_projection_error_certainty`

Error certainty tolerance then filters criteria using ordinal thresholds:

- `Always (high tolerance)` includes criteria ranked `High`.
- `Sometimes (medium tolerance)` includes criteria ranked `High` or `Medium`.
- `Rarely (low tolerance)` includes criteria ranked `High`, `Medium`, or `Low`.

A criterion is selected when at least one active error type passes the criterion
certainty threshold.

## Data Notes

The app validates required CSV columns at startup and repairs common text
encoding artifacts in character columns. Optional example citation fields in
`Applications - Application_inventory.csv` can contain multiple values separated
with semicolons.

If encoding artifacts appear in the app, re-save the affected CSV as UTF-8. The
runtime repair is a guardrail, but UTF-8 source files are preferred.

## Data Dictionary

Required columns by file:

- `Applications - Application_inventory.csv`: `Application`,
  `Application_category`, `Description`. Optional columns used when present:
  `Example citation`, `Citation URL`.
- `Applications - Application_categories.csv`: `Application_category`,
  `Question`, `Action`, `Underprediction_error_effect`,
  `Overprediction_error_effect`.
- `Applications - Application_error.csv`: `Error_effect`, `Definition`,
  `Error_effect_rank`.
- `Criteria - Criteria.csv`: `Model_stage`, `Model_step`, `Criterion`,
  `Description`, `Violation`, `Underprediction_prediction_error_certainty`,
  `Overprediction_prediction_error_certainty`, `Prediction_justification`,
  `Underprediction_projection_error_certainty`,
  `Overprediction_projection_error_certainty`, `Projection_justification`,
  `Solutions`.
- `Criteria - Criteria_error.csv`: `Error_certainty`, `Definition`,
  `Error_certainty_rank`.
- `glossary_app.csv`: `Term`, `Definition`, `Some examples`. Optional column
  hidden by the app when present: `References`.

Lookup-table values:

- Error severity ranks must be `High`, `Medium`, or `Low`.
- Error certainty ranks must be `High`, `Medium`, or `Low`.
- Application error effects in `Applications - Application_categories.csv` must
  exist in `Applications - Application_error.csv`.
- Criterion certainty values in `Criteria - Criteria.csv` must exist in
  `Criteria - Criteria_error.csv`.

## License

[MIT License](./LICENSE)
