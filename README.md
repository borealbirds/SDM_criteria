# SDM Criteria Selector

This project contains a Shiny app for filtering species distribution model (SDM)
evaluation criteria by application category, model type, application error
severity, and criterion error certainty.

## Main App

- `applications_criteria.qmd` contains the Shiny app.
- `applications_criteria.html` is a rendered HTML output.
- `SDM_criteria.Rproj` is the RStudio project file.

Open `applications_criteria.qmd` in RStudio or Positron and run the Shiny
document to use the app.

## Current Data Files

- `Applications - Application_inventory.csv` lists specific applications and
  links each one to an application category.
- `Applications - Application_categories.csv` defines each application category,
  its question/action framing, and the effect of underprediction and
  overprediction errors.
- `Applications - Application_error.csv` maps application error effects to
  severity ranks: `Correctness -> High`, `Quality -> Medium`, and
  `Efficiency -> Low`.
- `Criteria - Criteria.csv` defines the SDM criteria inventory, including model
  stage, model step, descriptions, violation effects, solutions, and certainty
  values for each model-type/error-type pathway.
- `Criteria - Criteria_error.csv` maps criterion error certainty values to
  ranks: `Always -> High`, `Sometimes -> Medium`, and `Rarely -> Low`.
- `glossary_app.csv` defines glossary terms used for clickable definitions in
  the app.
- `tab1_landing_page_intro.docx` provides the landing-page guidance text.

The older `model_applications.csv` and `model_criteria.csv` files are legacy
inputs and are no longer used by the app.

## Filtering Logic

The user selects:

- an application category, or a specific application from the exhaustive
  application list
- a model type: `Prediction` or `Projection`
- an application error severity tolerance: `Correctness (high tolerance)`,
  `Quality (medium tolerance)`, or `Efficiency (low tolerance)`
- a criterion error certainty tolerance: `Always (high tolerance)`,
  `Sometimes (medium tolerance)`, or `Rarely (low tolerance)`

After changing these controls, click `Filter Criteria` to apply the selected
scenario. The criteria, summaries, and report update from the last applied
filter set rather than changing dynamically while dropdowns are being adjusted.

The category and specific-application dropdowns stay synchronized: choosing a
category filters the application list, and choosing a specific application sets
the corresponding category. If no category is selected, the application dropdown
shows the exhaustive application list. The `View Application Table` button opens
the full application inventory for browsing.

The selected application category and severity tolerance determine whether
underprediction, overprediction, or both error types are relevant. For example,
if underprediction has a `Correctness` effect and overprediction has an
`Efficiency` effect, then a `Quality` severity tolerance considers
underprediction but not overprediction.

The selected model type determines which criterion certainty columns are used:

- `Underprediction_prediction_error_certainty`
- `Overprediction_prediction_error_certainty`
- `Underprediction_projection_error_certainty`
- `Overprediction_projection_error_certainty`

The certainty tolerance then filters criteria using ordinal thresholds:

- `Always` includes criteria ranked `High`
- `Sometimes` includes criteria ranked `High` or `Medium`
- `Rarely` includes criteria ranked `High`, `Medium`, or `Low`

A criterion is selected when at least one active error type passes the criterion
certainty threshold.

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
