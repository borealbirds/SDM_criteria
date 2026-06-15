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

- an application category
- a model type: `Prediction` or `Projection`
- an application error severity tolerance: `High`, `Medium`, or `Low`
- a criterion error certainty tolerance: `High`, `Medium`, or `Low`

The selected application category and severity tolerance determine whether
underprediction, overprediction, or both error types are relevant. For example,
if underprediction has severity `High` and overprediction has severity `Low`,
then a `Medium` severity tolerance considers underprediction but not
overprediction.

The selected model type determines which criterion certainty columns are used:

- `Underprediction_prediction_error_certainty`
- `Overprediction_prediction_error_certainty`
- `Underprediction_projection_error_certainty`
- `Overprediction_projection_error_certainty`

The certainty tolerance then filters criteria using ordinal thresholds:

- `High` includes criteria ranked `High`
- `Medium` includes criteria ranked `High` or `Medium`
- `Low` includes criteria ranked `High`, `Medium`, or `Low`

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
