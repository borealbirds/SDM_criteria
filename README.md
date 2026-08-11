# SDM Criteria Selector

This repository contains an R Shiny app for identifying species distribution
model (SDM) evaluation criteria for a biodiversity conservation use-case
scenario.

A use-case scenario combines:

- a model application category or specific model application;
- model type: `Prediction` or `Projection`;
- error severity tolerance; and
- error certainty tolerance.

The app applies the selections only when the user clicks `Filter Criteria`.

## Project Structure

- `app/global.R` loads and validates app data and defines shared constants.
- `app/ui.R` defines the sidebar, tabs, styling, and client-side interactions.
- `app/server.R` implements filtering, criterion relationships, displays, and
  report generation.
- `app/data/` contains the application, criteria, glossary, and landing-page
  source files.
- `validate_app_data.R` performs more extensive standalone data validation.

The criteria inventory is divided into two files:

- `criteria_core.csv` contains core criteria. These rows define
  underprediction and overprediction error certainty.
- `criteria_related.csv` contains related criteria that are special cases of
  core criteria. Relationships may be direct or may pass through another
  related criterion.

## Data Files

The `app/data` folder contains:

- `tab1_landing_page_intro.docx`: text for the
  `Overview & Instructions` tab.
- `application_inventory.csv`: specific model applications,
  their categories, descriptions, and optional example papers.
- `application_categories.csv`: conservation decision and SDM
  use descriptions, plus underprediction and overprediction error effects for
  each application category.
- `application_error.csv`: error-effect severity definitions
  and ranks.
- `criteria_core.csv`: model-specific core criteria and error certainty.
- `criteria_related.csv`: model-specific related special cases and their
  relationships to core criteria.
- `criteria_error.csv`: certainty definitions and ranks.
- `glossary_app.csv`: glossary terms, definitions, and examples.

## Requirements

Install the required R packages once:

```r
install.packages(c(
    "bslib",
    "dplyr",
    "DT",
    "officer",
    "plotly",
    "purrr",
    "shiny",
    "stringr"
))
```

## Running the App

From the repository root, run:

```r
shiny::runApp("app")
```

In Positron, open an R console with the repository root as the working
directory, run the command above, and open the URL printed in the console.

If the working directory is already `app`, use:

```r
shiny::runApp(".")
```

## Validating Data

From the repository root, run:

```r
source("validate_app_data.R")
```

A successful run ends with:

```text
All app data validation checks passed.
```

The validator checks:

- required columns;
- allowed severity and certainty ranks;
- application categories and error-effect lookup values;
- allowed model types;
- unique `Model_type` and `ID` combinations;
- that a criterion is not both core and related for the same model type;
- certainty values against `criteria_error.csv`;
- direct, indirect, and complete relationship lists;
- model-specific relationship targets;
- reciprocal core-to-related relationships; and
- relationship paths, including paths through intermediate related criteria.

## App Workflow

1. Select an application category, a specific application, or both. The two
   dropdowns update each other.
2. Select whether the SDM is being used for prediction or projection.
3. Select error severity and error certainty tolerances.
4. Click `Filter Criteria`.

Before filtering, every results tab except `Overview & Instructions` displays
a prompt.

The `Selected use-case scenario` tab summarizes the application, model type,
error settings, optional user details, and other applications in the category.

The `Applicable Criteria` tab displays:

- the selected error-type logic;
- selected core criteria grouped by model stage;
- expandable related special cases beneath each associated core criterion;
- direct and indirect relationship labels;
- details for either a core or related criterion; and
- radar plots summarizing selected core criteria along the model cycle.

Radar plots count core criteria only. Related criteria do not have independently
defined error certainty and therefore are not counted as separately filtered
criteria.

The `Generate a Report` tab provides report metadata and DOCX and CSV exports.
The `Glossary` tab displays searchable definitions and examples.

## Filtering Logic

Application category and error severity tolerance determine which error types
are active:

- `Correctness (high tolerance)` includes effects ranked `High`.
- `Quality (medium tolerance)` includes effects ranked `High` or `Medium`.
- `Efficiency (low tolerance)` includes effects ranked `High`, `Medium`,
  or `Low`.

Core criteria are first restricted to the selected `Model_type`. The app then
uses:

- `Underprediction_error_certainty`; and
- `Overprediction_error_certainty`.

Error certainty tolerance is ordinal:

- `Always (high tolerance)` includes `Always`.
- `Sometimes (medium tolerance)` includes `Always` and `Sometimes`.
- `Rarely (low tolerance)` includes `Always`, `Sometimes`, and `Rarely`.

A core criterion is selected when at least one active error type meets the
certainty threshold.

After core filtering, the app retrieves related criteria for the same model
type using `All_related_IDs` and `All_core_IDs`. It retains direct and
indirect classifications and the paths in `Core_relationship_paths`. Related
criteria are associated with selected cores; they are not filtered using
independent certainty values.

Because an ID can have a different role for Prediction and Projection, criteria
must always be identified by the combination of `Model_type` and `ID`.

## Reports

The on-screen report and CSV distinguish `Core criterion` from
`Related special case`.

Core rows include their selected underprediction and overprediction certainty.
Related rows leave certainty fields blank and include:

- associated core IDs and names;
- relationship type; and
- relationship paths.

Each related criterion appears once in the consolidated CSV, even when it is
associated with multiple selected core criteria. The DOCX is hierarchical:
related special cases are listed beneath each associated selected core
criterion.

## Data Dictionary

Required columns by file:

- `application_inventory.csv`: `Application`,
  `Application_category`, `Description`. Optional fields used when present:
  `Example citation`, `Citation URL`.
- `application_categories.csv`: `Application_category`,
  `Question`, `Action`, `Underprediction_error_effect`,
  `Overprediction_error_effect`.
- `application_error.csv`: `Error_effect`, `Definition`,
  `Error_effect_rank`.
- `criteria_core.csv`: `ID`, `Model_type`, `Model_stage`,
  `Model_step`, `Criterion`, `Description`, `Violation`, `Solutions`,
  `Justification`, `Underprediction_error_certainty`,
  `Overprediction_error_certainty`, `Direct_related_IDs`,
  `Indirect_related_IDs`, `All_related_IDs`, `Citations`.
- `criteria_related.csv`: `ID`, `Model_type`, `Model_stage`,
  `Model_step`, `Criterion`, `Description`, `Violation`, `Solutions`,
  `Justification`, `Core_reference`, `Direct_core_IDs`,
  `Indirect_core_IDs`, `All_core_IDs`, `Core_relationship_paths`,
  `Citations`.
- `criteria_error.csv`: `Error_certainty`, `Definition`,
  `Error_certainty_rank`.
- `glossary_app.csv`: `Term`, `Definition`, `Some examples`. The
  optional `References` column is hidden in the app.

`Solutions` and `Core_reference` may be blank for related criteria. Lists of
IDs are comma-separated. Multiple relationship paths are semicolon-separated,
with `>` indicating movement from a related criterion toward a core criterion.

## Encoding

CSV files should be saved as UTF-8. The app repairs common encoding artifacts
at runtime as a guardrail, but correcting the source CSV is preferred.

Multiple example citations or URLs in
`application_inventory.csv` may be separated with semicolons.

## License

[MIT License](./LICENSE)
