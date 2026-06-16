required_columns <- function(data, columns, file_label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(
      paste0(file_label, " is missing required column(s): ", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

read_app_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

application_inventory <- read_app_csv("Applications - Application_inventory.csv")
application_categories <- read_app_csv("Applications - Application_categories.csv")
application_error <- read_app_csv("Applications - Application_error.csv")
criteria_df <- read_app_csv("Criteria - Criteria.csv")
criteria_error <- read_app_csv("Criteria - Criteria_error.csv")
glossary <- read_app_csv("glossary_app.csv")

required_columns(
  application_inventory,
  c("Application", "Application_category", "Description"),
  "Applications - Application_inventory.csv"
)
required_columns(
  application_categories,
  c("Application_category", "Question", "Action", "Underprediction_error_effect", "Overprediction_error_effect"),
  "Applications - Application_categories.csv"
)
required_columns(
  application_error,
  c("Error_effect", "Definition", "Error_effect_rank"),
  "Applications - Application_error.csv"
)
required_columns(
  criteria_df,
  c(
    "Model_stage", "Model_step", "Criterion", "Description", "Violation",
    "Underprediction_prediction_error_certainty",
    "Overprediction_prediction_error_certainty",
    "Prediction_justification",
    "Underprediction_projection_error_certainty",
    "Overprediction_projection_error_certainty",
    "Projection_justification",
    "Solutions"
  ),
  "Criteria - Criteria.csv"
)
required_columns(
  criteria_error,
  c("Error_certainty", "Definition", "Error_certainty_rank"),
  "Criteria - Criteria_error.csv"
)
required_columns(glossary, c("Term", "Definition", "Some examples"), "glossary_app.csv")

allowed_ranks <- c("High", "Medium", "Low")
bad_application_ranks <- setdiff(unique(application_error$Error_effect_rank), allowed_ranks)
bad_criteria_ranks <- setdiff(unique(criteria_error$Error_certainty_rank), allowed_ranks)
if (length(bad_application_ranks) > 0) {
  stop("Unexpected application error rank(s): ", paste(bad_application_ranks, collapse = ", "), call. = FALSE)
}
if (length(bad_criteria_ranks) > 0) {
  stop("Unexpected criteria error rank(s): ", paste(bad_criteria_ranks, collapse = ", "), call. = FALSE)
}

missing_inventory_categories <- setdiff(
  unique(application_inventory$Application_category),
  unique(application_categories$Application_category)
)
if (length(missing_inventory_categories) > 0) {
  stop(
    "Application inventory references unknown category/categories: ",
    paste(missing_inventory_categories, collapse = ", "),
    call. = FALSE
  )
}

application_effects <- unique(application_error$Error_effect)
missing_application_effects <- setdiff(
  unique(c(application_categories$Underprediction_error_effect, application_categories$Overprediction_error_effect)),
  application_effects
)
if (length(missing_application_effects) > 0) {
  stop(
    "Application categories reference unknown error effect(s): ",
    paste(missing_application_effects, collapse = ", "),
    call. = FALSE
  )
}

certainty_columns <- c(
  "Underprediction_prediction_error_certainty",
  "Overprediction_prediction_error_certainty",
  "Underprediction_projection_error_certainty",
  "Overprediction_projection_error_certainty"
)
criteria_certainties <- unique(unlist(criteria_df[certainty_columns], use.names = FALSE))
criteria_certainties <- criteria_certainties[!is.na(criteria_certainties) & criteria_certainties != ""]
missing_certainties <- setdiff(criteria_certainties, unique(criteria_error$Error_certainty))
if (length(missing_certainties) > 0) {
  stop(
    "Criteria table references unknown error certainty value(s): ",
    paste(missing_certainties, collapse = ", "),
    call. = FALSE
  )
}

blank_criteria <- which(is.na(criteria_df$Criterion) | trimws(criteria_df$Criterion) == "")
if (length(blank_criteria) > 0) {
  stop("Criteria table has blank Criterion value(s) in row(s): ", paste(blank_criteria, collapse = ", "), call. = FALSE)
}

message("All app data validation checks passed.")
