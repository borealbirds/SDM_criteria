library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(plotly)
library(stringr)
library(officer)

# --- 1. Load and Prepare Data ---
application_inventory <- read.csv(
    "data/application_inventory.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
application_categories <- read.csv(
    "data/application_categories.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
application_error <- read.csv(
    "data/application_error.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
criteria_core <- read.csv(
    "data/criteria_core.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
criteria_related <- read.csv(
    "data/criteria_related.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
criteria_error <- read.csv(
    "data/criteria_error.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)
glossary <- read.csv(
    "data/glossary_app.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
)

MODEL_TYPES <- c("Prediction", "Projection")
TOLERANCE_LEVELS <- c("High", "Medium", "Low")
SEVERITY_TOLERANCE_CHOICES <- c(
    "Correctness (high tolerance)" = "High",
    "Quality (medium tolerance)" = "Medium",
    "Efficiency (low tolerance)" = "Low"
)
CERTAINTY_TOLERANCE_CHOICES <- c(
    "Always (high tolerance)" = "High",
    "Sometimes (medium tolerance)" = "Medium",
    "Rarely (low tolerance)" = "Low"
)
TOLERANCE_COLORS <- c(
    "High" = "#ffb3b3",
    "Medium" = "#ffffcc",
    "Low" = "#c2f0c2"
)
CERTAINTY_COLORS <- c(
    "Always" = unname(TOLERANCE_COLORS["High"]),
    "Sometimes" = unname(TOLERANCE_COLORS["Medium"]),
    "Rarely" = unname(TOLERANCE_COLORS["Low"])
)
APPLICATION_CATEGORY_CHOICES <- sort(unique(
    application_categories$Application_category
))
APPLICATION_CHOICES <- sort(unique(application_inventory$Application))
MODEL_STAGE_LEVELS <- unique(criteria_core$Model_stage)
DEFAULT_FILTERS <- list(
    cat_select = "",
    app_select = "",
    type_select = "Prediction",
    severity_tolerance = "Medium",
    certainty_tolerance = "Medium"
)
FILTER_PROMPT <- 'Please use the sidebar to select your model use-case scenario and click "Filter Criteria" to show the applicable criteria and generate a report.'

repair_mojibake <- function(x) {
    mojibake_pattern <- paste(
        c("\\u00c3", "\\u00c2", "\\u00e2"),
        collapse = "|"
    )
    needs_repair <- !is.na(x) & str_detect(x, mojibake_pattern)
    repaired <- iconv(x[needs_repair], from = "CP1252", to = "UTF-8")
    x[needs_repair] <- ifelse(is.na(repaired), x[needs_repair], repaired)
    x
}

repair_text_columns <- function(df) {
    df %>% mutate(across(where(is.character), repair_mojibake))
}

validate_columns <- function(df, required_cols, file_label) {
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        stop(
            paste0(
                file_label,
                " is missing required column(s): ",
                paste(missing_cols, collapse = ", ")
            ),
            call. = FALSE
        )
    }
}

validate_columns(
    application_inventory,
    c("Application", "Application_category", "Description"),
    "application_inventory.csv"
)

validate_columns(
    application_categories,
    c(
        "Application_category",
        "Question",
        "Action",
        "Underprediction_error_effect",
        "Overprediction_error_effect"
    ),
    "application_categories.csv"
)

validate_columns(
    application_error,
    c("Error_effect", "Definition", "Error_effect_rank"),
    "application_error.csv"
)

validate_columns(
    criteria_core,
    c(
        "ID",
        "Model_type",
        "Model_stage",
        "Model_step",
        "Criterion",
        "Description",
        "Violation",
        "Solutions",
        "Justification",
        "Underprediction_error_certainty",
        "Overprediction_error_certainty",
        "Direct_related_IDs",
        "Indirect_related_IDs",
        "All_related_IDs",
        "Citations"
    ),
    "criteria_core.csv"
)

validate_columns(
    criteria_related,
    c(
        "ID",
        "Model_type",
        "Model_stage",
        "Model_step",
        "Criterion",
        "Description",
        "Violation",
        "Solutions",
        "Justification",
        "Core_reference",
        "Direct_core_IDs",
        "Indirect_core_IDs",
        "All_core_IDs",
        "Core_relationship_paths",
        "Citations"
    ),
    "criteria_related.csv"
)

validate_columns(
    criteria_error,
    c("Error_certainty", "Definition", "Error_certainty_rank"),
    "criteria_error.csv"
)

validate_columns(
    glossary,
    c("Term", "Definition", "Some examples"),
    "glossary_app.csv"
)

application_inventory <- repair_text_columns(application_inventory)
application_categories <- repair_text_columns(application_categories)
application_error <- repair_text_columns(application_error)
criteria_core <- repair_text_columns(criteria_core)
criteria_related <- repair_text_columns(criteria_related)
criteria_error <- repair_text_columns(criteria_error)
glossary <- repair_text_columns(glossary)

application_categories <- application_categories %>%
    left_join(
        application_error %>%
            rename(
                Underprediction_error_effect = Error_effect,
                Underprediction_error_effect_definition = Definition,
                Underprediction_error_severity_rank = Error_effect_rank
            ),
        by = "Underprediction_error_effect"
    ) %>%
    left_join(
        application_error %>%
            rename(
                Overprediction_error_effect = Error_effect,
                Overprediction_error_effect_definition = Definition,
                Overprediction_error_severity_rank = Error_effect_rank
            ),
        by = "Overprediction_error_effect"
    )

read_landing_docx <- function(filepath) {
    if (!file.exists(filepath)) {
        return(rep("", 20))
    }
    doc <- read_docx(filepath)
    content <- docx_summary(doc)
    text_lines <- content$text[content$content_type == "paragraph"]
    text_lines <- text_lines[
        text_lines != "" & !is.na(text_lines) & text_lines != "NA"
    ]
    repair_mojibake(text_lines)
}

landing_lines <- read_landing_docx("data/tab1_landing_page_intro.docx")
