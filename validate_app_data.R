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

data_path <- function(filename) {
  file.path("app", "data", filename)
}

application_inventory <- read_app_csv(data_path("application_inventory.csv"))
application_categories <- read_app_csv(data_path("application_categories.csv"))
application_error <- read_app_csv(data_path("application_error.csv"))
criteria_core <- read_app_csv(data_path("criteria_core.csv"))
criteria_related <- read_app_csv(data_path("criteria_related.csv"))
criteria_error <- read_app_csv(data_path("criteria_error.csv"))
glossary <- read_app_csv(data_path("glossary_app.csv"))

required_columns(
  application_inventory,
  c("Application", "Application_category", "Description"),
  "application_inventory.csv"
)
required_columns(
  application_categories,
  c("Application_category", "Question", "Action", "Underprediction_error_effect", "Overprediction_error_effect"),
  "application_categories.csv"
)
required_columns(
  application_error,
  c("Error_effect", "Definition", "Error_effect_rank"),
  "application_error.csv"
)
required_columns(
  criteria_core,
  c(
    "ID", "Model_type", "Model_stage", "Model_step", "Criterion",
    "Description", "Violation", "Solutions", "Justification",
    "Underprediction_error_certainty", "Overprediction_error_certainty",
    "Direct_related_IDs", "Indirect_related_IDs", "All_related_IDs",
    "Citations"
  ),
  "criteria_core.csv"
)
required_columns(
  criteria_related,
  c(
    "ID", "Model_type", "Model_stage", "Model_step", "Criterion",
    "Description", "Violation", "Solutions", "Justification",
    "Core_reference", "Direct_core_IDs", "Indirect_core_IDs", "All_core_IDs",
    "Core_relationship_paths", "Citations"
  ),
  "criteria_related.csv"
)
required_columns(
  criteria_error,
  c("Error_certainty", "Definition", "Error_certainty_rank"),
  "criteria_error.csv"
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
  "Underprediction_error_certainty",
  "Overprediction_error_certainty"
)
criteria_certainties <- unique(unlist(criteria_core[certainty_columns], use.names = FALSE))
criteria_certainties <- criteria_certainties[!is.na(criteria_certainties) & criteria_certainties != ""]
missing_certainties <- setdiff(criteria_certainties, unique(criteria_error$Error_certainty))
if (length(missing_certainties) > 0) {
  stop(
    "Core criteria table references unknown error certainty value(s): ",
    paste(missing_certainties, collapse = ", "),
    call. = FALSE
  )
}

blank_core_criteria <- which(is.na(criteria_core$Criterion) | trimws(criteria_core$Criterion) == "")
if (length(blank_core_criteria) > 0) {
  stop("Core criteria table has blank Criterion value(s) in row(s): ", paste(blank_core_criteria, collapse = ", "), call. = FALSE)
}

blank_related_criteria <- which(is.na(criteria_related$Criterion) | trimws(criteria_related$Criterion) == "")
if (length(blank_related_criteria) > 0) {
  stop("Related criteria table has blank Criterion value(s) in row(s): ", paste(blank_related_criteria, collapse = ", "), call. = FALSE)
}

allowed_model_types <- c("Prediction", "Projection")
unknown_model_types <- setdiff(
  unique(c(criteria_core$Model_type, criteria_related$Model_type)),
  allowed_model_types
)
if (length(unknown_model_types) > 0) {
  stop("Unexpected model type(s): ", paste(unknown_model_types, collapse = ", "), call. = FALSE)
}

core_keys <- paste(criteria_core$Model_type, criteria_core$ID, sep = "::")
related_keys <- paste(criteria_related$Model_type, criteria_related$ID, sep = "::")
if (anyDuplicated(core_keys)) {
  stop("criteria_core.csv contains duplicate Model_type and ID combinations.", call. = FALSE)
}
if (anyDuplicated(related_keys)) {
  stop("criteria_related.csv contains duplicate Model_type and ID combinations.", call. = FALSE)
}
overlapping_keys <- intersect(core_keys, related_keys)
if (length(overlapping_keys) > 0) {
  stop(
    "Criteria cannot be both core and related for the same model type: ",
    paste(overlapping_keys, collapse = ", "),
    call. = FALSE
  )
}

split_ids <- function(value) {
  if (length(value) == 0 || is.na(value) || trimws(value) == "") {
    return(character())
  }
  trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]])
}

relationship_errors <- character()
for (i in seq_len(nrow(criteria_core))) {
  row <- criteria_core[i, ]
  related_ids <- as.character(criteria_related$ID[
    criteria_related$Model_type == row$Model_type
  ])
  direct_ids <- split_ids(row$Direct_related_IDs)
  indirect_ids <- split_ids(row$Indirect_related_IDs)
  all_ids <- split_ids(row$All_related_IDs)
  missing_ids <- setdiff(all_ids, related_ids)
  if (length(missing_ids) > 0) {
    relationship_errors <- c(
      relationship_errors,
      paste0(row$Model_type, " core ", row$ID, " references unknown related ID(s): ", paste(missing_ids, collapse = ", "))
    )
  }
  if (!all(c(direct_ids, indirect_ids) %in% all_ids)) {
    relationship_errors <- c(
      relationship_errors,
      paste0(row$Model_type, " core ", row$ID, " has direct/indirect IDs absent from All_related_IDs")
    )
  }
}

for (i in seq_len(nrow(criteria_related))) {
  row <- criteria_related[i, ]
  core_ids <- as.character(criteria_core$ID[criteria_core$Model_type == row$Model_type])
  direct_ids <- split_ids(row$Direct_core_IDs)
  indirect_ids <- split_ids(row$Indirect_core_IDs)
  all_ids <- split_ids(row$All_core_IDs)
  missing_ids <- setdiff(all_ids, core_ids)
  if (length(missing_ids) > 0) {
    relationship_errors <- c(
      relationship_errors,
      paste0(row$Model_type, " related ", row$ID, " references unknown core ID(s): ", paste(missing_ids, collapse = ", "))
    )
  }
  if (!all(c(direct_ids, indirect_ids) %in% all_ids)) {
    relationship_errors <- c(
      relationship_errors,
      paste0(row$Model_type, " related ", row$ID, " has direct/indirect IDs absent from All_core_IDs")
    )
  }

  paths <- trimws(strsplit(row$Core_relationship_paths, ";", fixed = TRUE)[[1]])
  related_ids <- as.character(criteria_related$ID[
    criteria_related$Model_type == row$Model_type
  ])
  for (path in paths) {
    nodes <- trimws(strsplit(path, ">", fixed = TRUE)[[1]])
    valid_intermediates <- length(nodes) <= 2 || all(nodes[2:(length(nodes) - 1)] %in% related_ids)
    if (
      length(nodes) < 2 || nodes[1] != as.character(row$ID) ||
      !(tail(nodes, 1) %in% core_ids) || !valid_intermediates
    ) {
      relationship_errors <- c(
        relationship_errors,
        paste0(row$Model_type, " related ", row$ID, " has invalid relationship path: ", path)
      )
    }
  }
}

for (i in seq_len(nrow(criteria_core))) {
  row <- criteria_core[i, ]
  for (related_id in split_ids(row$All_related_IDs)) {
    related_row <- criteria_related[
      criteria_related$Model_type == row$Model_type &
        as.character(criteria_related$ID) == related_id,
      ,
      drop = FALSE
    ]
    if (nrow(related_row) > 0 && !(as.character(row$ID) %in% split_ids(related_row$All_core_IDs[1]))) {
      relationship_errors <- c(
        relationship_errors,
        paste0(row$Model_type, " core ", row$ID, " and related ", related_id, " are not reciprocal")
      )
    }
  }
}

for (i in seq_len(nrow(criteria_related))) {
  row <- criteria_related[i, ]
  for (core_id in split_ids(row$All_core_IDs)) {
    core_row <- criteria_core[
      criteria_core$Model_type == row$Model_type &
        as.character(criteria_core$ID) == core_id,
      ,
      drop = FALSE
    ]
    if (nrow(core_row) > 0 && !(as.character(row$ID) %in% split_ids(core_row$All_related_IDs[1]))) {
      relationship_errors <- c(
        relationship_errors,
        paste0(row$Model_type, " related ", row$ID, " and core ", core_id, " are not reciprocal")
      )
    }
  }
}

if (length(relationship_errors) > 0) {
  stop(paste(unique(relationship_errors), collapse = "\n"), call. = FALSE)
}

message("All app data validation checks passed.")
