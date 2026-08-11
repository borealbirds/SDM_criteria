script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_argument) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_argument[1]), winslash = "/"))
} else {
  normalizePath(getwd(), winslash = "/")
}

data_dir <- file.path(script_dir, "app", "data")

read_character_csv <- function(path) {
  data <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    fileEncoding = "UTF-8-BOM"
  )
  data[] <- lapply(data, trimws)
  data
}

require_columns <- function(data, required, filename) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      filename, " is missing required column(s): ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

assert_known_values <- function(values, allowed, label) {
  unknown <- setdiff(unique(values), allowed)
  if (length(unknown) > 0) {
    stop(label, " contains unexpected value(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
}

assert_stable_id_mapping <- function(data, filename) {
  id_stage <- unique(data[c("ID", "Model_stage")])
  id_criterion <- unique(data[c("ID", "Criterion")])
  if (anyDuplicated(id_stage$ID)) {
    stop(filename, " maps at least one ID to multiple model stages.", call. = FALSE)
  }
  if (anyDuplicated(id_criterion$ID)) {
    stop(filename, " maps at least one ID to multiple criterion names.", call. = FALSE)
  }
}

count_distinct <- function(data, group_levels, id_column, count_column) {
  grid <- do.call(expand.grid, c(group_levels, stringsAsFactors = FALSE))
  grid[[count_column]] <- integer(nrow(grid))

  for (i in seq_len(nrow(grid))) {
    keep <- rep(TRUE, nrow(data))
    for (column in names(group_levels)) {
      keep <- keep & data[[column]] == grid[[column]][i]
    }
    grid[[count_column]][i] <- length(unique(data[[id_column]][keep]))
  }
  grid
}

add_percent <- function(data, group_columns, count_column, percent_column) {
  group_key <- if (length(group_columns) == 0) {
    rep("all", nrow(data))
  } else {
    do.call(paste, c(data[group_columns], sep = "\r"))
  }
  totals <- ave(data[[count_column]], group_key, FUN = sum)
  data[[percent_column]] <- ifelse(
    totals == 0,
    0,
    round(100 * data[[count_column]] / totals, 1)
  )
  data
}

order_rows <- function(data, level_order) {
  order_values <- lapply(names(level_order), function(column) {
    match(data[[column]], level_order[[column]])
  })
  data[do.call(order, order_values), , drop = FALSE]
}

format_distribution <- function(data, category_column, count_column, percent_column, category_order) {
  data <- data[match(category_order, data[[category_column]]), , drop = FALSE]
  paste0(
    data[[category_column]], ": ", data[[count_column]],
    " (", sprintf("%.1f", data[[percent_column]]), "%)",
    collapse = "; "
  )
}

core <- read_character_csv(file.path(data_dir, "criteria_core.csv"))
related <- read_character_csv(file.path(data_dir, "criteria_related.csv"))
certainty_lookup <- read_character_csv(file.path(data_dir, "criteria_error.csv"))

common_columns <- c("ID", "Model_type", "Model_stage", "Model_step", "Criterion")
require_columns(
  core,
  c(common_columns, "Underprediction_error_certainty", "Overprediction_error_certainty"),
  "criteria_core.csv"
)
require_columns(related, common_columns, "criteria_related.csv")
require_columns(
  certainty_lookup,
  c("Error_certainty", "Error_certainty_rank"),
  "criteria_error.csv"
)

model_type_levels <- c("Prediction", "Projection")
model_stage_levels <- c(
  "Foundational",
  "Data quality, relevance and preparation",
  "Model building",
  "Model assessment"
)
criterion_type_levels <- c("Core", "Related")
error_type_levels <- c("Underprediction", "Overprediction")
rank_levels <- c("High", "Medium", "Low")
certainty_levels <- certainty_lookup$Error_certainty[match(rank_levels, certainty_lookup$Error_certainty_rank)]

if (anyNA(certainty_levels)) {
  stop("criteria_error.csv must define one certainty category for each of High, Medium, and Low.", call. = FALSE)
}

assert_known_values(core$Model_type, model_type_levels, "criteria_core.csv Model_type")
assert_known_values(related$Model_type, model_type_levels, "criteria_related.csv Model_type")
assert_known_values(core$Model_stage, model_stage_levels, "criteria_core.csv Model_stage")
assert_known_values(related$Model_stage, model_stage_levels, "criteria_related.csv Model_stage")
assert_known_values(
  c(core$Underprediction_error_certainty, core$Overprediction_error_certainty),
  certainty_levels,
  "criteria_core.csv error certainty"
)
assert_stable_id_mapping(core, "criteria_core.csv")
assert_stable_id_mapping(related, "criteria_related.csv")

if (anyDuplicated(core[c("ID", "Model_type")])) {
  stop("criteria_core.csv contains duplicate ID and Model_type combinations.", call. = FALSE)
}
if (anyDuplicated(related[c("ID", "Model_type")])) {
  stop("criteria_related.csv contains duplicate ID and Model_type combinations.", call. = FALSE)
}

core_unique <- unique(core[c("ID", "Model_stage", "Criterion")])
core_unique$Criterion_type <- "Core"
related_unique <- unique(related[c("ID", "Model_stage", "Criterion")])
related_unique$Criterion_type <- "Related"
criteria_unique <- rbind(core_unique, related_unique)

criteria_by_stage <- count_distinct(
  criteria_unique,
  list(Criterion_type = criterion_type_levels, Model_stage = model_stage_levels),
  "ID",
  "n_unique_criteria"
)
criteria_by_stage <- add_percent(
  criteria_by_stage,
  "Criterion_type",
  "n_unique_criteria",
  "percent_within_criterion_type"
)
criteria_by_stage <- order_rows(
  criteria_by_stage,
  list(Criterion_type = criterion_type_levels, Model_stage = model_stage_levels)
)

core_model <- unique(core[c("ID", "Model_type", "Model_stage")])
core_model$Criterion_type <- "Core"
related_model <- unique(related[c("ID", "Model_type", "Model_stage")])
related_model$Criterion_type <- "Related"
criteria_model <- rbind(core_model, related_model)

criteria_by_stage_model <- count_distinct(
  criteria_model,
  list(
    Criterion_type = criterion_type_levels,
    Model_type = model_type_levels,
    Model_stage = model_stage_levels
  ),
  "ID",
  "n_criteria"
)
criteria_by_stage_model <- add_percent(
  criteria_by_stage_model,
  c("Criterion_type", "Model_type"),
  "n_criteria",
  "percent_within_criterion_and_model_type"
)
criteria_by_stage_model <- order_rows(
  criteria_by_stage_model,
  list(
    Criterion_type = criterion_type_levels,
    Model_type = model_type_levels,
    Model_stage = model_stage_levels
  )
)

underprediction <- core[c("ID", "Model_type", "Model_stage")]
underprediction$Error_type <- "Underprediction"
underprediction$Error_certainty <- core$Underprediction_error_certainty
overprediction <- core[c("ID", "Model_type", "Model_stage")]
overprediction$Error_type <- "Overprediction"
overprediction$Error_certainty <- core$Overprediction_error_certainty
core_error_long <- rbind(underprediction, overprediction)

core_error_certainty <- count_distinct(
  core_error_long,
  list(
    Model_type = model_type_levels,
    Error_type = error_type_levels,
    Error_certainty = certainty_levels
  ),
  "ID",
  "n_core_criteria"
)
core_error_certainty <- add_percent(
  core_error_certainty,
  c("Model_type", "Error_type"),
  "n_core_criteria",
  "percent_within_model_and_error_type"
)
core_error_certainty <- order_rows(
  core_error_certainty,
  list(
    Model_type = model_type_levels,
    Error_type = error_type_levels,
    Error_certainty = certainty_levels
  )
)

core_error_by_stage <- count_distinct(
  core_error_long,
  list(
    Model_type = model_type_levels,
    Model_stage = model_stage_levels,
    Error_type = error_type_levels,
    Error_certainty = certainty_levels
  ),
  "ID",
  "n_core_criteria"
)
core_error_by_stage <- add_percent(
  core_error_by_stage,
  c("Model_type", "Model_stage", "Error_type"),
  "n_core_criteria",
  "percent_within_stage_model_and_error_type"
)
core_error_by_stage <- order_rows(
  core_error_by_stage,
  list(
    Model_type = model_type_levels,
    Model_stage = model_stage_levels,
    Error_type = error_type_levels,
    Error_certainty = certainty_levels
  )
)

core_pairs <- core[c(
  "ID", "Model_type", "Underprediction_error_certainty",
  "Overprediction_error_certainty"
)]
core_error_pairs <- count_distinct(
  core_pairs,
  list(
    Model_type = model_type_levels,
    Underprediction_error_certainty = certainty_levels,
    Overprediction_error_certainty = certainty_levels
  ),
  "ID",
  "n_core_criteria"
)
core_error_pairs <- add_percent(
  core_error_pairs,
  "Model_type",
  "n_core_criteria",
  "percent_within_model_type"
)
core_error_pairs <- order_rows(
  core_error_pairs,
  list(
    Model_type = model_type_levels,
    Underprediction_error_certainty = certainty_levels,
    Overprediction_error_certainty = certainty_levels
  )
)

n_core <- length(unique(core$ID))
n_related <- length(unique(related$ID))
summary_lines <- c(
  "CRITERIA INVENTORY",
  sprintf(
    "The framework contained %d unique criteria: %d core criteria and %d related criteria.",
    n_core + n_related, n_core, n_related
  )
)

for (criterion_type in criterion_type_levels) {
  rows <- criteria_by_stage[criteria_by_stage$Criterion_type == criterion_type, ]
  summary_lines <- c(
    summary_lines,
    paste0(
      criterion_type, " criteria by model stage: ",
      format_distribution(
        rows, "Model_stage", "n_unique_criteria",
        "percent_within_criterion_type", model_stage_levels
      ),
      "."
    )
  )
}

summary_lines <- c(summary_lines, "", "MODEL-SPECIFIC CRITERIA COUNTS")
for (criterion_type in criterion_type_levels) {
  for (model_type in model_type_levels) {
    rows <- criteria_by_stage_model[
      criteria_by_stage_model$Criterion_type == criterion_type &
        criteria_by_stage_model$Model_type == model_type,
    ]
    summary_lines <- c(
      summary_lines,
      paste0(
        model_type, " models included ", sum(rows$n_criteria), " ",
        tolower(criterion_type), " criteria: ",
        format_distribution(
          rows, "Model_stage", "n_criteria",
          "percent_within_criterion_and_model_type", model_stage_levels
        ),
        "."
      )
    )
  }
}

summary_lines <- c(summary_lines, "", "CORE-CRITERION ERROR CERTAINTY")
for (model_type in model_type_levels) {
  for (error_type in error_type_levels) {
    rows <- core_error_certainty[
      core_error_certainty$Model_type == model_type &
        core_error_certainty$Error_type == error_type,
    ]
    summary_lines <- c(
      summary_lines,
      paste0(
        model_type, " model ", tolower(error_type), " certainty: ",
        format_distribution(
          rows, "Error_certainty", "n_core_criteria",
          "percent_within_model_and_error_type", certainty_levels
        ),
        "."
      )
    )
  }
}

summary_lines <- c(summary_lines, "", "CORE-CRITERION ERROR CERTAINTY BY MODEL STAGE")
for (model_type in model_type_levels) {
  for (stage in model_stage_levels) {
    stage_rows <- core_error_by_stage[
      core_error_by_stage$Model_type == model_type &
        core_error_by_stage$Model_stage == stage,
    ]
    distributions <- vapply(error_type_levels, function(error_type) {
      rows <- stage_rows[stage_rows$Error_type == error_type, ]
      paste0(
        tolower(error_type), " [",
        format_distribution(
          rows, "Error_certainty", "n_core_criteria",
          "percent_within_stage_model_and_error_type", certainty_levels
        ),
        "]"
      )
    }, character(1))
    summary_lines <- c(
      summary_lines,
      paste0(model_type, " - ", stage, ": ", paste(distributions, collapse = "; "), ".")
    )
  }
}

summary_lines <- c(
  summary_lines,
  "",
  paste0(
    "INTERPRETATION NOTE: Unique framework counts collapse model types by criterion ID. ",
    "Error-certainty counts are model-specific core-criterion records because a criterion's ",
    "certainty can differ between prediction and projection models. Percentages use the total ",
    "number of core criteria in the corresponding model type, stage, and error type as the denominator."
  )
)

cat(paste(summary_lines, collapse = "\n"), "\n")
