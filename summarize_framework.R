#CRITERIA SUMMARY ##########

# 1. Load the criteria tables ----
# Run this script from the repository root with source("summarize_framework.R").
suppressPackageStartupMessages(library(dplyr))

data_dir <- file.path("app", "data")

core <- read.csv(
  file.path(data_dir, "criteria_core.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

related <- read.csv(
  file.path(data_dir, "criteria_related.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# 2. Set the order used in summaries and future figures ----
model_type_order <- c("Prediction", "Projection")
model_stage_order <- c(
  "Foundational",
  "Data quality, relevance and preparation",
  "Model building",
  "Model assessment"
)
error_type_order <- c("Underprediction", "Overprediction")
error_certainty_order <- c("Always", "Sometimes", "Rarely")


# 3. Summarize core and related criteria by model stage ----
# These counts collapse prediction and projection rows with the same criterion ID.
criteria_inventory <- bind_rows(
  core %>% transmute(Criterion_type = "Core", ID, Model_type, Model_stage),
  related %>% transmute(Criterion_type = "Related", ID, Model_type, Model_stage)
) %>%
  mutate(
    Criterion_type = factor(Criterion_type, levels = c("Core", "Related")),
    Model_type = factor(Model_type, levels = model_type_order),
    Model_stage = factor(Model_stage, levels = model_stage_order)
  )

framework_totals <- criteria_inventory %>%
  distinct(Criterion_type, ID) %>%
  count(Criterion_type, name = "n_unique_criteria", .drop = FALSE)

criteria_by_stage <- criteria_inventory %>%
  distinct(Criterion_type, Model_stage, ID) %>%
  count(
    Criterion_type,
    Model_stage,
    name = "n_unique_criteria",
    .drop = FALSE
  ) %>%
  group_by(Criterion_type) %>%
  mutate(
    percent = round(100 * n_unique_criteria / sum(n_unique_criteria), 1)
  ) %>%
  ungroup()


# 4. Summarize criteria separately for prediction and projection ----
# These are model-specific counts because some criteria apply to only one model type.
criteria_by_stage_and_model_type <- criteria_inventory %>%
  distinct(Criterion_type, Model_type, Model_stage, ID) %>%
  count(
    Criterion_type,
    Model_type,
    Model_stage,
    name = "n_criteria",
    .drop = FALSE
  ) %>%
  group_by(Criterion_type, Model_type) %>%
  mutate(percent = round(100 * n_criteria / sum(n_criteria), 1)) %>%
  ungroup()


# 5. Convert underprediction and overprediction certainty to long format ----
# Each row represents one core criterion, model type, and error type.
core_error_long <- bind_rows(
  core %>%
    transmute(
      ID,
      Model_type,
      Model_stage,
      Error_type = "Underprediction",
      Error_certainty = Underprediction_error_certainty
    ),
  core %>%
    transmute(
      ID,
      Model_type,
      Model_stage,
      Error_type = "Overprediction",
      Error_certainty = Overprediction_error_certainty
    )
) %>%
  mutate(
    Model_type = factor(Model_type, levels = model_type_order),
    Model_stage = factor(Model_stage, levels = model_stage_order),
    Error_type = factor(Error_type, levels = error_type_order),
    Error_certainty = factor(Error_certainty, levels = error_certainty_order)
  )


# 6. Summarize error certainty overall and by model type ----
# Each core criterion contributes one underprediction and one overprediction record.
error_certainty_summary <- core_error_long %>%
  count(Error_certainty, name = "n_core_criterion_records", .drop = FALSE) %>%
  mutate(
    percent = round(
      100 * n_core_criterion_records / sum(n_core_criterion_records),
      1
    )
  )

error_certainty_by_model_type <- core_error_long %>%
  count(
    Model_type,
    Error_certainty,
    name = "n_core_criterion_records",
    .drop = FALSE
  ) %>%
  group_by(Model_type) %>%
  mutate(
    percent = round(
      100 * n_core_criterion_records / sum(n_core_criterion_records),
      1
    )
  ) %>%
  ungroup()


# 7. Summarize core criteria by error-certainty category ----
# Percentages are calculated within each model type and error type.
core_error_certainty <- core_error_long %>%
  count(
    Model_type,
    Error_type,
    Error_certainty,
    name = "n_core_criteria",
    .drop = FALSE
  ) %>%
  group_by(Model_type, Error_type) %>%
  mutate(percent = round(100 * n_core_criteria / sum(n_core_criteria), 1)) %>%
  ungroup()


# 8. Summarize core error certainty by model stage ----
# Percentages are calculated within each model type, stage, and error type.
core_error_certainty_by_stage <- core_error_long %>%
  count(
    Model_stage,
    Error_certainty,
    name = "n_core_criteria",
    .drop = FALSE
  ) %>%
  group_by(Model_stage) %>%
  mutate(percent = round(100 * n_core_criteria / sum(n_core_criteria), 1)) %>%
  ungroup()


# 9. Print the summaries to the R console ----
# The script creates no CSV or text outputs. The objects remain available in R.
cat("\nUnique core and related criteria\n")
print(framework_totals)

cat("\nUnique criteria by model stage\n")
print(criteria_by_stage)

cat("\nCriteria by model stage and model type\n")
print(criteria_by_stage_and_model_type)

cat("\nError certainty overall\n")
print(error_certainty_summary)

cat("\nError certainty by model type\n")
print(error_certainty_by_model_type)

cat("\nCore criteria by error certainty\n")
print(core_error_certainty)

cat("\nCore error certainty by model stage\n")
print(core_error_certainty_by_stage)
