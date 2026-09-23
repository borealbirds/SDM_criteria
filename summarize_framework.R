#CRITERIA SUMMARY ##########

# 1. Load the criteria tables ----
# Run this script from the repository root with source("summarize_framework.R").
suppressPackageStartupMessages({
  library(dplyr)
  library(ggalluvial)
  library(ggplot2)
  library(purrr)
  library(tidyr)
})

data_dir <- file.path("app", "data")
figure_dir <- paste0(
  "G:/Shared drives/BAM_ECKnight/ModelEvaluation/Papers/",
  "Criteria & Applications/Figures"
)

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


# APPLICATION SUMMARY ##########

# 1. Load the application inventory ----
applications <- read.csv(
  file.path(data_dir, "application_inventory.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

application_categories <- read.csv(
  file.path(data_dir, "application_categories.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

application_error <- read.csv(
  file.path(data_dir, "application_error.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

criteria_error <- read.csv(
  file.path(data_dir, "criteria_error.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

application_category_order <- unique(applications$Application_category)


# 2. Count the number of applications ----
# Count distinct application names so repeated records are not double-counted.
application_total <- applications %>%
  summarise(n_applications = n_distinct(Application))


# 3. Summarize applications by application category ----
applications_by_category <- applications %>%
  distinct(Application_category, Application) %>%
  count(Application_category, name = "n_applications") %>%
  mutate(
    Application_category = factor(
      Application_category,
      levels = application_category_order
    ),
    percent = round(100 * n_applications / sum(n_applications), 1)
  ) %>%
  arrange(Application_category)


# 4. Print the application summaries to the R console ----
cat("\nNumber of applications\n")
print(application_total)

cat("\nApplications by application category\n")
print(applications_by_category)


# APPLICATION-CRITERIA LINKS ##########

# 1. Define helpers that reproduce the app's filtering rules ----
tolerance_order <- c("High", "Medium", "Low")

rank_at_or_above <- function(rank_value, tolerance) {
  value_rank <- match(rank_value, tolerance_order)
  tolerance_rank <- match(tolerance, tolerance_order)
  !is.na(value_rank) & !is.na(tolerance_rank) & value_rank <= tolerance_rank
}

split_criteria_ids <- function(value) {
  if (length(value) == 0 || is.na(value) || trimws(value) == "") {
    return(character())
  }
  trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]])
}

combine_error_pathways <- function(pathways) {
  pathways <- unique(pathways[!is.na(pathways) & pathways != ""])
  if (
    "Both" %in% pathways ||
      all(c("Underprediction", "Overprediction") %in% pathways)
  ) {
    return("Both")
  }
  if (length(pathways) == 0) "None" else pathways[1]
}


# 2. Select criteria for one application-category scenario ----
# Related criteria inherit the error pathway of the selected core criteria to
# which they are linked. Each related criterion is counted only once.
select_scenario_criteria <- function(
  application_category,
  model_type,
  severity_tolerance,
  certainty_tolerance
) {
  category_row <- application_categories %>%
    filter(Application_category == application_category) %>%
    slice(1)

  under_severity <- application_error$Error_effect_rank[
    match(
      category_row$Underprediction_error_effect,
      application_error$Error_effect
    )
  ]
  over_severity <- application_error$Error_effect_rank[
    match(
      category_row$Overprediction_error_effect,
      application_error$Error_effect
    )
  ]

  under_active <- rank_at_or_above(under_severity, severity_tolerance)
  over_active <- rank_at_or_above(over_severity, severity_tolerance)

  selected_core <- core %>%
    filter(Model_type == model_type) %>%
    mutate(
      Underprediction_certainty_rank = criteria_error$Error_certainty_rank[
        match(
          Underprediction_error_certainty,
          criteria_error$Error_certainty
        )
      ],
      Overprediction_certainty_rank = criteria_error$Error_certainty_rank[
        match(
          Overprediction_error_certainty,
          criteria_error$Error_certainty
        )
      ],
      Underprediction_selected = under_active & rank_at_or_above(
        Underprediction_certainty_rank,
        certainty_tolerance
      ),
      Overprediction_selected = over_active & rank_at_or_above(
        Overprediction_certainty_rank,
        certainty_tolerance
      ),
      Error_pathway = case_when(
        Underprediction_selected & Overprediction_selected ~ "Both",
        Underprediction_selected ~ "Underprediction",
        Overprediction_selected ~ "Overprediction",
        TRUE ~ "None"
      )
    ) %>%
    filter(Underprediction_selected | Overprediction_selected)

  related_links <- map_dfr(seq_len(nrow(selected_core)), function(i) {
    related_ids <- split_criteria_ids(selected_core$All_related_IDs[i])
    if (length(related_ids) == 0) {
      return(NULL)
    }
    tibble(
      Related_ID = related_ids,
      Core_ID = as.character(selected_core$ID[i]),
      Core_error_pathway = selected_core$Error_pathway[i]
    )
  })

  if (nrow(related_links) == 0) {
    selected_related <- related[0, ] %>%
      mutate(Error_pathway = character())
  } else {
    related_pathways <- related_links %>%
      group_by(Related_ID) %>%
      summarise(
        Error_pathway = combine_error_pathways(Core_error_pathway),
        .groups = "drop"
      )

    selected_related <- related %>%
      filter(
        Model_type == model_type,
        as.character(ID) %in% related_pathways$Related_ID
      ) %>%
      mutate(Related_ID = as.character(ID)) %>%
      left_join(related_pathways, by = "Related_ID") %>%
      select(-Related_ID)
  }

  active_error_types <- c(
    if (under_active) "Underprediction",
    if (over_active) "Overprediction"
  )

  scenario_fields <- tibble(
    Application_category = application_category,
    Model_type = model_type,
    Severity_tolerance = severity_tolerance,
    Certainty_tolerance = certainty_tolerance
  )

  selected_criteria <- bind_rows(
    selected_core %>%
      transmute(
        Criterion_type = "Core",
        Criterion_key = paste0("C", ID),
        ID = as.character(ID),
        Model_stage,
        Criterion,
        Error_pathway
      ),
    selected_related %>%
      transmute(
        Criterion_type = "Related",
        Criterion_key = paste0("R", ID),
        ID = as.character(ID),
        Model_stage,
        Criterion,
        Error_pathway
      )
  )

  if (nrow(selected_criteria) > 0) {
    selected_criteria <- bind_cols(
      scenario_fields[rep(1, nrow(selected_criteria)), ],
      selected_criteria
    )
  } else {
    selected_criteria <- scenario_fields[0, ] %>%
      mutate(
        Criterion_type = character(),
        Criterion_key = character(),
        ID = character(),
        Model_stage = character(),
        Criterion = character(),
        Error_pathway = character()
      )
  }

  n_possible <- sum(core$Model_type == model_type) +
    sum(related$Model_type == model_type)

  summary <- scenario_fields %>%
    mutate(
      Active_error_types = if (
        length(active_error_types) == 0
      ) "None" else paste(active_error_types, collapse = " and "),
      n_core_criteria = nrow(selected_core),
      n_related_criteria = nrow(selected_related),
      n_total_criteria = n_core_criteria + n_related_criteria,
      percent_of_model_criteria = round(
        100 * n_total_criteria / n_possible,
        1
      )
    )

  list(summary = summary, criteria = selected_criteria)
}


# 3. Evaluate every category, model type, and tolerance combination ----
scenario_grid <- expand_grid(
  Application_category = application_category_order,
  Model_type = model_type_order,
  Severity_tolerance = tolerance_order,
  Certainty_tolerance = tolerance_order
)

scenario_results <- pmap(
  scenario_grid,
  function(
    Application_category,
    Model_type,
    Severity_tolerance,
    Certainty_tolerance
  ) {
    select_scenario_criteria(
      Application_category,
      Model_type,
      Severity_tolerance,
      Certainty_tolerance
    )
  }
)

scenario_summary <- map_dfr(scenario_results, "summary")
scenario_criteria <- map_dfr(scenario_results, "criteria")

criteria_return_range <- scenario_summary %>%
  group_by(Application_category, Model_type) %>%
  summarise(
    minimum_criteria = min(n_total_criteria),
    maximum_criteria = max(n_total_criteria),
    .groups = "drop"
  )

cat("\nCriteria returned across tolerance combinations\n")
print(criteria_return_range)


# FRAMEWORK OUTCOMES FIGURE ##########

# 1. Prepare all application-category and tolerance outcomes ----
severity_labels <- c(
  "High" = "Error severity tolerance\nCorrectness",
  "Medium" = "Error severity tolerance\nQuality",
  "Low" = "Error severity tolerance\nEfficiency"
)

certainty_labels <- c(
  "High" = "Always",
  "Medium" = "Sometimes",
  "Low" = "Rarely"
)

model_type_labels <- c(
  "Prediction" = "Model type\nPrediction",
  "Projection" = "Model type\nProjection"
)

wrap_category <- function(x, width = 34) {
  vapply(x, function(value) {
    paste(strwrap(value, width = width), collapse = "\n")
  }, character(1))
}

framework_outcomes <- scenario_summary %>%
  mutate(
    Application_category = factor(
      Application_category,
      levels = rev(application_category_order)
    ),
    Model_type = factor(Model_type, levels = model_type_order),
    Severity_tolerance = factor(
      Severity_tolerance,
      levels = tolerance_order
    ),
    Certainty_tolerance = factor(
      Certainty_tolerance,
      levels = tolerance_order
    ),
    error_pathway_label = recode(
      Active_error_types,
      "Underprediction" = "U",
      "Overprediction" = "O",
      "Underprediction and Overprediction" = "U + O",
      "None" = "None"
    ),
    n_core_criteria_plot = na_if(n_core_criteria, 0L),
    tile_label = if_else(
      is.na(n_core_criteria_plot),
      "NA",
      paste(n_core_criteria_plot, error_pathway_label, sep = "\n")
    )
  )

minimum_returned <- min(framework_outcomes$n_core_criteria_plot, na.rm = TRUE)
maximum_returned <- max(framework_outcomes$n_core_criteria_plot, na.rm = TRUE)


# 2. Plot criteria returned for every possible app scenario ----
framework_outcomes_figure <- ggplot(
  framework_outcomes,
  aes(
    x = Certainty_tolerance,
    y = Application_category,
    fill = n_core_criteria_plot
  )
) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(
    aes(
      label = tile_label,
      colour = if_else(
        is.na(n_core_criteria_plot),
        FALSE,
        n_core_criteria_plot > maximum_returned * 0.55
      )
    ),
    lineheight = 0.9,
    size = 3.1
  ) +
  facet_grid(
    rows = vars(Model_type),
    cols = vars(Severity_tolerance),
    labeller = labeller(
      Model_type = model_type_labels,
      Severity_tolerance = severity_labels
    )
  ) +
  scale_x_discrete(
    labels = certainty_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(
    labels = function(x) wrap_category(x, width = 24),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_gradientn(
    colours = c("#F8F3E6", "#BEDCE8", "#65A6BC", "#286C83"),
    limits = c(minimum_returned, maximum_returned),
    na.value = "#DDDCD7",
    name = "Core criteria returned"
  ) +
  scale_colour_manual(
    values = c("TRUE" = "white", "FALSE" = "#222222"),
    guide = "none"
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Error certainty tolerance",
    y = NULL,
    caption = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#F3CF93", colour = NA),
    axis.text.x = element_text(size = 8.5),
    axis.text.y = element_text(size = 9, lineheight = 0.9),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.spacing.y = unit(0.8, "lines"),
    plot.margin = margin(10, 15, 10, 10)
  )


# 3. Save the framework-results figure ----
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

framework_outcomes_path <- file.path(
  figure_dir,
  "framework_core_criteria_outcomes_heatmap.png"
)

ggsave(
  filename = framework_outcomes_path,
  plot = framework_outcomes_figure,
  width = 13,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

cat("\nSaved framework-results figure to\n", framework_outcomes_path, "\n")


# CASE-STUDY SANKEY FIGURE ##########

# 1. Prepare model-stage labels and colours ----
stage_labels <- c(
  "Foundational" = "Foundational",
  "Data quality, relevance and preparation" = "Data quality / preparation",
  "Model building" = "Model building",
  "Model assessment" = "Model assessment"
)

stage_colours <- c(
  "Foundational" = "#595959",
  "Data quality / preparation" = "#56B4E9",
  "Model building" = "#E69F00",
  "Model assessment" = "#009E73"
)

# 2. Apply the manuscript case-study filters to prediction core criteria ----
# Application: Anthropogenic-disturbance screening for avoidance or constraints
# Category: Threshold for restriction or precautionary management
# Severity tolerance: Correctness, which retains underprediction only
# Certainty tolerance: Sometimes, which retains Always and Sometimes
wrap_criterion_label <- function(id, criterion, width = 34) {
  label <- paste0(id, ": ", criterion)
  paste(strwrap(label, width = width), collapse = "\n")
}

case_study_criteria <- core %>%
  filter(Model_type == "Prediction") %>%
  mutate(
    Error_certainty = factor(
      Underprediction_error_certainty,
      levels = error_certainty_order
    ),
    Applicability = factor(
      if_else(
        Underprediction_error_certainty %in% c("Always", "Sometimes"),
        "Retained",
        "Filtered out"
      ),
      levels = c("Retained", "Filtered out")
    ),
    Model_stage = factor(
      stage_labels[Model_stage],
      levels = stage_labels[model_stage_order]
    ),
    Criterion_label = map2_chr(
      ID,
      Criterion,
      wrap_criterion_label
    )
  ) %>%
  arrange(
    Model_stage,
    suppressWarnings(as.numeric(ID))
  ) %>%
  mutate(
    Criterion_label = factor(
      Criterion_label,
      levels = unique(Criterion_label)
    )
  )

case_study_summary <- case_study_criteria %>%
  count(Error_certainty, Applicability, name = "n_criteria", .drop = FALSE)

selected_core <- case_study_criteria %>%
  filter(Applicability == "Retained")

selected_related_ids <- unique(trimws(unlist(strsplit(
  selected_core$All_related_IDs[
    !is.na(selected_core$All_related_IDs) &
      selected_core$All_related_IDs != ""
  ],
  ",",
  fixed = TRUE
))))

n_related_case_study <- related %>%
  filter(
    Model_type == "Prediction",
    as.character(ID) %in% selected_related_ids
  ) %>%
  nrow()

n_retained_core <- sum(case_study_criteria$Applicability == "Retained")
n_filtered_core <- sum(case_study_criteria$Applicability == "Filtered out")
n_total_case_study <- n_retained_core + n_related_case_study

stopifnot(
  nrow(case_study_criteria) == 18,
  sum(case_study_criteria$Error_certainty == "Always") == 4,
  sum(case_study_criteria$Error_certainty == "Sometimes") == 10,
  sum(case_study_criteria$Error_certainty == "Rarely") == 4,
  n_retained_core == 14,
  n_filtered_core == 4,
  n_related_case_study == 18,
  n_total_case_study == 32
)

cat("\nCase-study core-criteria filtering\n")
print(case_study_summary)
cat(
  "\nCase-study expansion:",
  n_retained_core,
  "retained core +",
  n_related_case_study,
  "linked related =",
  n_total_case_study,
  "total criteria\n"
)

case_study_sankey <- case_study_criteria %>%
  count(
    Model_stage,
    Error_certainty,
    Applicability,
    name = "n_criteria",
    .drop = FALSE
  ) %>%
  filter(n_criteria > 0)

stopifnot(sum(case_study_sankey$n_criteria) == 18)


# 3. Plot model-stage counts through certainty filtering ----
case_study_figure <- ggplot(
  case_study_sankey,
  aes(
    axis1 = Model_stage,
    axis2 = Error_certainty,
    axis3 = Applicability,
    y = n_criteria
  )
) +
  geom_alluvium(
    aes(fill = Model_stage, alpha = Applicability),
    width = 0.12,
    knot.pos = 0.45
  ) +
  geom_stratum(width = 0.13, fill = "#f7f7f7", colour = "#555555") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.2,
    lineheight = 0.9
  ) +
  annotate(
    "label",
    x = 1.5,
    y = 19.3,
    label = paste(
      "Error-type filter",
      "Underprediction retained",
      "Overprediction excluded",
      sep = "\n"
    ),
    size = 3.2,
    fontface = "bold",
    fill = "#f7f7f7",
    colour = "#333333",
    label.size = 0.25
  ) +
  scale_x_discrete(
    limits = c(
      "Model stage",
      "Underprediction error certainty",
      "Filtering outcome"
    ),
    expand = c(0.16, 0.16)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_fill_manual(
    values = stage_colours,
    name = "Model stage"
  ) +
  scale_alpha_manual(
    values = c(
      "Retained" = 0.8,
      "Filtered out" = 0.3
    ),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "left",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Filtering criteria for environmental impact assessment",
    subtitle = paste(
      "Case study: anthropogenic-disturbance screening for the golden-winged warbler;",
      "prediction model, Correctness severity tolerance, and Sometimes certainty tolerance."
    ),
    x = NULL,
    y = NULL,
    caption = paste(
      "Ribbon width represents the number of core criteria. Four Always and 10 Sometimes criteria are retained;",
      "four Rarely criteria are filtered out. Colours identify the model stage."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, colour = "#444444"),
    plot.caption = element_text(size = 8, colour = "#555555"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )

# 4. Create an unlabelled version for external editing ----
case_study_figure_unlabelled <- ggplot(
  case_study_sankey,
  aes(
    axis1 = Model_stage,
    axis2 = Error_certainty,
    axis3 = Applicability,
    y = n_criteria
  )
) +
  geom_alluvium(
    aes(fill = Model_stage, alpha = Applicability),
    width = 0.12,
    knot.pos = 0.45
  ) +
  geom_stratum(width = 0.13, fill = "#f7f7f7", colour = "#555555") +
  scale_x_discrete(
    limits = c(
      "Model stage",
      "Underprediction error certainty",
      "Filtering outcome"
    ),
    expand = c(0.16, 0.16)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_fill_manual(values = stage_colours, name = "Model stage") +
  scale_alpha_manual(
    values = c(
      "Retained" = 0.8,
      "Filtered out" = 0.3
    ),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "left",
      title.hjust = 0.5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )


# 5. Save the labelled and unlabelled case-study figures ----
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(
  figure_dir,
  "case_study_criteria_filtering_sankey.png"
)

unlabelled_figure_path <- file.path(
  figure_dir,
  "case_study_criteria_filtering_sankey_unlabelled.png"
)

ggsave(
  filename = figure_path,
  plot = case_study_figure,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = unlabelled_figure_path,
  plot = case_study_figure_unlabelled,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

cat(
  "\nSaved case-study figures to\n",
  figure_path,
  "\n",
  unlabelled_figure_path,
  "\n",
  sep = ""
)
