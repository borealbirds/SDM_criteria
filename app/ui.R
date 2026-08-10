# --- 2. UI ---
ui <- fluidPage(
    tags$head(
        tags$style(HTML(
            "
      th { vertical-align: bottom !important; white-space: normal !important; }
      .dataTables_wrapper { overflow-x: hidden !important; }
      .centered-intro { text-align: center; margin-bottom: 10px; }
      .centered-intro h3 { margin-bottom: 2px; }
      .step-heading { font-weight: 700; color: #2c3e50; margin: 14px 0 6px; }
      .sidebar-title { font-size: 1.2em; margin-top: 0; margin-bottom: 12px; }
      .section-title { background-color: #2c3e50; color: white; padding: 10px 15px; border-radius: 5px; margin-bottom: 15px; font-size: 1.1em; font-weight: bold; }
      .custom-legend { text-align: center; margin-top: 5px; margin-bottom: 15px; padding: 8px; background: #f8f9fa; border-radius: 8px; border: 1px solid #ddd; }
      .legend-item { display: inline-block; margin: 0 12px; font-size: 0.8em; font-weight: bold; }
      .legend-box { display: inline-block; width: 10px; height: 10px; margin-right: 5px; border: 1px solid #666; vertical-align: middle; }
      .report-box { border: 1px solid #dee2e6; padding: 20px; border-radius: 8px; background-color: #ffffff; margin-bottom: 20px; }
      .profile-box { border: 1px solid #dee2e6; padding: 15px; border-radius: 8px; background-color: #ffffff; margin-bottom: 15px; }
      .profile-box-muted { background-color: #f8f9fa; }
      .profile-box-info { background-color: #e8f4f8; }
      .plot-box { background-color: #ffffff; border: 1px solid #ddd; padding: 5px; border-radius: 8px; margin-bottom: 10px; }
      .plot-title { text-align: center; font-weight: bold; margin-bottom: 0; font-size: 0.8em; }
      .criteria-scroll { height: 40vh; overflow-y: auto; padding-right: 5px; }
      .details-box { height: 85vh; overflow-y: auto; background-color: #fcfcfc; border: 1px solid #ddd; border-top: 5px solid #18bc9c; padding: 15px; }
      .empty-state { padding: 16px; border: 1px dashed #bbb; border-radius: 8px; color: #666; background-color: #fafafa; }
      .criteria-count { margin-bottom: 12px; color: #2c3e50; font-weight: 600; }
      .risk-note { font-size: 0.85em; color: #555; margin-top: -4px; margin-bottom: 8px; }
      .report-actions { display: flex; gap: 10px; flex-wrap: wrap; }
      .well .form-group { margin-bottom: 8px !important; }
      .well hr { margin-top: 10px !important; margin-bottom: 10px !important; }
      .well h4 { margin-bottom: 5px !important; font-size: 1.1em; }
    "
        ))
    ),
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    sidebarLayout(
        sidebarPanel(
            h4(class = "sidebar-title", "Select your model use-case scenario"),
            h4("Model Application"),
            div(
                class = "risk-note",
                "Select a broad category to narrow the application list, or select a specific application directly from the full list. The two fields update each other."
            ),
            actionButton(
                "view_applications",
                "View all applications as table",
                class = "btn-info w-100"
            ),
            selectInput(
                "cat_select",
                "Select model application category:",
                choices = c(
                    "Select a category" = "",
                    APPLICATION_CATEGORY_CHOICES
                ),
                selected = ""
            ),
            selectInput(
                "app_select",
                "Select model application:",
                choices = c(
                    "All applications in category" = "",
                    APPLICATION_CHOICES
                ),
                selected = ""
            ),
            textAreaInput(
                "user_purpose",
                "Add any further details about the biodiversity conservation application of your model to include in a report of model criteria.",
                placeholder = "Enter your specific project goals here...",
                rows = 2
            ),
            hr(),
            h4("Model Type"),
            div(
                class = "risk-note",
                "Select whether you are using your SDM for prediction or projection for the selected application."
            ),
            selectInput(
                "type_select",
                "Select model type:",
                choices = MODEL_TYPES,
                selected = "Prediction"
            ),
            hr(),
            h4("Error Tolerance"),
            div(
                class = "risk-note",
                "Select how severe the potential effect of model error on the desired application must be before the relevant criterion is included."
            ),
            selectInput(
                "severity_tolerance",
                "Select error severity tolerance:",
                choices = SEVERITY_TOLERANCE_CHOICES,
                selected = "Medium"
            ),
            div(
                class = "risk-note",
                "Select how consistently a criterion violation must lead to error before the criterion is included."
            ),
            selectInput(
                "certainty_tolerance",
                "Select error certainty tolerance:",
                choices = CERTAINTY_TOLERANCE_CHOICES,
                selected = "Medium"
            ),
            hr(),
            actionButton(
                "filter_criteria",
                "Filter Criteria",
                class = "btn-primary w-100"
            )
        ),

        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Overview & Instructions",
                    br(),
                    uiOutput("full_landing_page")
                ),
                tabPanel(
                    "Selected use-case scenario",
                    br(),
                    uiOutput("app_profile_display")
                ),
                tabPanel(
                    "Applicable Criteria",
                    br(),
                    uiOutput("applicable_criteria_tab")
                ),
                tabPanel(
                    "Generate a Report",
                    br(),
                    uiOutput("generate_report_tab")
                ),
                tabPanel(
                    "Glossary",
                    br(),
                    div(class = "section-title", "Glossary"),
                    div(class = "profile-box", DTOutput("glossary_table"))
                )
            )
        )
    )
)
