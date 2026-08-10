# --- 3. Server ---
server <- function(input, output, session) {
    selected_row_data <- reactiveVal(NULL)
    submitted_purpose <- reactiveVal("")
    applied_filters <- reactiveVal(DEFAULT_FILTERS)
    filters_applied <- reactiveVal(FALSE)

    current_filters <- reactive({
        applied_filters()
    })

    filter_prompt_ui <- function() {
        div(class = "profile-box", p(em(FILTER_PROMPT)))
    }

    selected_error_types_ui <- function() {
        category_row <- selected_category()
        active <- active_error_types()
        filters <- current_filters()
        pathway_df <- data.frame(
            `Error type` = c("Underprediction", "Overprediction"),
            Effect = c(
                category_row$Underprediction_error_effect,
                category_row$Overprediction_error_effect
            ),
            Rank = c(
                category_row$Underprediction_error_severity_rank,
                category_row$Overprediction_error_severity_rank
            ),
            Included = c(
                ifelse("Underprediction" %in% active, "Yes", "No"),
                ifelse("Overprediction" %in% active, "Yes", "No")
            ),
            check.names = FALSE
        )
        pathway_table <- tags$table(
            class = "table table-sm table-striped",
            tags$thead(tags$tr(lapply(names(pathway_df), tags$th))),
            tags$tbody(lapply(seq_len(nrow(pathway_df)), function(i) {
                tags$tr(lapply(pathway_df[i, ], tags$td))
            }))
        )
        tagList(
            div(class = "section-title", "Selected error type(s)"),
            div(
                class = "profile-box profile-box-muted",
                p(
                    "Application severity determines which error pathways are considered before criterion certainty is applied."
                ),
                p(
                    strong("Selected error severity tolerance: "),
                    severity_tolerance_label(filters$severity_tolerance)
                ),
                p(
                    strong("Selected error certainty tolerance: "),
                    certainty_tolerance_label(filters$certainty_tolerance)
                ),
                p(
                    strong("Error Types Considered: "),
                    ifelse(
                        length(active) == 0,
                        "None at this severity tolerance",
                        paste(active, collapse = ", ")
                    )
                ),
                pathway_table,
                p(
                    strong("Underprediction effect: "),
                    category_row$Underprediction_error_effect,
                    " - ",
                    plain_display_text(
                        category_row$Underprediction_error_effect_definition
                    )
                ),
                p(
                    strong("Overprediction effect: "),
                    category_row$Overprediction_error_effect,
                    " - ",
                    plain_display_text(
                        category_row$Overprediction_error_effect_definition
                    )
                )
            )
        )
    }

    output$applicable_criteria_tab <- renderUI({
        if (!filters_applied()) {
            return(filter_prompt_ui())
        }
        tagList(
            selected_error_types_ui(),
            fluidRow(
                column(
                    8,
                    div(class = "section-title", "List of selected criteria"),
                    p("Click on any row to display details."),
                    div(
                        class = "criteria-scroll",
                        uiOutput("criteria_tables_ui")
                    ),
                    hr(),
                    div(
                        class = "section-title",
                        "Summary of applicable criteria along the model cycle"
                    ),
                    fluidRow(
                        column(
                            6,
                            div(
                                class = "plot-box",
                                h6(
                                    class = "plot-title",
                                    "Underprediction Criteria"
                                ),
                                plotlyOutput(
                                    "radar_underprediction",
                                    height = "280px"
                                )
                            )
                        ),
                        column(
                            6,
                            div(
                                class = "plot-box",
                                h6(
                                    class = "plot-title",
                                    "Overprediction Criteria"
                                ),
                                plotlyOutput(
                                    "radar_overprediction",
                                    height = "280px"
                                )
                            )
                        )
                    ),
                    div(
                        class = "custom-legend",
                        div(
                            class = "legend-item",
                            span(
                                class = "legend-box",
                                style = paste0(
                                    "background:",
                                    TOLERANCE_COLORS["High"]
                                )
                            ),
                            "High"
                        ),
                        div(
                            class = "legend-item",
                            span(
                                class = "legend-box",
                                style = paste0(
                                    "background:",
                                    TOLERANCE_COLORS["Medium"]
                                )
                            ),
                            "Medium"
                        ),
                        div(
                            class = "legend-item",
                            span(
                                class = "legend-box",
                                style = paste0(
                                    "background:",
                                    TOLERANCE_COLORS["Low"]
                                )
                            ),
                            "Low"
                        )
                    )
                ),
                column(
                    4,
                    div(class = "section-title", "Criterion Details"),
                    wellPanel(class = "details-box", uiOutput("details_panel"))
                )
            )
        )
    })

    output$generate_report_tab <- renderUI({
        if (!filters_applied()) {
            return(filter_prompt_ui())
        }
        tagList(
            div(class = "section-title", "Prepared by"),
            div(
                class = "report-box",
                fluidRow(
                    column(
                        4,
                        textInput("user_name", "User's Name:", value = "")
                    ),
                    column(
                        4,
                        textInput("report_date", "Date:", value = Sys.Date())
                    ),
                    column(
                        4,
                        textAreaInput(
                            "team_overview",
                            "Overview for my team:",
                            rows = 2
                        )
                    )
                ),
                hr(),
                div(
                    class = "report-actions",
                    downloadButton(
                        "download_docx",
                        "Export as .docx",
                        class = "btn-success"
                    ),
                    downloadButton(
                        "download_csv",
                        "Export as CSV",
                        class = "btn-info"
                    )
                )
            ),
            uiOutput("report_tab_content")
        )
    })

    rank_at_or_above <- function(rank_value, tolerance) {
        if (is.null(tolerance) || tolerance == "") {
            return(TRUE)
        }
        value_rank <- match(rank_value, TOLERANCE_LEVELS)
        tolerance_rank <- match(tolerance, TOLERANCE_LEVELS)
        !is.na(value_rank) &
            !is.na(tolerance_rank) &
            value_rank <= tolerance_rank
    }

    tolerance_label <- function(tolerance) {
        if (is.null(tolerance) || tolerance == "") {
            return("No tolerance selected")
        }
        paste(tolerance, "or higher")
    }

    severity_tolerance_label <- function(tolerance) {
        label <- names(SEVERITY_TOLERANCE_CHOICES)[match(
            tolerance,
            SEVERITY_TOLERANCE_CHOICES
        )]
        ifelse(
            is.na(label),
            tolerance_label(tolerance),
            paste(label, "or higher")
        )
    }

    certainty_tolerance_label <- function(tolerance) {
        label <- names(CERTAINTY_TOLERANCE_CHOICES)[match(
            tolerance,
            CERTAINTY_TOLERANCE_CHOICES
        )]
        ifelse(
            is.na(label),
            tolerance_label(tolerance),
            paste(label, "or higher")
        )
    }

    selected_category <- reactive({
        filters <- current_filters()
        category_name <- filters$cat_select
        if (
            (is.null(category_name) || category_name == "") &&
                !is.null(filters$app_select) &&
                filters$app_select != ""
        ) {
            app_row <- application_inventory %>%
                filter(Application == filters$app_select) %>%
                slice(1)
            req(nrow(app_row) > 0)
            category_name <- app_row$Application_category
        }
        req(category_name)
        category_row <- application_categories %>%
            filter(Application_category == category_name) %>%
            slice(1)
        req(nrow(category_row) > 0)
        category_row
    })

    selected_applications <- reactive({
        filters <- current_filters()
        if (is.null(filters$cat_select) || filters$cat_select == "") {
            return(
                application_inventory %>%
                    arrange(Application_category, Application)
            )
        }
        application_inventory %>%
            filter(Application_category == filters$cat_select) %>%
            arrange(Application)
    })

    selected_application <- reactive({
        filters <- current_filters()
        if (is.null(filters$app_select) || filters$app_select == "") {
            return(NULL)
        }
        app_row <- application_inventory %>%
            filter(Application == filters$app_select) %>%
            slice(1)
        if (nrow(app_row) == 0) {
            return(NULL)
        }
        app_row
    })

    selected_certainty_columns <- reactive({
        filters <- current_filters()
        req(filters$type_select)
        model_prefix <- tolower(filters$type_select)
        c(
            underprediction = paste0(
                "Underprediction_",
                model_prefix,
                "_error_certainty"
            ),
            overprediction = paste0(
                "Overprediction_",
                model_prefix,
                "_error_certainty"
            )
        )
    })

    active_error_types <- reactive({
        category_row <- selected_category()
        filters <- current_filters()
        severity_tol <- filters$severity_tolerance
        active <- c()
        if (
            rank_at_or_above(
                category_row$Underprediction_error_severity_rank,
                severity_tol
            )
        ) {
            active <- c(active, "Underprediction")
        }
        if (
            rank_at_or_above(
                category_row$Overprediction_error_severity_rank,
                severity_tol
            )
        ) {
            active <- c(active, "Overprediction")
        }
        active
    })

    criteria_with_framework_fields <- reactive({
        cols <- selected_certainty_columns()
        criteria_df %>%
            mutate(
                Underprediction_certainty = .data[[cols[["underprediction"]]]],
                Overprediction_certainty = .data[[cols[["overprediction"]]]]
            ) %>%
            left_join(
                criteria_error %>%
                    rename(
                        Underprediction_certainty = Error_certainty,
                        Underprediction_certainty_definition = Definition,
                        Underprediction_certainty_rank = Error_certainty_rank
                    ),
                by = "Underprediction_certainty"
            ) %>%
            left_join(
                criteria_error %>%
                    rename(
                        Overprediction_certainty = Error_certainty,
                        Overprediction_certainty_definition = Definition,
                        Overprediction_certainty_rank = Error_certainty_rank
                    ),
                by = "Overprediction_certainty"
            )
    })

    criterion_match_reason <- function(under_match, over_match) {
        if (under_match && over_match) {
            return("Matched underprediction and overprediction")
        }
        if (under_match) {
            return("Matched underprediction")
        }
        if (over_match) {
            return("Matched overprediction")
        }
        "Not selected"
    }

    filtered_data <- reactive({
        active <- active_error_types()
        if (length(active) == 0) {
            return(criteria_with_framework_fields()[0, ])
        }

        filters <- current_filters()
        certainty_tol <- filters$certainty_tolerance
        criteria_with_framework_fields() %>%
            rowwise() %>%
            mutate(
                Underprediction_selected = "Underprediction" %in%
                    active &&
                    rank_at_or_above(
                        Underprediction_certainty_rank,
                        certainty_tol
                    ),
                Overprediction_selected = "Overprediction" %in%
                    active &&
                    rank_at_or_above(
                        Overprediction_certainty_rank,
                        certainty_tol
                    ),
                Match_reason = criterion_match_reason(
                    Underprediction_selected,
                    Overprediction_selected
                )
            ) %>%
            ungroup() %>%
            filter(Underprediction_selected | Overprediction_selected)
    })

    display_certainty <- function(value, selected) {
        if (isTRUE(selected)) value else ""
    }

    get_tolerance_style <- function(level) {
        color <- TOLERANCE_COLORS[level]
        if (is.na(color)) {
            color <- "#ffffff"
        }
        paste0(
            "background-color: ",
            color,
            "; padding: 2px 8px; border-radius: 4px; font-weight: bold;",
            " border: 1px solid rgba(0,0,0,0.1); color: #333;"
        )
    }

    plain_display_text <- function(text) {
        if (is.null(text) || text == "" || is.na(text) || text == "NA") {
            return("")
        }
        text
    }

    split_semicolon_values <- function(value) {
        if (
            is.null(value) ||
                length(value) == 0 ||
                is.na(value) ||
                trimws(value) == ""
        ) {
            return(character(0))
        }
        trimws(unlist(strsplit(value, ";", fixed = TRUE)))
    }

    render_application_examples <- function(app_row) {
        if (!all(c("Example citation", "Citation URL") %in% names(app_row))) {
            return(NULL)
        }
        citations <- split_semicolon_values(app_row[["Example citation"]])
        urls <- split_semicolon_values(app_row[["Citation URL"]])
        if (length(citations) == 0 && length(urls) == 0) {
            return(NULL)
        }
        n_examples <- max(length(citations), length(urls))
        tagList(
            p(strong("Example of model application:")),
            tags$ul(lapply(seq_len(n_examples), function(i) {
                citation <- if (i <= length(citations)) {
                    citations[i]
                } else {
                    "Example paper"
                }
                url <- if (i <= length(urls)) urls[i] else ""
                tags$li(
                    plain_display_text(citation),
                    if (url != "") {
                        tagList(" ", tags$a(href = url, target = "_blank", url))
                    } else {
                        NULL
                    }
                )
            }))
        )
    }

    observeEvent(
        input$cat_select,
        {
            choices_df <- application_inventory
            if (!is.null(input$cat_select) && input$cat_select != "") {
                choices_df <- choices_df %>%
                    filter(Application_category == input$cat_select)
            }
            choices <- choices_df %>% arrange(Application) %>% pull(Application)
            current_app <- isolate(input$app_select)
            selected_app <- if (
                !is.null(current_app) && current_app %in% choices
            ) {
                current_app
            } else {
                ""
            }
            updateSelectInput(
                session,
                "app_select",
                choices = c("All applications in category" = "", choices),
                selected = selected_app
            )
            selected_row_data(NULL)
        },
        ignoreInit = FALSE
    )

    observeEvent(
        input$app_select,
        {
            if (is.null(input$app_select) || input$app_select == "") {
                return()
            }
            app_row <- application_inventory %>%
                filter(Application == input$app_select) %>%
                slice(1)
            if (nrow(app_row) == 0) {
                return()
            }
            if (!identical(input$cat_select, app_row$Application_category)) {
                updateSelectInput(
                    session,
                    "cat_select",
                    selected = app_row$Application_category
                )
            }
        },
        ignoreInit = TRUE
    )

    observeEvent(input$view_applications, {
        showModal(modalDialog(
            title = "Model Application Inventory",
            div(
                style = "text-align: right; margin-bottom: 10px;",
                modalButton("Close")
            ),
            DTOutput("application_inventory_table"),
            size = "l",
            easyClose = TRUE,
            footer = NULL
        ))
    })

    output$application_inventory_table <- renderDT({
        table_df <- application_inventory %>%
            transmute(
                `Application category` = Application_category,
                Application,
                Description
            )
        datatable(
            table_df,
            rownames = FALSE,
            filter = "top",
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })

    output$glossary_table <- renderDT({
        glossary_table <- glossary %>%
            select(-any_of("References")) %>%
            rename(Example = `Some examples`) %>%
            arrange(Term) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    autoWidth = TRUE
                )
            )
        glossary_table
    })

    observeEvent(input$filter_criteria, {
        if (
            (is.null(input$cat_select) || input$cat_select == "") &&
                (is.null(input$app_select) || input$app_select == "")
        ) {
            showNotification(
                "Select an application category or a model application before filtering criteria.",
                type = "warning"
            )
            return()
        }
        category_to_apply <- input$cat_select
        if (
            (is.null(category_to_apply) || category_to_apply == "") &&
                !is.null(input$app_select) &&
                input$app_select != ""
        ) {
            app_row <- application_inventory %>%
                filter(Application == input$app_select) %>%
                slice(1)
            if (nrow(app_row) > 0) {
                category_to_apply <- app_row$Application_category
            }
        }
        applied_filters(list(
            cat_select = category_to_apply,
            app_select = input$app_select,
            type_select = input$type_select,
            severity_tolerance = input$severity_tolerance,
            certainty_tolerance = input$certainty_tolerance
        ))
        submitted_purpose(input$user_purpose)
        filters_applied(TRUE)
        selected_row_data(NULL)
    })

    output$full_landing_page <- renderUI({
        req(landing_lines)
        is_major_heading <- function(line) {
            line %in% c("Overview", "Instructions")
        }
        is_step_heading <- function(line) str_detect(line, "^Step [0-9A-Z]")
        elements <- list(div(class = "centered-intro", h3(landing_lines[1])))
        section_lines <- character(0)
        flush_section <- function(lines) {
            if (length(lines) == 0) {
                return(NULL)
            }
            div(
                class = "profile-box",
                lapply(lines, function(line) tags$p(plain_display_text(line)))
            )
        }

        for (line in landing_lines[-1]) {
            if (is_major_heading(line) || is_step_heading(line)) {
                elements <- append(elements, list(flush_section(section_lines)))
                heading <- if (is_step_heading(line)) {
                    div(class = "step-heading", plain_display_text(line))
                } else {
                    div(class = "section-title", plain_display_text(line))
                }
                elements <- append(elements, list(heading))
                section_lines <- character(0)
            } else {
                section_lines <- c(section_lines, line)
            }
        }
        elements <- append(elements, list(flush_section(section_lines)))
        tagList(compact(elements))
    })

    output$app_profile_display <- renderUI({
        if (!filters_applied()) {
            return(filter_prompt_ui())
        }
        category_row <- selected_category()
        apps_for_category <- selected_applications()
        selected_app <- selected_application()
        filters <- current_filters()
        tagList(
            div(
                class = "section-title",
                "Description of selected use-case scenario"
            ),
            div(
                class = "profile-box",
                p(
                    "The use-case scenario is the combination of model application, model type, and error tolerance, as determined by the user."
                ),
                p(
                    strong("Selected application category: "),
                    category_row$Application_category
                ),
                tags$ul(
                    tags$li(
                        strong("Conservation decision: "),
                        plain_display_text(category_row$Question)
                    ),
                    tags$li(
                        strong("SDM use description: "),
                        plain_display_text(category_row$Action)
                    )
                ),
                if (!is.null(selected_app)) {
                    tagList(
                        p(
                            strong("Model application: "),
                            selected_app$Application
                        ),
                        p(
                            strong("Model application description: "),
                            plain_display_text(selected_app$Description)
                        ),
                        render_application_examples(selected_app)
                    )
                },
                p(strong("Model type: "), filters$type_select),
                if (submitted_purpose() != "") {
                    tagList(
                        h4(
                            style = "color: #2c3e50; font-size: 1.1em;",
                            "Report Details"
                        ),
                        p(style = "font-style: italic;", submitted_purpose())
                    )
                }
            ),
            div(class = "section-title", "Other applications in this category"),
            div(
                class = "profile-box profile-box-info",
                tags$ul(lapply(seq_len(nrow(apps_for_category)), function(i) {
                    tags$li(
                        tags$strong(apps_for_category$Application[i]),
                        ": ",
                        plain_display_text(apps_for_category$Description[i])
                    )
                }))
            )
        )
    })

    render_stage_plot <- function(error_type) {
        active <- active_error_types()
        if (!(error_type %in% active)) {
            return(render_empty_plot(paste(
                error_type,
                "is not considered at this severity tolerance"
            )))
        }
        selected_flag <- paste0(error_type, "_selected")
        data_counts <- filtered_data() %>%
            filter(.data[[selected_flag]]) %>%
            count(Model_stage, name = "n")
        if (nrow(data_counts) == 0) {
            return(render_empty_plot(paste(
                "No",
                tolower(error_type),
                "criteria selected"
            )))
        }
        data <- data.frame(
            Model_stage = MODEL_STAGE_LEVELS,
            stringsAsFactors = FALSE
        ) %>%
            left_join(data_counts, by = "Model_stage") %>%
            mutate(n = ifelse(is.na(n), 0, n)) %>%
            bind_rows(slice(., 1))
        plot_ly(
            data,
            r = ~n,
            theta = ~Model_stage,
            type = "scatterpolar",
            mode = "lines+markers",
            fill = "toself",
            line = list(color = "#18bc9c"),
            marker = list(color = "#18bc9c", size = 6),
            fillcolor = "rgba(24, 188, 156, 0.25)",
            hovertemplate = "%{theta}<br>Criteria: %{r}<extra></extra>"
        ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = TRUE,
                        rangemode = "tozero",
                        tickfont = list(size = 10)
                    ),
                    angularaxis = list(tickfont = list(size = 10))
                ),
                showlegend = FALSE,
                margin = list(l = 35, r = 35, t = 10, b = 35)
            )
    }

    render_empty_plot <- function(message) {
        plot_ly() %>%
            layout(
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                annotations = list(
                    text = message,
                    x = 0.5,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE
                ),
                margin = list(l = 20, r = 20, t = 10, b = 10)
            )
    }

    output$radar_underprediction <- renderPlotly({
        req(filters_applied())
        render_stage_plot("Underprediction")
    })

    output$radar_overprediction <- renderPlotly({
        req(filters_applied())
        render_stage_plot("Overprediction")
    })

    output$criteria_tables_ui <- renderUI({
        if (!filters_applied()) {
            return(NULL)
        }
        data <- filtered_data()
        if (nrow(data) == 0) {
            return(div(
                class = "empty-state",
                "No criteria match this combination. Try lowering the severity or certainty tolerance, or selecting another model type."
            ))
        }
        stages <- unique(data$Model_stage)
        map(stages, function(s) {
            tagList(
                h5(
                    style = "margin-top:15px; border-bottom: 1px solid #ccc; font-weight:bold; font-size: 0.9em;",
                    s
                ),
                DTOutput(paste0("table_", gsub("[^[:alnum:]]", "_", s)))
            )
        })
    })

    observe({
        req(filters_applied())
        data <- filtered_data()
        for (s in unique(data$Model_stage)) {
            local({
                curr_s <- s
                output[[paste0(
                    "table_",
                    gsub("[^[:alnum:]]", "_", curr_s)
                )]] <- renderDT({
                    df_subset <- data %>%
                        filter(Model_stage == curr_s) %>%
                        transmute(
                            Criterion,
                            Underprediction = mapply(
                                display_certainty,
                                Underprediction_certainty,
                                Underprediction_selected
                            ),
                            Overprediction = mapply(
                                display_certainty,
                                Overprediction_certainty,
                                Overprediction_selected
                            )
                        )
                    datatable(
                        df_subset,
                        selection = 'single',
                        rownames = FALSE,
                        options = list(
                            dom = 't',
                            paging = FALSE,
                            columnDefs = list(
                                list(width = '50%', targets = 0),
                                list(width = '25%', targets = 1),
                                list(width = '25%', targets = 2)
                            )
                        )
                    ) %>%
                        formatStyle(
                            'Underprediction',
                            backgroundColor = styleEqual(
                                names(CERTAINTY_COLORS),
                                CERTAINTY_COLORS
                            )
                        ) %>%
                        formatStyle(
                            'Overprediction',
                            backgroundColor = styleEqual(
                                names(CERTAINTY_COLORS),
                                CERTAINTY_COLORS
                            )
                        )
                })
                observeEvent(
                    input[[paste0(
                        "table_",
                        gsub("[^[:alnum:]]", "_", curr_s),
                        "_rows_selected"
                    )]],
                    {
                        idx <- input[[paste0(
                            "table_",
                            gsub("[^[:alnum:]]", "_", curr_s),
                            "_rows_selected"
                        )]]
                        selected_row_data(
                            data %>%
                                filter(Model_stage == curr_s) %>%
                                slice(idx)
                        )
                    }
                )
            })
        }
    })

    report_table_data <- reactive({
        req(filters_applied())
        data <- filtered_data()
        if (nrow(data) == 0) {
            return(data.frame(
                Message = "No criteria match the current selections."
            ))
        }
        data %>%
            transmute(
                Criterion,
                Model_stage,
                Model_step,
                Underprediction_certainty = mapply(
                    display_certainty,
                    Underprediction_certainty,
                    Underprediction_selected
                ),
                Overprediction_certainty = mapply(
                    display_certainty,
                    Overprediction_certainty,
                    Overprediction_selected
                ),
                Match_reason
            )
    })

    output$report_criteria_table <- renderDT({
        report_df <- report_table_data()
        table <- datatable(
            report_df,
            rownames = FALSE,
            options = list(dom = 't', paging = FALSE)
        )
        if (
            !all(
                c("Underprediction_certainty", "Overprediction_certainty") %in%
                    names(report_df)
            )
        ) {
            return(table)
        }
        table %>%
            formatStyle(
                'Underprediction_certainty',
                backgroundColor = styleEqual(
                    names(CERTAINTY_COLORS),
                    CERTAINTY_COLORS
                )
            ) %>%
            formatStyle(
                'Overprediction_certainty',
                backgroundColor = styleEqual(
                    names(CERTAINTY_COLORS),
                    CERTAINTY_COLORS
                )
            )
    })

    output$report_tab_content <- renderUI({
        if (!filters_applied()) {
            return(filter_prompt_ui())
        }
        category_row <- selected_category()
        filters <- current_filters()
        app_text <- if (
            is.null(filters$app_select) || filters$app_select == ""
        ) {
            "All applications in category"
        } else {
            filters$app_select
        }
        active <- active_error_types()
        tagList(
            div(class = "section-title", "Selected use-case scenario"),
            div(
                class = "report-box",
                p(
                    strong("Selected application category: "),
                    filters$cat_select
                ),
                p(strong("Model application: "), app_text),
                p(strong("Model type: "), filters$type_select),
                p(
                    strong("Selected error severity tolerance: "),
                    severity_tolerance_label(filters$severity_tolerance)
                ),
                p(
                    strong("Selected error certainty tolerance: "),
                    certainty_tolerance_label(filters$certainty_tolerance)
                ),
                p(
                    strong("Selected error type(s): "),
                    ifelse(
                        length(active) == 0,
                        "None",
                        paste(active, collapse = ", ")
                    )
                ),
                p(
                    strong("Underprediction effect: "),
                    category_row$Underprediction_error_effect,
                    " (",
                    category_row$Underprediction_error_severity_rank,
                    ")"
                ),
                p(
                    strong("Overprediction effect: "),
                    category_row$Overprediction_error_effect,
                    " (",
                    category_row$Overprediction_error_severity_rank,
                    ")"
                )
            ),
            div(
                class = "section-title",
                "User-provided model application details"
            ),
            div(
                class = "report-box",
                if (submitted_purpose() == "") {
                    p(em("No additional application details provided."))
                } else {
                    p(submitted_purpose())
                },
                p(
                    strong("Team overview: "),
                    ifelse(
                        is.null(input$team_overview) ||
                            input$team_overview == "",
                        "N/A",
                        input$team_overview
                    )
                )
            ),
            div(class = "section-title", "Selected criteria"),
            div(
                class = "report-box",
                div(
                    class = "criteria-count",
                    paste0("Showing ", nrow(filtered_data()), " criteria.")
                ),
                fluidRow(
                    column(
                        6,
                        h6("Underprediction Criteria", align = "center"),
                        plotlyOutput(
                            "radar_underprediction_report",
                            height = "280px"
                        )
                    ),
                    column(
                        6,
                        h6("Overprediction Criteria", align = "center"),
                        plotlyOutput(
                            "radar_overprediction_report",
                            height = "280px"
                        )
                    )
                ),
                hr(),
                DTOutput("report_criteria_table")
            )
        )
    })

    output$radar_underprediction_report <- renderPlotly({
        req(filters_applied())
        render_stage_plot("Underprediction")
    })

    output$radar_overprediction_report <- renderPlotly({
        req(filters_applied())
        render_stage_plot("Overprediction")
    })

    output$download_docx <- downloadHandler(
        filename = function() {
            paste("SDM-Report-", Sys.Date(), ".docx", sep = "")
        },
        content = function(file) {
            req(filters_applied())
            category_row <- selected_category()
            report_data <- report_table_data()
            active <- active_error_types()
            filters <- current_filters()
            app_text <- if (
                is.null(filters$app_select) || filters$app_select == ""
            ) {
                "All applications in category"
            } else {
                filters$app_select
            }

            doc <- read_docx() %>%
                body_add_par(
                    "SDM Application & Criteria Report",
                    style = "heading 1"
                ) %>%
                body_add_par(
                    paste("Prepared by:", input$user_name),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste("Report Date:", input$report_date),
                    style = "Normal"
                ) %>%
                body_add_par("", style = "Normal") %>%

                body_add_par(
                    "1. Selected use-case scenario",
                    style = "heading 2"
                ) %>%
                body_add_par(
                    paste("Selected application category:", filters$cat_select),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste("Model application:", app_text),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste("Model type:", filters$type_select),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste(
                        "Selected error severity tolerance:",
                        severity_tolerance_label(filters$severity_tolerance)
                    ),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste(
                        "Selected error certainty tolerance:",
                        certainty_tolerance_label(filters$certainty_tolerance)
                    ),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste(
                        "Selected error type(s):",
                        ifelse(
                            length(active) == 0,
                            "None",
                            paste(active, collapse = ", ")
                        )
                    ),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste(
                        "Underprediction effect:",
                        category_row$Underprediction_error_effect,
                        "(",
                        category_row$Underprediction_error_severity_rank,
                        ")"
                    ),
                    style = "Normal"
                ) %>%
                body_add_par(
                    paste(
                        "Overprediction effect:",
                        category_row$Overprediction_error_effect,
                        "(",
                        category_row$Overprediction_error_severity_rank,
                        ")"
                    ),
                    style = "Normal"
                ) %>%
                body_add_par("", style = "Normal") %>%

                body_add_par(
                    "2. User-provided model application details",
                    style = "heading 2"
                ) %>%
                body_add_par(
                    ifelse(
                        submitted_purpose() == "",
                        "No additional application details provided.",
                        submitted_purpose()
                    ),
                    style = "Normal"
                ) %>%
                body_add_par("", style = "Normal") %>%
                body_add_par("Team overview:", style = "heading 3") %>%
                body_add_par(
                    ifelse(
                        input$team_overview == "",
                        "N/A",
                        input$team_overview
                    ),
                    style = "Normal"
                ) %>%
                body_add_par("", style = "Normal") %>%

                body_add_par("3. Selected criteria", style = "heading 2") %>%
                body_add_par(
                    paste(
                        "Number of criteria included:",
                        nrow(filtered_data())
                    ),
                    style = "Normal"
                ) %>%
                body_add_par("", style = "Normal") %>%
                body_add_table(report_data, style = "table_template")

            print(doc, target = file)
        }
    )

    output$download_csv <- downloadHandler(
        filename = function() {
            paste("SDM-Criteria-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(filters_applied())
            write.csv(report_table_data(), file, row.names = FALSE)
        }
    )

    output$details_panel <- renderUI({
        if (!filters_applied()) {
            return(p(em(FILTER_PROMPT)))
        }
        res <- selected_row_data()
        if (is.null(res)) {
            return(p(em("Click a criterion to view details.")))
        }
        filters <- current_filters()
        justification <- if (identical(filters$type_select, "Projection")) {
            res$Projection_justification
        } else {
            res$Prediction_justification
        }
        tagList(
            p(strong("Criterion: "), res$Criterion),
            p(strong("Model Stage: "), res$Model_stage),
            p(strong("Model Step: "), res$Model_step),
            p(
                strong("Description:"),
                br(),
                plain_display_text(res$Description)
            ),
            p(
                strong(style = "color: #d63384;", "Violation:"),
                br(),
                div(
                    style = "font-size: 0.85em; background-color: #fff0f7; padding: 8px; border-left: 4px solid #d63384;",
                    plain_display_text(res$Violation)
                )
            ),
            p(strong("Selected Error Pathway:"), br(), res$Match_reason),
            p(
                strong("Underprediction Certainty: "),
                res$Underprediction_certainty
            ),
            p(
                strong("Overprediction Certainty: "),
                res$Overprediction_certainty
            ),
            if (!is.na(justification) && justification != "") {
                p(
                    strong("Justification:"),
                    br(),
                    plain_display_text(justification)
                )
            },
            p(
                strong(style = "color: #27ae60;", "Potential Solutions:"),
                br(),
                div(
                    style = "font-size: 0.85em; background-color: #f4fdf7; padding: 8px; border-left: 4px solid #27ae60;",
                    plain_display_text(res$Solutions)
                )
            )
        )
    })
}
