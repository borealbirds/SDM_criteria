# Shiny App

This folder contains the modular Shiny application:

- `global.R` loads data and shared configuration;
- `ui.R` defines the interface; and
- `server.R` implements filtering, criterion relationships, and reports.

The app reads its source files from `data/`.

From the repository root, run:

```r
shiny::runApp("app")
```

From this `app` directory, run:

```r
shiny::runApp(".")
```

Run the full lookup-table validator from the repository root before publishing
data changes:

```r
source("validate_app_data.R")
```

See the [project README](../README.md) for package requirements, filtering
logic, criteria relationships, data schemas, and report behavior.
