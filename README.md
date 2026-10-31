# rdevdash

[![R-CMD-check](https://github.com/Polkas/rdevdash/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Polkas/rdevdash/actions/workflows/R-CMD-check.yaml)

Welcome to the R Package Developer Dashboard, a comprehensive tool designed to streamline the process of tracking and managing CRAN packages.

Key Dashboard Features:
- **Package Overview**: The latest version, license, maintainer, links, CRAN check health and last month's downloads (with a sparkline) at a glance.
- **Package Downloads**: Interactive (zoom/hover) download trends aggregated by day, week, month or year, comparisons with other packages, optional trend lines, log scale and CSV export.
- **Version Differences**: NEWS, DESCRIPTION dependencies and NAMESPACE differences between any two releases.
- **CRAN Check Results**: Check results for every CRAN flavor with a status summary.
- **Package Dependencies**: Recursive user and developer dependencies, plus the heaviest ones.
- **CRAN Releases**: Full release history and an interactive release timeline.

Quality of life:
- Modern [`bslib`](https://rstudio.github.io/bslib/) (Bootstrap 5) interface with a **light/dark mode** toggle.
- **Autocomplete search** over all CRAN packages.
- **Shareable links** - the selected package and page are kept in the URL, e.g. `?pkg=dplyr&tab=downloads`.
- **Caching** of all network calls (shared across sessions, 6 hours), so revisiting a package is instant.
- Friendly messages instead of errors when something is not available (e.g. a package without a NEWS file).

## Run locally

```r
# install.packages("pak")
pak::local_install_deps()
shiny::runApp()
```

This dashboard is primarily powered by the `pacs` package, which is an essential toolkit for R developers looking to gain insights into package development, maintenance, and deployment on CRAN.

What is `pacs` ?

`pacs` is an R package that provides a suite of functions to interact with CRAN packages, analyze dependencies, compare package versions, and much more. It serves as the backbone of this dashboard. 
For more information, visit [the pacs CRAN page](https://CRAN.R-project.org/package=pacs).
