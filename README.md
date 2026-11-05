# rdevdash

[![R-CMD-check](https://github.com/Polkas/rdevdash/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Polkas/rdevdash/actions/workflows/R-CMD-check.yaml)

Welcome to the R Package Developer Dashboard, a comprehensive tool designed to streamline the process of tracking and managing CRAN packages.

Key Dashboard Features:
- **Package Overview**: The latest version, license, maintainer, links, CRAN check health and last month's downloads (with a sparkline) at a glance. Expand **GitHub issues** for open/closed issue counts and links (pull requests excluded). Repository links are discovered from both `URL` and `BugReports`.
- **Package Downloads**: Interactive (zoom/hover) download trends aggregated by day, week, month or year, comparisons with other packages, optional trend lines, log scale and CSV export.
- **Version Differences**: NEWS, DESCRIPTION dependencies and NAMESPACE differences between any two releases.
- **CRAN Check Results**: Check results for every CRAN flavor with a status summary.
- **Package Dependencies**: Recursive user and developer dependencies, plus the heaviest ones and the package's declared `SystemRequirements`.
- **CRAN Releases**: Full release history and an interactive release timeline.

Quality of life:
- Modern [`bslib`](https://rstudio.github.io/bslib/) (Bootstrap 5) interface with a **light/dark mode** toggle.
- **Autocomplete search** over all CRAN packages.
- **Shareable links** - the selected package and page are kept in the URL, e.g. `?pkg=dplyr&tab=downloads`.
- **In-memory caching** of network results for up to 6 hours, shared by sessions within the same R worker. Cached results avoid repeated requests; errors are not cached. Each worker has its own cache, which is cleared on restart or redeployment.
- Release history is fetched only on the Versions and Releases pages. GitHub issue counts are fetched only when their panel is expanded, then cached.
- Friendly messages instead of errors when something is not available (e.g. a package without a NEWS file).
- Release history prefers CRANDB to avoid slow CRAN HTML scraping, with CRAN as a fallback; failed requests are logged for deployment troubleshooting.

## Run locally

```r
# install.packages("pak")
pak::local_install_deps()
shiny::runApp()
```

## Deploy to Posit Connect Cloud

The repository includes a `manifest.json` that records the deployment's R
version and package dependencies. Regenerate it after changing dependencies
or updating the app:

```r
install.packages("rsconnect") # if not already installed
rsconnect::writeManifest(appDir = ".", appMode = "shiny")
```

Commit the updated manifest along with the app changes, push to the branch
configured in Connect Cloud, and publish again. The app loads its package source
with `pkgload` at startup, so `pkgload` is a runtime dependency, not just a
development suggestion. Shiny 1.9.0 or newer is required for busy indicators.

This dashboard is primarily powered by the `pacs` package, which is an essential toolkit for R developers looking to gain insights into package development, maintenance, and deployment on CRAN.

What is `pacs` ?

`pacs` is an R package that provides a suite of functions to interact with CRAN packages, analyze dependencies, compare package versions, and much more. It serves as the backbone of this dashboard. 
For more information, visit [the pacs CRAN page](https://CRAN.R-project.org/package=pacs).
