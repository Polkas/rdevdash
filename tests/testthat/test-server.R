test_that("app_server initializes and processes inputs correctly", {
  skip_on_cran()
  skip_if_offline()
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  testServer(app_server, {
    # Simulate input for 'pac' (package name)
    session$setInputs(pac = "shiny", nav = "version_diff")

    # Check that 'pac' reactive returns the correct package name
    expect_equal(pac(), "shiny")

    # Check that 'last_version' reactive returns a version string
    expect_true(is.character(last_version()))
    expect_true(nzchar(last_version()))

    # Check that 'get_timemachine' reactive returns a data frame
    timemachine_data <- get_timemachine()
    expect_s3_class(timemachine_data, "data.frame")
    expect_true(nrow(timemachine_data) > 0)

    # Check that 'get_description' reactive returns a list
    description_data <- get_description()
    expect_type(description_data, "list")
    expect_true(length(description_data) > 0)

    # Simulate selection of versions in the UI
    versions <- timemachine_data$Version
    if (length(versions) >= 2) {
      old_version <- versions[length(versions) - 1]
      new_version <- versions[length(versions)]
      session$setInputs(version_old = old_version, version_new = new_version)

      # Check that 'pac_news' reactive returns NEWS content
      news_content <- pac_news()
      expect_true(is.character(news_content$text))
      expect_true(nzchar(news_content$text))

      # Check that 'output$news_diff' generates output
      news_diff_output <- output$news_diff
      expect_true(nzchar(news_diff_output$html))
    }

    # Simulate inputs for downloads tab
    session$setInputs(
      date_input = c(Sys.Date() - 30, Sys.Date() - 1),
      date_unit = "day",
      pac_compare = "ggplot2",
      add_smooth = FALSE
    )

    # Check that 'download_pacs' reactive returns data for both packages
    download_data <- download_pacs()
    expect_s3_class(download_data, "data.frame")
    expect_true(nrow(download_data) > 0)
    expect_setequal(unique(download_data$package), c("shiny", "ggplot2"))

    # Dependencies
    user_dependencies <- user_deps()
    expect_s3_class(user_dependencies, "data.frame")

    dev_dependencies <- dev_deps()
    expect_s3_class(dev_dependencies, "data.frame")
    expect_gte(nrow(dev_dependencies), nrow(user_dependencies))

    # Check outputs for package info tab
    expect_equal(output$name, "shiny")
    expect_true(nzchar(output$title))
    expect_true(nzchar(output$description))
    expect_true(nzchar(output$last_version))
    expect_true(nzchar(output$license))

    # Check outputs for CRAN check results
    check_results <- checkpage()
    if (length(check_results) > 0) {
      expect_s3_class(check_results, "data.frame")
      expect_true(check_overall() %in% c("OK", "NOTE", "WARN", "ERROR", "FAIL"))
    }

    # Releases
    expect_equal(as.numeric(output$number_releases), nrow(timemachine_data))
  })
})

test_that("app_server rejects packages that are not on CRAN", {
  skip_on_cran()
  skip_if_offline()
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  testServer(app_server, {
    session$setInputs(pac = "surely.not.a.cran.package")
    expect_error(pac(), "not a valid CRAN package")
  })
})

test_that("Overview never fetches release history and Versions loads it on demand", {
  app_cache()$reset()
  on.exit(app_cache()$reset(), add = TRUE)
  calls <- 0L
  issue_calls <- 0L
  local_mocked_bindings(
    cran_packages = function() c("shiny", "example"),
    github_issue_counts = function(repo) {
      issue_calls <<- issue_calls + 1L
      c(open = 12, closed = 345)
    },
    fetch_release_history = function(pac) {
      calls <<- calls + 1L
      data.frame(
        Package = pac,
        Version = c("1.0", "2.0"),
        Released = as.Date(c("2024-01-01", "2024-02-01")),
        Archived = as.Date(c("2024-02-01", NA)),
        LifeDuration = as.difftime(c(31, 60), units = "days")
      )
    }
  )
  local_mocked_bindings(
    pac_last = function(...) "2.0",
    pac_description = function(...) list(
      Title = "Test package", Description = "An offline fixture.",
      `Date/Publication` = "2024-02-01",
      URL = "https://docs.example.org",
      BugReports = "https://github.com/owner/project/issues",
      SystemRequirements = "C++17,\n GNU make"
    ),
    pac_checkpage = function(...) data.frame(Flavor = "test", Status = "OK"),
    pac_checkred = function(...) FALSE,
    pac_deps_user = function(...) data.frame(Package = character(0), Version = character(0)),
    pac_deps_dev = function(...) data.frame(Package = character(0), Version = character(0)),
    pac_deps_heavy = function(...) data.frame(Package = character(0), NrUniqueDeps = integer(0)),
    .package = "pacs"
  )
  local_mocked_bindings(
    cran_downloads = function(...) data.frame(date = Sys.Date() - 1, package = "shiny", count = 10),
    .package = "cranlogs"
  )
  testServer(app_server, {
    session$setInputs(pac = "shiny", nav = "overview")
    expect_equal(calls, 0L)
    expect_equal(issue_calls, 0L)
    expect_match(output$pkg_summary$html, 'href="https://github.com/owner/project"')
    expect_equal(output$system_requirements, "C++17, GNU make")
    session$setInputs(nav = "version_diff")
    expect_equal(calls, 1L)
    session$setInputs(nav = "overview", pac = "example")
    expect_equal(calls, 1L)
    session$setInputs(nav = "cran_releases")
    expect_equal(get_timemachine()$Version, c("1.0", "2.0"))
    expect_equal(calls, 2L)
    session$setInputs(nav = "overview", github_details = "issues")
    expect_match(output$github_issues$html, "Open issues:")
    expect_match(output$github_issues$html, "345")
    expect_match(output$github_issues$html, "is%3Aissue\\+is%3Aclosed")
    expect_equal(issue_calls, 1L)
  })
})

test_that("cached results are reused between Shiny sessions in the same worker", {
  calls <- 0L
  key <- list("cross-session-cache", runif(1))
  local_mocked_bindings(cran_packages = function() "shiny")
  for (i in seq_len(2)) {
    testServer(app_server, {
      expect_equal(cached(key, {
        calls <<- calls + 1L
        "shared"
      }), "shared")
    })
  }
  expect_equal(calls, 1L)
})
