test_that("app_server initializes and processes inputs correctly", {
  options(repos = c(CRAN = "http://cran.rstudio.com/"))

  testServer(app_server, {
    # Simulate input for 'pac' (package name)
    session$setInputs(pac = "shiny")

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
      session$setInputs(version_old = old_version)
      session$setInputs(version_new = new_version)

      # Check that 'pac_news' reactive returns NEWS content
      news_content <- pac_news()
      expect_true(is.character(news_content))
      expect_true(nzchar(news_content))

      # Check that 'output$news_diff' generates output
      news_diff_output <- output$news_diff
      expect_true(is.character(news_diff_output))
      expect_true(nzchar(news_diff_output))
    }

    # Simulate inputs for downloads tab
    session$setInputs(
      date_input = c(Sys.Date() - 30, Sys.Date()),
      date_unit = "day",
      pac_compare = "ggplot2",
      add_smooth = FALSE
    )

    # Check that 'download_pacs' reactive returns data
    download_data <- download_pacs()
    expect_s3_class(download_data, "data.frame")
    expect_true(nrow(download_data) > 0)

    # Since 'output$downloads_plot' is a plot, we can't directly test it here

    # Simulate inputs for dependencies tab
    user_dependencies <- user_deps()
    expect_s3_class(user_dependencies, "data.frame")

    dev_dependencies <- dev_deps()
    expect_s3_class(dev_dependencies, "data.frame")

    # Check outputs for package info tab
    expect_equal(output$name, "shiny")
    expect_true(nzchar(output$title))
    expect_true(nzchar(output$description))
    expect_true(nzchar(output$last_version))
    expect_true(nzchar(output$license))

    # Check outputs for CRAN check results
    check_results <- pacs::pac_checkpage("shiny")
    if (length(check_results) > 0) {
      expect_s3_class(check_results, "data.frame")
    }

  })
})
