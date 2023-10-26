with_mock_dir("api/list", {
  test_that("Can find apis by string", {
    test_result <- find_api("events")
    expect_contains(test_result$title, c("Events API", "AWS IoT Events"))
    expect_contains(test_result$service, c("events", "twilio_events_v1"))
    memoise::forget(.fetch_apis_impl)
  })
  test_that("find_api memoisation works", {
    local_mocked_bindings(
      .list_apis = function() {
        message("list_apis called")
        tibble::tibble(
          name = "a",
          preferred = "1.0",
          versions = list(
            tibble::tibble(
              version = "1.0",
              info = list(title = "A"),
              swaggerUrl = "a"
            )
          )
        )
      }
    )
    expect_message(find_api("x"), "list_apis called")
    expect_no_message(find_api("x"))
    expect_message(find_api("x", TRUE), "list_apis called")
    memoise::forget(.fetch_apis_impl)
  })
})
