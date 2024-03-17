test_that("create_item_data works with 2 items", {
  expect_type(create_item_data(osrs_api("Abyssal whip|bucket",
                                     history = "latest")),
              "list")
})

test_that("create_item_data works with 2 items", {
  expect_type(create_item_data(osrs_api("Abyssal whip|bucket",
                                     history = "all")),
              "list")
})

test_that("create_item_data has created a column 'date' if the history argument is not sample", {
            expect_named(osrs_api("Abyssal whip",
                                  history = "all") |>
                           create_item_data() |>
                           dplyr::select(tidyselect::any_of("date")),
                         "date")
          })

test_that("create_item_data can still run but without a 'date' column if history argument is sample", {
  expect_length(osrs_api("Abyssal whip",
                        history = "sample") |>
                 create_item_data() |>
                 dplyr::select(tidyselect::any_of("date")),
               0)
})
