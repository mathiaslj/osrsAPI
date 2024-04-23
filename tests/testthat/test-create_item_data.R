test_that("create_item_data works with 2 items", {
  expect_type(create_item_data("Abyssal whip|bucket",
                                   history = "latest"),
              "list")
})

test_that("create_item_data has created a column 'date' if the history argument is not sample", {
  expect_named(create_item_data("Abyssal whip",
                                history = "all") |>
                 dplyr::select(tidyselect::any_of("date")),
               "date")
})

test_that("create_item_data can still run but without a 'date' column if history argument is sample", {
  expect_length(create_item_data("Abyssal whip",
                                 history = "sample") |>
                  dplyr::select(tidyselect::any_of("date")),
                0)
})
