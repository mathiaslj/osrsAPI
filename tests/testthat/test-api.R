test_that("Gives error when requesting items that does not exist", {
  expect_error(osrs_api(items = "bob"))
})

test_that("Gives error when trying to give wrong history", {
  expect_error(osrs_api(items = "Abyssal_whip", history = "something"))
})

test_that("Correctly returns something when giving valid items name with wrong
          capitalisation and backspace. Can give several items", {
  expect_no_condition(osrs_api(items = c("abyssal Whip", "  bucket")))
  expect_named(osrs_api(items = c("abyssal Whip", "  bucket")), c("content", "path", "response"))
})

test_that("Gives error when trying to fetch several items from many days", {
  expect_error(osrs_api(items = c("abyssal Whip", "  bucket"), history = "last90d"))
})
