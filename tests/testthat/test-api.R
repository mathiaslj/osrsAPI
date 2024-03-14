test_that("Gives error when requesting item that does not exist", {
  expect_error(osrs_api(item = "bob", history = "latest"))
})
