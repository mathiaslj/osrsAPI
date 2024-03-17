#' Create a tibble of information about the object
#'
#' @param .x An object returned from [osrs_api()]
#'
#' @return A tibble with information about items, prices, dates
#' @export
#'
#' @examples
#' # Get the entire price history of the abyssal whip
#' whip <- osrs_api(items = "Abyssal_whip", history = "all")
#'
#' # Create a data set with information about the item
#' create_item_data(whip)
#'
#' # Same can be done for other values of history
#' osrs_api(items = c("bucket", "Abyssal whip"), history = "latest") |>
#'   create_item_data()
#' osrs_api(items = c("Dragon dagger"), history = "sample") |>
#'   create_item_data()
#'
create_item_data <- function(.x) {
  contents <- .x$content

  item_data <- lapply(contents, function(x) dplyr::bind_rows(x)) |>
    bind_rows(.id = "item")

  # If we have not gotten just a sample, give the relative day of the price
  if (!grepl(.x$path, "sample")) {
    item_data <- item_data |>
      group_by(item) |>
      arrange(desc(timestamp),
              .by_group = TRUE) |>
      mutate(rel_day = row_number() - 1,
             date = Sys.Date() - rel_day,
             .after = "price") |>
      ungroup() |>
      select(- c("timestamp"))
  }

  return(item_data)
}
