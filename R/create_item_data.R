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
    dplyr::bind_rows(.id = "item")

  # If we have not gotten just a sample, give the relative day of the price
  if (!grepl("sample", .x$path)) {
    item_data <- item_data |>
      dplyr::group_by(item) |>
      dplyr::arrange(desc(timestamp),
                     .by_group = TRUE) |>
      dplyr::mutate(rel_day = dplyr::row_number() - 1,
                    date = Sys.Date() - rel_day,
                    .after = "price") |>
      dplyr::ungroup() |>
      dplyr::select(- c("timestamp"))
  }

  return(item_data)
}
