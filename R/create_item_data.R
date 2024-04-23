#' Create a tibble of information with prices of requested item
#'
#' @description
#' The function is a wrapper of [osrs_api()] that returns a tibble which is easier to
#' work with than the list object from the `osrs_api` function.
#'
#' @param items `character` vector of the items
#' @param history `character` with type of request. Default is `latest`, but `all` is
#' also available
#' @param user_agent `character` of the user agent
#'
#' @return A tibble with information about items, prices, dates
#' @export
#'
#' @examples
#' # Create a data set with information about the item
#' create_item_data(items = "Abyssal_whip", history = "all")
#'
#' # Same can be done for other values of history
#' create_item_data(items = c("bucket", "Abyssal whip"), history = "latest") |>
#' create_item_data(items = c("Dragon dagger"), history = "sample")
#'
create_item_data <- function(items, history = "all", user_agent = "GE_price_tracker") {
  requests <- purrr::map(items,
                         osrs_api,
                         history = history,
                         user_agent = user_agent)

  item_data <- purrr::map(requests,
                          \(x) bind_rows(x$content) |>
                            dplyr::mutate(item = names(x$content), .before = everything())) |>
    bind_rows()

  # If we have not gotten just a sample, give the relative day of the price
  # Note we are just using the first entry of requests, as it is the same for different
  # items if we have more
  if (!grepl("sample", requests[[1]]$path)) {
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
