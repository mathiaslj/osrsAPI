###################
# Using httr

#' Get information about Grand Exchange prices of items in OSRS
#'
#' @description
#' Use the Weird Gloop API to fetch information about Grand Exchange prices in
#' Oldschool Runescape
#'
#' @param items `character` vector of the items
#' @param history `character` with type of request. Default is "latest", but "all" is
#' also available
#' @param user_agent `character` of the user agent
#'
#' @return A list of class `osrs_api`
#'
#' @export
#'
#' @examples
#' # Get the latest prices of both a bucket and the abyssal whip
#' bucket_and_whip <- osrs_api(items = c("bucket", "Abyssal whip"), history = "latest")
#'
#' # Get the entire price history of the abyssal whip
#' whip <- osrs_api(items = "Abyssal_whip", history = "all")
#'
osrs_api <- function(items, history = "latest", user_agent = "GE_price_tracker") {

  # Give valid values of history argument
  history_valid <- c("latest", "all", "last90d", "sample")
  history <- match.arg(history, history_valid)

  # Give error message
  if (history != "latest") {
    if (length(items) > 1 || grepl("\\|", items)) {
      cli::cli_abort('Multiple items only supported for {.code history = "latest"}')
    }
  }

  # Ensuring that we use _ to seperate words and make the first letter capital
  items <- gsub("\\s", "_", trimws(items)) |>
    stringr::str_to_sentence() |>
    paste(collapse = "|")

  # The the path needed
  items_path <- paste0("/exchange/history/osrs/", history, "?name=", items)

  # Create the full URL
  url <- httr::modify_url("https://api.weirdgloop.org", path = items_path)

  # GET the information from the URL
  resp <- httr::GET(url,
                    httr::user_agent(user_agent))

  # Check if success
  if (httr::http_status(resp)$category != "Success") {
    stop("Failed to retrieve data from the API")
  }

  # Check if the format is json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Parse the json, creating a list
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  # If no success, stop and give the error message
  if (isFALSE(parsed$success)) {
    stop(parsed$error)
  }

  # Check if an error happened
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "OSRS API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # Make output a new class
  structure(
    list(
      content = parsed,
      path = items_path,
      response = resp
    ),
    class = "osrs_api"
  )
}

#' Print method for class
#'
#' @param x object of class `osrs_api`
#' @param ... additional arguments passed to methods
#'
#' @export
#'
#' @examples
#' whip <- osrs_api(items = "Abyssal_whip", history = "latest")
#' print(whip)
print.osrs_api <- function(x, ...) {
  # "condense" the information about response a bit, otherwise the str call goes nuts
  x$response <- lapply(x$response, class)

  cat("<OSRS Weird Gloop API ", x$path, ">\n", sep = "")
  utils::str(x)
  invisible(x)
}
