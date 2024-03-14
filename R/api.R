library(httr)
library(xml2)

###################
# Using httr

osrs_api <- function(item = "Abyssal_whip", history = "latest", user_agent = "GE_price_tracker") {

  # Give valid values of history argument
  history_valid <- c("latest", "all")
  history <- match.arg(history, history_valid)

  # The the path needed
  item_path <- paste0("/exchange/history/osrs/", history, "?name=", item)

  # Create the full URL
  url <- modify_url("https://api.weirdgloop.org", path = item_path)

  # GET the information from the URL
  resp <- GET(url,
              httr::user_agent(user_agent))

  # Check if success
  if (http_status(resp)$category != "Success") {
    stop("Failed to retrieve data from the API")
  }

  # Check if the format is json
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Parse the json, creating a list
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  # If no success, stop and give the error message
  if (!parsed$success) {
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
      path = item_path,
      response = resp
    ),
    class = "osrs_api"
  )
}

print.osrs_api <- function(x, ...) {
  # "condense" the information about response a bit, otherwise the str call goes nuts
  x$response <- lapply(x$response, class)

  cat("<OSRS Weird Gloop API ", x$path, ">\n", sep = "")
  str(x)
}

test <- osrs_api(item = "bob", history = "latest")
