% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_item_data.R
\name{create_item_data}
\alias{create_item_data}
\title{Create a tibble of information with prices of requested item}
\usage{
create_item_data(items, history = "all", user_agent = "GE_price_tracker")
}
\arguments{
\item{items}{\code{character} vector of the items}

\item{history}{\code{character} with type of request. Default is \code{latest}, but \code{all} is
also available}

\item{user_agent}{\code{character} of the user agent}
}
\value{
A tibble with information about items, prices, dates
}
\description{
The function is a wrapper of \code{\link[=osrs_api]{osrs_api()}} that returns a tibble which is easier to
work with than the list object from the \code{osrs_api} function.
}
\examples{
# Create a data set with information about the item
create_item_data(items = "Abyssal_whip", history = "all")

# Same can be done for other values of history
create_item_data(items = c("bucket", "Abyssal whip"), history = "latest") |>
create_item_data(items = c("Dragon dagger"), history = "sample")

}
