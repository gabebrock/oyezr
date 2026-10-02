#' Fetch Supreme Court Docket Details from the Oyez API
#'
#' @param term Integer. The court term year (e.g., 2022).
#' @param docket_number Character. The case docket number (e.g., "20-1199").
#'
#' @return A `tibble::tibble` containing case metadata
#' @export
#'
#' @examples
#' sffa <- sc_case(2022, "20-1199")

sc_case <- function(term, docket_number) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # parameter validation
  if (missing(term) || is.null(term)) {
    cli::cli_abort('Argument {.arg term} must be specified.')
  } else if (!is.numeric(term) ||
             term < 1850 ||
             term > current_year) {
    cli::cli_abort(
      'Argument {.arg term} must be a 4 digit year between 1850 and {current_year}.'
    )
  } else if (!grepl("^\\d{2}-\\d{4}$", docket_number)) {
    cli::cli_abort(
      'Argument {.docket_number} must be two-digits, followed be a dash and
      four more digits.'
    )
  }

  # format inputs
  term_str <- as.character(term) # prep year as string for http query
  docket_str  <- as.character(docket_number)

  # build URL and HTTP request
  url <- sprintf("https://api.oyez.org/cases/%s/%s", term_str, docket_str)

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
    httr2::req_error(is_error = function(resp) FALSE)

  # perform request
  res <- httr2::req_perform(req)

  # check status code
  if (httr2::resp_status(res) != 200) {
    warning(sprintf("Failed to fetch case %s/%s. HTTP Status: %d", term_str, docket_str, resp_status(res)))
    return(tibble::tibble())
  }

  # parse JSON response
  data <- httr2::resp_body_json(res)

  # format JSON list into single-row tibble
  case_tbl <- purrr::map(data, function(x) {
    # if element is NULL, swap with NA
    if (is.null(x)) {
      NA
    # if element is single
    } else if (length(x) == 1) {
      x
    #
    } else {
      list(x) # wrap nested vectors in list-column
    }
  }) |>
    tibble::as_tibble()

  return(case_tbl)
}






# ID
# name
# first_party, first_party_label, second_party, second_party_label
# description, facts_of_the_case, question, conclusion
# timeline #need to build
# citation #need to build
# decided_by$name
#
# href
# justia_url
