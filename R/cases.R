#' Fetch Supreme Court Cases from Oyez API
#'
#' @param term Integer or character. Supreme court term year (e.g., 2022, "2022)
#' @param per_page Integer. Number of results per page. Set to 0 to fetch all cases in term. Default is 0
#'
#' @return A tidy tibble of Supreme Court cases.
#' @export
#'
#' @examples
#' sc_cases(term = 2022)

sc_cases <- function(term = NULL, per_page = 0) {
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
  }

  # build http request
  url <- "https://api.oyez.org/cases" # set base url for case query
  term_str <- as.character(term) # prep year as string for http query

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64: x64") |>
    httr2::req_url_query(
      page = 0,
      per_page = per_page,
      filter = paste0("term:", term_str)
    )

  # perform request
  res <- httr2::req_perform(req)

    # check for empty payload response
    has_body <- httr2::resp_has_body(res)
    raw_txt  <- httr2::resp_body_string(res)

    if (!has_body) {
      cli::cli_abort("No cases found for term")
    }

  # parse JSON into df
  df_raw <- jsonlite::fromJSON(raw_txt, flatten = TRUE) |>
    tibble::as_tibble()

  if (nrow(df_raw) == 0) {
    return(tibble::as_tibble())
  }

  # helper for cleaning column names to snake_case
  clean_cols <- function(x) {
    out <- names(x) |>
      stringr::str_replace_all('\\.', '_') |>
      stringr::str_replace_all('([a-z])([A-Z])', '\\1_\\2') |>
      stringr::str_to_lower() |>
      stringr::str_remove_all('\\$')
    stats::setNames(x, nm = out)
  }

  # df transform pipeline
  df_clean <- df_raw |>
    # unnest timeline events (granted, argued, decided)
    tidyr::unnest(timeline, names_repair = "unique") |>
    # drop timeline endpoint URLs if present
    dplyr::select(-dplyr::any_of(c("href...8", "href.1"))) |>
    # convert Unix time stamps to Date objects
    dplyr::mutate(
      date = lubridate::as_date(
        lubridate::as_datetime(as.numeric(dates))
      )
    ) |>
    dplyr::select(-dates) |>
    # pivot timeline events wider (e.g. date_granted)
    tidyr::pivot_wider(
      names_from = event,
      values_from = date,
      names_prefix = "date_"
    ) |>
    # construct citations (e.g. 600 U.S. (2023))
    dplyr::mutate(
      citation = dplyr::case_when(
        !is.na(citation.volume) & !is.na(citation.year) ~
          paste0(citation.volume, " U.S. (", citation.year, ")"),
        TRUE ~ NA_character_
      )
    ) |>
    # Clean HTML tags from question field
    dplyr::mutate(
      question = stringr::str_remove_all(question, "<[^>]+>") |>
        stringr::str_replace_all("[\r\n]+", " ") |>
        stringr::str_squish()
    ) |>
    # drop raw citation components
    dplyr::select(-dplyr::any_of(c("citation.volume", "citation.page", "citation.year", "citation.href"))) |>
    # clean up timeline names into tidy snake_case to match other vars
    clean_cols()

  return(df_clean)
}






