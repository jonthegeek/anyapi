#' Search for APIS
#'
#' Search APIs from APIs.guru by title, service, and provider.
#'
#' @param pattern A string to search for. This string is converted to lowercase
#'   before comparison.
#' @param refresh Whether to force a refresh of the APIs.guru API list.
#'
#' @return A tibble with columns `title`, `provider`, `service`, and `apid_url`.
#' @export
#'
#' @examplesIf interactive()
#' find_api("events")
#' find_api("fec.gov")
find_api <- function(pattern, refresh = FALSE) {
  pattern <- tolower(nectar::stabilize_string(pattern))
  apis <- .fetch_apis(refresh)
  apis[.api_has_pattern(apis, pattern), ]
}

.api_has_pattern <- function(apis, pattern) {
  .str_detect_lower(apis$title, pattern) |
    .str_detect_lower(apis$service, pattern) |
    .str_detect_lower(apis$provider, pattern)
}

.fetch_apis <- function(refresh = FALSE, call = rlang::caller_env()) {
  refresh <- .stabilize_lgl_scalar(refresh, call = call)
  if (refresh) {
    memoise::forget(.fetch_apis_impl)
  }
  return(.fetch_apis_impl())
}

.fetch_apis_impl <- function() {
  apis <- tidyr::unnest_longer(.list_apis(), "versions")
  apis <- .filter_preferred(apis)
  apis <- .unnest_versions(apis)
  apis <- .separate_provider(apis)
  .select_columns(apis)
}

.list_apis <- function() {
  apisguru::list_apis()
}

.filter_preferred <- function(apis) {
  apis[apis$preferred == apis$versions$version, ]
}

.unnest_versions <- function(apis) {
  tidyr::hoist(
    tidyr::unnest_wider(apis, "versions", names_sep = "_"),
    "versions_info",
    title = list(1, "title")
  )
}

.separate_provider <- function(apis) {
  tidyr::separate_wider_delim(
    apis,
    "name",
    ":",
    names = c("provider", "service"),
    too_few = "align_start"
  )
}

.select_columns <- function(apis) {
  tibble::tibble(
    title = apis$title,
    provider = apis$provider,
    service = apis$service,
    apid_url = apis$versions_swaggerUrl
  )
}

.str_detect_lower <- function(x, pattern) {
  grepl(pattern, tolower(x))
}

.stabilize_lgl_scalar <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  stbl::stabilize_lgl_scalar(
    x,
    allow_null = FALSE,
    allow_zero_length = FALSE,
    allow_na = FALSE,
    x_arg = arg,
    call = call
  )
}
