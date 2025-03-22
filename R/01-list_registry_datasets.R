#'
#' List all IATI Registry datasets
#' 
#' @param base_url The IATI API base URL. This is currently set to
#'   https://iatiregistry.org/api/action.
#' @param limit The number of datasets to list per page. Only one page will be
#'   returned at a time.
#' @param offset The offset to start returning packages from.
#' 
#' @returns A data.frame listing the IATI Registry datasets
#' 
#' @examples
#' iati_list_datasets()
#' 
#' @export
#' 

iati_list_datasets <- function(base_url = "https://iatiregistry.org/api/action",
                               limit = NULL,
                               offset = NULL) {
  req <- httr2::request(base_url = base_url)

  req <- req |>
    httr2::req_url_path_append("package_list") |>
    httr2::req_url_query(limit = limit, offset = offset, .multi = "explode")

  req <- req |>
    httr2::req_auth_bearer_token(token = Sys.getenv("IATI_TOKEN"))

  resp <- req |>
    httr2::req_perform()

  output <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    do.call(cbind, args = _) |>
    as.data.frame()
    
  output
}