#' Validate that a string looks like a URL
#'
#' Cursory check that the input starts with `http://` or
#' `https://` and contains at least one dot in the domain portion.
#'
#' @param url Character vector of length 1 with the URL
#' @return Logical `TRUE` or `FALSE` as to whether the string passes the basic pattern
#' @keywords internal
is_valid_url <- function(url) {
  if (is.null(url) || length(url) == 0 || is.na(url)) {
    return(FALSE)
  }
  grepl("^https?://[^\\s/$.?#].[^\\s]*$", url, perl = TRUE)
}


#' Collapse a character vector of code chunks into a single insertable string
#'
#' Each chunk is separated by a comment banner so the user can visually
#' distinguish chunk boundaries once inserted into the editor.
#'
#' @param chunks Character vector where each element is one extracted code chunk body
#' @return A single character string ready for insertion with annotations for chunk boundaries.
#' @keywords internal
collapse_chunks <- function(chunks) {
  separator <- paste0(
    "\n\n",
    "# ---- chunk boundary ----",
    "\n\n"
  )
  paste(chunks, collapse = separator)
}


#' Minimal HTML escaping for IDE dialog messages
#'
#' Helper function to escape HTML in RStudio [rstudioapi::showDialog()] dialog rendering.
#'
#' @param x A character vector of length 1
#' @return The same vector but with `&`, `<`, and `>` escaped.
#' @keywords internal
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}


#' Make an HTTP GET request with standard headers and timeout
#'
#' Helper wrapper for `httr2` boilerplate so each strategy doesn't have to repeat it.
#' Returns the response object or stops with a descriptive error message.
#'
#' @param url Character vector of length 1 with the URL to fetch
#' @param accept Optional Accept header value (e.g., `"application/vnd.github.v3+json"`)
#' @param token Optional Authorization token (e.g., a GitHub PAT)
#' @return An `httr2` response object.
#' @keywords internal
#' @importFrom httr2 request req_timeout req_user_agent req_headers req_perform resp_status
http_get <- function(url, accept = NULL, token = NULL) {
  
  ## start composing the request
  req <- 
    httr2::request(url) %>%
    httr2::req_timeout(seconds = 15) %>%
    httr2::req_user_agent("crawl_chunks agent")

  ## optionally add headers and token
  hdrs <- list()
  if (!is.null(accept)) hdrs[["Accept"]] <- accept
  if (!is.null(token)) hdrs[["Authorization"]] <- paste("token", token)
  if (length(hdrs) > 0) {
    req <- do.call(httr2::req_headers, c(list(.req = req), hdrs))
  }

  ## issue request 
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(
        "HTTP request failed.\n",
        "  URL:   ", url, "\n",
        "  Error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop("Server returned HTTP ", status, " for URL: ", url, call. = FALSE)
  }
  resp
}


#' Normalize a URL before strategy dispatch
#'
#' Converts known URL patterns to their most fetchable form. Currently
#' handles GitHub Gist URLs, converting the HTML view to the raw
#' content endpoint. Non-gist URLs pass through unchanged.
#'
#' @param url Character vector of length 1 with the URL
#' @return URL either as-is or normalized per the logic.
#' @keywords internal
normalize_url <- function(url) {

  ## if a raw gist url ... return as-is
  if (grepl("gist\\.githubusercontent\\.com", url, ignore.case = TRUE)) {
    return(url)
  }

  ## if gist.github.com/<user>/<hash> pattern ...
  ## ... then convert to raw content URL to make it easier to parse
  ## NOTE: the /raw suffix returns the latest revision of the first file as text
  ## regex looks heavy ...
  gist_pattern <- "^https?://gist\\.github\\.com/([^/]+)/([0-9a-fA-F]+).*$"
  if (grepl(gist_pattern, url, perl = TRUE)) {
    ## combo of regmatches and regexec will look for indices and then extract matches
    ## NOTE: in the future relace with stringr for clarity?
    m <- regmatches(url, regexec(gist_pattern, url, perl = TRUE))[[1]]
    user <- m[2]
    hash <- m[3]
    return(sprintf("https://gist.githubusercontent.com/%s/%s/raw", user, hash))
  }

  ## if not a gist return as-is
  url
}


## infix operator for NULL-default (avoids depending on rlang for %||%).
`%||%` <- function(x, y) if (is.null(x)) y else x