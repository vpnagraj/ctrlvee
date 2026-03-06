#' Fetch R code chunks from any URL
#'
#' The primary engine of the ctrlvee package. Give it a URL
#' to a raw source file, a GitHub Gist, or a rendered
#' HTML page and it returns the R code chunks found there.
#'
#' @param url Character vector of length 1 with the URL to fetch
#' @param strategy One of `"auto"` (default), `"raw"`,  or
#'   `"html"`. When `"auto"`, the strategy is detected from the URL
#'   pattern using [detect_strategy()].
#' @param verbose Logical; default is `TRUE` and progress messages are printed
#'
#' @return A character vector of R code chunk bodies. Returns
#'   `character(0)` if no R chunks are found.
#'
#'
#' @examples
#' \dontrun{
#' ## rendered Quarto book chapter (HTML strategy detected)
#' chunks <- crawl_chunks("https://r4ds.hadley.nz/data-visualize.html")
#'
#'
#' ## GitHub Gist (plain .R file) (raw strategy detected)
#' chunks <- crawl_chunks(
#'   "https://gist.github.com/vpnagraj/59fa609c5adf47c8c7a5b156eb261be7"
#' )
#'
#' ## you can also dictate a specific strategy
#' chunks <- crawl_chunks("https://example.com/tutorial", strategy = "html")
#' }
#'
#' @export
crawl_chunks <- function(url, strategy = c("auto", "raw", "html"),
                         verbose = TRUE) {

  ## match and validate the strategy and URL args
  strategy <- match.arg(strategy)
  stopifnot(is.character(url), length(url) == 1)

  ## normalize known URL patterns (e.g. gist HTML -> gist raw)
  original_url <- url
  url <- normalize_url(url)
  if (verbose && !identical(url, original_url)) {
    message("[ctrlvee] Normalized URL: ", url)
  }

  ## optionally detect strategy automatically
  if (strategy == "auto") {
    strategy <- detect_strategy(url)
    if (verbose) message("[ctrlvee] Auto-detected strategy: ", toupper(strategy))
  }

  ## handle the crawling by strategy
  chunks <- switch(strategy,
    raw    = crawl_chunks_raw(url, verbose = verbose),
    html   = crawl_chunks_html(url, verbose = verbose),
    stop("Unknown strategy: ", strategy, call. = FALSE)
  )

  chunks
}