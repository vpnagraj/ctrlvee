#' Detect the best extraction strategy for a URL
#'
#' Examines the URL pattern and returns a strategy string that tells the
#' caller which extraction function to use. This will be one of either: 
#' `"raw"` (URL points directly to a raw source file or a GitHub Gist) or
#' `"html"` (URL is a rendered web page like a Quarto book chapter or pkgdown site).
#'
#' @param url Character vector of length 1 with the URL to fetch
#' @return One of `"raw"`, `"github"`, or `"html"`.
#'
#' @details
#' Detection is entirely URL-based (no network requests) and intentionally
#' conservative; when in doubt it falls back to `"html"`.
#'
#' **Rules (evaluated in order):**
#' 1. Contains `gist.githubusercontent.com` or matches
#'    `gist.github.com/<user>/<hash>`: `"raw"`
#' 2. Contains `raw.githubusercontent.com` or ends in `.Rmd`, `.qmd`,
#'    `.md`, `.R`, or `.Rmarkdown`: `"raw"`
#' 3. Everything else: `"html"`
#'
#' @examples
#' detect_strategy("https://gist.github.com/vpnagraj/59fa609c5adf47c8c7a5b156eb261be7")
#'
#' detect_strategy("https://raw.githubusercontent.com/hadley/r4ds/main/data-visualize.qmd")
#'
#' detect_strategy("https://r4ds.hadley.nz/data-visualize.html")
#'
#' @export
detect_strategy <- function(url) {

  ## raw gist content is always plain text
  if (grepl("gist\\.githubusercontent\\.com", url, ignore.case = TRUE)) {
    return("raw")
  }

  ## gist urls with https get normalized to raw with normalize_url ... consider raw
  if (grepl("^https?://gist\\.github\\.com/", url, perl = TRUE)) {
    return("raw")
  }

  ## raw source files
  if (grepl("raw\\.githubusercontent\\.com", url, ignore.case = TRUE)) {
    return("raw")
  }
  
  ## files ending in common source extensions (regex is case-insensitive)
  if (grepl("\\.(qmd|rmd|rmarkdown|md|[rR])([?#].*)?$", url, perl = TRUE)) {
    return("raw")
  }

  ## if none of the above ... consider html
  "html"
}