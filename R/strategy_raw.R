#' Fetch and extract R code from a raw source URL
#'
#' Downloads the raw text content of a URL (typically a `.qmd`, `.Rmd`,
#' `.md`, or `.R` file served as plain text) and extracts R code.
#'
#' For Markdown-family files, fenced R code blocks are extracted
#' individually. For plain `.R` files (or any file where no fenced
#' chunks are found), the entire file content is returned as a single
#' chunk.
#'
#' @param url Character vector of length 1 with the URL to fetch
#' @param verbose Logical; default is `TRUE` and progress messages are printed
#'
#' @return A character vector where each element is the body of one R
#'   chunk. For plain `.R` files, a vector of length 1 containing the
#'   full file. Returns `character(0)` if the file is empty.
#'
#' @examples
#' ## fenced chunks from a Quarto source file
#' chunks <- crawl_chunks_raw(
#'   "https://raw.githubusercontent.com/hadley/r4ds/main/data-visualize.qmd"
#' )
#'
#' ## plain .R file from a GitHub Gist
#' chunks <- crawl_chunks_raw(
#'   "https://gist.githubusercontent.com/vpnagraj/59fa609c5adf47c8c7a5b156eb261be7/raw"
#' )
#'
#' @export
#' @importFrom httr2 resp_body_string
crawl_chunks_raw <- function(url, verbose = TRUE) {

  ## check validity of url input
  stopifnot(is.character(url), length(url) == 1)
  if (!is_valid_url(url)) {
    stop("Invalid URL: ", url, call. = FALSE)
  }
  if (verbose) message("[ctrlvee] Strategy: RAW SOURCE")
  if (verbose) message("[ctrlvee] Fetching: ", url)

  resp <- http_get(url)
  body <- httr2::resp_body_string(resp)

  if (verbose) message("[ctrlvee] Downloaded ", nchar(body), " characters.")

  ## check if the body has non zero size with nzchar()
  body_trimmed <- trimws(body)
  ## if FALSE return an empty character vector
  if (!nzchar(body_trimmed)) {
    if (verbose) message("[ctrlvee] File is empty.")
    return(character(0))
  }

  ## try extracting fenced code chunks first
  chunks <- extract_r_chunks_from_markdown(body)

  ## if there any chunks ... count how many
  if (length(chunks) > 0) {
    if (verbose) message("[ctrlvee] Found ", length(chunks), " fenced R chunk(s).")
    return(chunks)
  }

  ## if there are no fenced chunks then return the whole file as one chunk
  if (verbose) {
    message("[ctrlvee] No fenced R chunks found.")
    message("[ctrlvee] Returning entire file as a single code chunk (1 chunk).")
  }

  return(body_trimmed)
}


#' Extract fenced R code chunks from a Markdown text string
#'
#' A helper to go line-by-line through a Markdown file to find the common R code fence
#' styles used in R Markdown, Quarto, and GitHub Markdown. 
#' Note that non-R fenced blocks (e.g., `python`, `sql`, etc.) are skipped.
#'
#' @param text Character vector of length 1 with Markdown source text
#' @return Character vector of extracted code bodies.
#' @export
extract_r_chunks_from_markdown <- function(text) {

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  ## regex for finding start of R chunks
  ## gnarly ... but basically looks for backticks and R or r
  open_pattern <- "^(`{3,})\\s*\\{\\s*[.]?[rR][^}]*\\}\\s*$|^(`{3,})\\s*[rR](?!\\w).*$"
  close_pattern <- "^`{3,}\\s*$"

  ## initialize some values for objects to handline to loop below
  chunks    <- character(0)
  in_chunk  <- FALSE
  current   <- character(0)
  fence_len <- 3L

  ## loop throuhg all lines and pull out code in between open / close patterns detected
  ## NOTE: uses nzchar to check for empty strings
  for (line in lines) {
    if (!in_chunk) {
      m <- regmatches(line, regexec(open_pattern, line, perl = TRUE))[[1]]
      if (length(m) > 0 && nzchar(m[1])) {
        in_chunk  <- TRUE
        ticks     <- if (nzchar(m[2])) m[2] else m[3]
        fence_len <- nchar(ticks)
        current   <- character(0)
      }
    } else {
      if (grepl(close_pattern, line, perl = TRUE)) {
        closing_ticks <- nchar(regmatches(line, regexpr("^`+", line)))
        if (closing_ticks >= fence_len) {
          chunks   <- c(chunks, paste(current, collapse = "\n"))
          in_chunk <- FALSE
          current  <- character(0)
          next
        }
      }
      current <- c(current, line)
    }
  }

  chunks
}