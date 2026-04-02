#' Fetch and extract R code blocks from a rendered HTML page
#'
#' Downloads an HTML page (e.g. a rendered Quarto book chapter at
#' `r4ds.hadley.nz`) and extracts code blocks that are tagged as R.
#'
#' @param url Character vector of length 1 with the URL to fetch
#' @param verbose Logical; default is `TRUE` and progress messages are printed
#'
#' @return A character vector where each element is the text content of
#'   one R code block, in document order. Returns `character(0)` if
#'   none are found.
#'
#' @details
#' ## What gets extracted
#'
#' The function looks for `<code>` elements whose `class` attribute
#' matches known patterns for R source code (e.g. `sourceCode r`,
#' `language-r`). It then grabs the inner text, stripping any
#' syntax-highlighting `<span>` elements.
#'
#' ## Limitations
#'
#' * Chunk options (labels, `echo`, `eval`, etc.) are lost in rendered HTML
#'   because the renderer strips them.
#' * Output blocks that the renderer styled identically to source blocks
#'   may occasionally be captured. A heuristic filters out blocks where
#'   the majority of lines look like R console output (`[1]`, `##`).
#'
#'
#' @examples
#' ## extract R code from a rendered Quarto book chapter (check license first!)
#' chunks <- crawl_chunks_html("https://r4ds.hadley.nz/data-visualize.html")
#' length(chunks)
#' cat(chunks[[1]])
#'
#' @export
#' @importFrom httr2 resp_body_string
#' @importFrom xml2 read_html xml_find_all xml_text xml_attr xml_parent
crawl_chunks_html <- function(url, verbose = interactive()) {

  stopifnot(is.character(url), length(url) == 1)
  if (!is_valid_url(url)) {
    stop("Invalid URL: ", url, call. = FALSE)
  }
  if (verbose) message("[ctrlvee] Strategy: HTML PARSING")
  if (verbose) message("[ctrlvee] Fetching: ", url)

  resp <- http_get(url)
  html_text <- httr2::resp_body_string(resp)

  if (verbose) message("[ctrlvee] Downloaded ", nchar(html_text), " characters of HTML.")

  chunks <- extract_r_chunks_from_html(html_text)

  if (verbose) message("[ctrlvee] Found ", length(chunks), " R code block(s).")
  chunks
}


#' Extract R code blocks from an HTML string
#'
#' The HTML-parsing workhorse used by [crawl_chunks_html()]. Also
#' exported so users can call it on local HTML strings if needed.
#'
#' @param html A character vector of length 1 containing HTML content
#' @return Character vector of extracted R code bodies.
#' @export
#' @importFrom xml2 read_html xml_find_all xml_text xml_attr xml_parent
#' @importFrom purrr map_chr map_lgl
extract_r_chunks_from_html <- function(html) {

  doc <- xml2::read_html(html)

  ## xpath: find <code> elements whose class (or parent <pre> class)
  ## indicates r source code. we pad the class with spaces and use
  ## contains() to safely match class tokens without substring collisions.
  xpath <- paste0(
    "//pre/code[",
    "  contains(concat(' ', normalize-space(@class), ' '), ' sourceCode r ') or ",
    "  contains(concat(' ', normalize-space(@class), ' '), ' sourceCode R ') or ",
    "  contains(concat(' ', normalize-space(@class), ' '), ' language-r ') or ",
    "  contains(concat(' ', normalize-space(@class), ' '), ' language-R ') or ",
    "  contains(concat(' ', normalize-space(@class), ' '), ' lang-r ') or ",
    "  contains(concat(' ', normalize-space(@class), ' '), ' r ') or ",
    "  normalize-space(../@class) = 'r' or ",
    "  contains(concat(' ', normalize-space(../@class), ' '), ' sourceCode r ') or ",
    "  contains(concat(' ', normalize-space(../@class), ' '), ' language-r ') or ",
    "  contains(concat(' ', normalize-space(../@class), ' '), ' r ')",
    "]"
  )

  code_nodes <- xml2::xml_find_all(doc, xpath)

  ## fallback: some quarto builds use class="cell-code" on the wrapper
  ## and class="sourceCode r" on the <pre> rather than <code>.
  if (length(code_nodes) == 0) {
    xpath_broad <- paste0(
      "//pre[",
      "  contains(concat(' ', normalize-space(@class), ' '), ' sourceCode r ') or ",
      "  contains(concat(' ', normalize-space(@class), ' '), ' sourceCode R ')",
      "]/code"
    )
    code_nodes <- xml2::xml_find_all(doc, xpath_broad)
  }

  if (length(code_nodes) == 0) return(character(0))

  ## extract the text content of each matched <code> node
  raw_chunks <- purrr::map_chr(code_nodes, xml2::xml_text)

  ## heuristic: skip blocks that look like r console output.
  ## output blocks often start with "[1]" or "##". skip any block where
  ## the majority of non-empty lines match that pattern.
  is_output <- purrr::map_lgl(raw_chunks, function(chunk) {
    lines <- strsplit(chunk, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) return(TRUE)
    output_lines <- grepl("^(\\s*\\[\\d+\\]|\\s*##)", lines)
    mean(output_lines) > 0.5
  })

  chunks <- raw_chunks[!is_output]

  ## trim trailing whitespace and drop any empty chunks
  chunks <- trimws(chunks, which = "right")
  chunks <- chunks[nzchar(chunks)]

  chunks
}