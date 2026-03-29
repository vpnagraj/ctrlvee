#' RStudio / Positron Addin: Crawl R Chunks from a URL
#'
#' When invoked from the Addins menu or a keyboard shortcut, this function:
#'
#' 1. Opens a dialog asking the user for a web URL.
#' 2. Auto-detects the best extraction strategy.
#' 3. Fetches the page and extracts R code chunks.
#' 4. Inserts the extracted code into the active source editor at the
#'    current cursor position.
#'
#' @section Keyboard shortcut:
#' After installing the package, open
#' **Tools > Modify Keyboard Shortcuts…** (RStudio) or the Command Palette
#' in Positron, search for *"Extract External R Code and Insert Inline"*, and assign your
#' preferred shortcut (e.g. `Ctrl+Shift+U` / `Cmd+Shift+U`).
#'
#' @return Called for side effects (i.e., inserting text); returns `NULL` invisibly.
#' @export
#' @importFrom rstudioapi showPrompt insertText showDialog isAvailable getActiveDocumentContext
addin_crawl_chunks <- function() {

  ## first check the IDE availability
  if (!rstudioapi::isAvailable()) {
    stop(
      "This addin requires RStudio (>= 1.1) or Positron. ",
      call. = FALSE
    )
  }

  ## pop open the prompt for url
  url <- rstudioapi::showPrompt(
    title   = "ctrlvee: Extract R Code From URL",
    message = paste0(
      "Enter a URL to page that contains R code (e.g., raw source or rendered HTML)"
    ),
    default = ""
  )

  if (is.null(url) || !nzchar(trimws(url))) return(invisible(NULL))
  url <- trimws(url)

  ## validate url input
  if (!is_valid_url(url)) {
    rstudioapi::showDialog(
      title   = "Invalid URL",
      message = paste0(
        "The text you entered does not look like a valid URL:<br>",
        "<code>", html_escape(url), "</code><br><br>",
        "Please include <code>http://</code> or <code>https://</code>."
      )
    )
    return(invisible(NULL))
  }

  ## detect strategy
  url <- normalize_url(url)
  strategy <- detect_strategy(url)
  strategy_label <- switch(strategy,
    raw    = "Raw source / plain R file",
    github = "GitHub repo (API)",
    html   = "Rendered HTML page"
  )
  message("[ctrlvee] Strategy: ", strategy_label)

  ## etch and extract
  chunks <- tryCatch(
    crawl_chunks(url, strategy = strategy, verbose = TRUE),
    error = function(e) {
      rstudioapi::showDialog(
        title   = "Fetch Error",
        message = paste0(
          "Could not retrieve R chunks.<br><br>",
          "<b>Strategy:</b> ", strategy_label, "<br>",
          "<b>Error:</b> ", html_escape(conditionMessage(e)),
          "<br><br>",
          "<b>Tip:</b> Try forcing a different strategy in the console:<br>",
          "<code>crawl_chunks('", html_escape(url),
          "', strategy = 'html')</code>"
        )
      )
      return(NULL)
    }
  )

  if (is.null(chunks)) return(invisible(NULL))

  ## handle case where no chunks are found
  if (length(chunks) == 0) {
    rstudioapi::showDialog(
      title   = "No R Chunks Found",
      message = paste0(
        "The page was fetched successfully (strategy: <b>", strategy_label,
        "</b>) but no R code chunks were found.<br><br>",
        "<b>Suggestions:</b><br>",
        "&#8226; For rendered HTML: make sure the page has visible R ",
        "code blocks.<br>",
        "&#8226; For raw files: use the GitHub <em>Raw</em> button URL.<br>"
      )
    )
    return(invisible(NULL))
  }

  ## build insertion text with provenance
  header <- paste0(
    "# -----------------------------------------------------------------\n",
    "# Chunks fetched by ctrlvee from: ", url, "\n",
    "# Strategy: ", strategy_label, "\n",
    "# Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Chunks: ", length(chunks), "\n",
    "# NOTE: Check the source license before reusing this code.\n",
    "# -----------------------------------------------------------------\n\n"
  )

  body <- collapse_chunks(chunks)
  insert_text <- paste0(header, body, "\n")

  ## insert text at cursor
  cursor <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range
  rstudioapi::insertText(location = cursor, text = insert_text)
  message("[ctrlvee] Inserted ", length(chunks), " chunk(s) into the editor.")

  invisible(NULL)
}