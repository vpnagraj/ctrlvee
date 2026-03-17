# ctrlvee

 <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->

Fetch R code from an external source and insert it directly in an editor. Built as an add-in for integrating with Positron and RStudio.


## Overview

### Motivation

You're reading a web book (or blog post, vignette, README, etc) and you see some R code you want to try out. How do you get that into your editor? Sure, you might just copy/paste. But that can be tedious and error-prone, especially for documents that have code split across multiple chunks.

Is there an easier way to go out and pull the code from an external source so you can run it on your computer?

Enter `ctrlvee` ...

### Solution

`ctrlvee` provides functionality to parse R code from a given URL that contains either rendered R chunks (e.g., Quarto book, RMarkdown vignette, GitHub README) or "raw" R code (e.g., Rmd/Qmd/Md, files with fenced chunks, R script in a Gist). Through the provided Positron/RStudio add-in, the parsed code will be inserted directly into the editor so you can run it on your system.

While scraping the contents from the URL, `ctrlvee` checks the source to automatically detect a strategy (i.e., HTML or raw).

## Setup

### Installation

You can install `ctrlvee` by cloning the repo and installing from source.

Alternatively, install from GitHub with a tool like `pak`:

```r
pak::pkg_install("vpnagraj/ctrlvee")
```

### Keyboard shortcuts

`ctrlvee` is written as an add-in for Positron and RStudio. For easisest access, consider configuring a keyboard shortcut:

- **RStudio:** Tools → Modify Keyboard Shortcuts → search "Extract External R Code"
- **Positron:** Command Palette → search "Extract External R Code"

## Usage

### Quick start

1. Install `ctrlvee`
2. Open any `.R` script in RStudio or Positron
3. Place your cursor where you want code inserted
4. **Addins → Extract External R Code and Insert Inline** (or your keyboard shortcut).
5. Paste a URL (e.g., rendered Quarto chapter like https://dstt.stephenturner.us/validation.html)
6. Code appears at your cursor with comments about provenance

### Alternatives

`ctrlvee` is recommended for use as an IDE add-in. But you can use the exported package functions programatically too.

For example:

```r
library(ctrlvee)

## crawl code chunks and auto-detect strategy from URL
crawl_chunks("https://dstt.stephenturner.us/validation.html")

## crawl code chunks and force a specific strategy
crawl_chunks("https://dstt.stephenturner.us/validation.html", strategy = "html")

## see the just the strategy detection in action
detect_strategy("https://dstt.stephenturner.us/validation.html")
```

## Limitations

### Development status

**NOTE**: The `ctrlvee` package is currently *experimental*. YMMV. With that said, please use the GitHub issue queue to report issues and/or suggest new features.

### Using code from external sources

- **Provenance**: If you're extracting external code, you should know where it comes from. While `ctrlvee` will include a provenance statement, you should look before you leap. Make sure you know understand where the code is coming from before running it.
- **Licenses**: Before reusing extracted content in your own work, always verify the source's license. Just because it is available, does not necessarily mean it is appropriate to be used for what you have in mind. Always read the license.
- **Dependencies**: It's common to find code that is distributed without explicitly including steps to install dependencies. Don't forget that step when you try to run the code on your system. And of course keep in mind that dependencies may no longer be available and/or versions might have changed since the code was published.
