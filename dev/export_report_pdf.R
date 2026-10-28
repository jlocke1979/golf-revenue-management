#!/usr/bin/env Rscript

# Render the Dobson Ranch reading copy to one PDF.
#
#   Rscript dev/export_report_pdf.R
#
# The file to grab is:
#   outputs/dobson-ranch-report.pdf
#
# The name is the course, then "report". It stays the same when a chapter
# is added or the order below changes. Add the sample's end date only when
# an older run has to be kept.

# Reading order. Append a row to include another vignette.
# Move a row when the order changes. The introduction is added in front.
chapters <- data.frame(
  path = c(
    "vignettes/dobson-ranch.Rmd",
    "vignettes/correlations-and-relationships.Rmd",
    "vignettes/revenue-and-golfers.Rmd",
    "vignettes/dollars-per-golfer.Rmd",
    "vignettes/operational-opportunities.Rmd"
  ),
  title = c(
    "Capacity and utilization",
    "Correlations and relationships",
    "Revenue and golfers",
    "Dollars per golfer",
    "Operational Opportunities and Next Steps"
  ),
  blurb = c(
    "Daylight share and golfers per day, by month, weekday, and season.",
    "Each day's golfers against each revenue type, one year at a time.",
    "Annual golfers, recorded cart rentals, and revenue by type.",
    "Recorded revenue divided by the golfers who played.",
    "Five questions to investigate, and the records that would test them."
  ),
  stringsAsFactors = FALSE
)

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    return(normalizePath(getwd()))
  }
  normalizePath(sub("^--file=", "", file_arg[[1]]))
}

root <- normalizePath(file.path(dirname(script_path()), ".."))
setwd(root)

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Install rmarkdown before exporting the report.", call. = FALSE)
}

chrome <- Sys.getenv(
  "CHROME",
  unset = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
)
if (!file.exists(chrome)) {
  stop(
    "Chrome is required to print the report. Set CHROME to the browser binary.",
    call. = FALSE
  )
}

missing <- chapters$path[!file.exists(file.path(root, chapters$path))]
if (length(missing) > 0) {
  stop(
    paste0("Missing vignette: ", paste(missing, collapse = ", ")),
    call. = FALSE
  )
}

lib <- file.path(tempdir(), "golfops-export-lib")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
message("Installing golfops into a temporary library")
install.packages(root, repos = NULL, type = "source", lib = lib, quiet = TRUE)
.libPaths(c(lib, .libPaths()))

html_dir <- file.path(tempdir(), "golfops-export")
dir.create(html_dir, recursive = TRUE, showWarnings = FALSE)
css <- file.path(root, "dev", "report-export.css")

render_chapter <- function(chapter, params = NULL) {
  message("Rendering ", chapter)
  knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
  html_name <- paste0(tools::file_path_sans_ext(basename(chapter)), ".html")
  render_args <- list(
    input = file.path(root, chapter),
    output_format = rmarkdown::html_vignette(
      css = css,
      self_contained = TRUE
    ),
    output_file = html_name,
    output_dir = html_dir,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  if (!is.null(params)) {
    render_args$params <- params
  }
  do.call(rmarkdown::render, render_args)
  html_path <- file.path(html_dir, html_name)
  pdf_path <- file.path(html_dir, paste0(tools::file_path_sans_ext(html_name), ".pdf"))
  status <- system2(
    chrome,
    c(
      "--headless=new",
      "--disable-gpu",
      "--no-pdf-header-footer",
      "--window-size=1100,1600",
      paste0("--print-to-pdf=", pdf_path),
      paste0("file://", html_path)
    )
  )
  if (!identical(status, 0L) || !file.exists(pdf_path) || file.info(pdf_path)$size == 0) {
    stop(paste0("Chrome did not write a PDF for ", chapter), call. = FALSE)
  }
  pdf_path
}

chapter_pdfs <- vapply(chapters$path, render_chapter, character(1))

count_pages <- function(path) {
  script <- file.path(tempdir(), "count_pages.swift")
  writeLines(
    c(
      "import Foundation",
      "import PDFKit",
      "let doc = PDFDocument(url: URL(fileURLWithPath: CommandLine.arguments[1]))!",
      "print(doc.pageCount)"
    ),
    script
  )
  out <- system2("swift", c(script, path), stdout = TRUE, stderr = FALSE)
  as.integer(out[[length(out)]])
}

chapter_pages <- vapply(chapter_pdfs, count_pages, integer(1))
if (any(!is.finite(chapter_pages)) || any(chapter_pages < 1)) {
  stop("A chapter PDF has no pages.", call. = FALSE)
}

sample <- dobson_operating_daily()
front_pages <- 1L
front_pdf <- character(1)
toc_pages <- integer(nrow(chapters))
for (attempt in 1:3) {
  toc_pages <- front_pages + cumsum(c(1L, chapter_pages[-length(chapter_pages)]))
  rows <- paste0(
    "<tr><td><strong>", seq_len(nrow(chapters)), ". ", chapters$title,
    "</strong><br><span>", chapters$blurb, "</span></td>",
    '<td class="where">', toc_pages, "</td></tr>"
  )
  front_pdf <- render_chapter(
    "dev/report-front.Rmd",
    params = list(
      start_date = format(min(sample$date), "%Y-%m-%d"),
      end_date = format(max(sample$date), "%Y-%m-%d"),
      toc_html = paste0("<table class=\"contents\">", paste(rows, collapse = ""), "</table>")
    )
  )
  n_front <- count_pages(front_pdf)
  if (identical(n_front, front_pages)) {
    break
  }
  front_pages <- n_front
  if (attempt == 3) {
    stop("The introduction did not settle on a page count.", call. = FALSE)
  }
}

merge_swift <- file.path(tempdir(), "merge_report.swift")
outline_labels <- c("Introduction", chapters$title)
outline_starts <- c(0L, toc_pages - 1L)
writeLines(
  c(
    "import Foundation",
    "import PDFKit",
    "let args = CommandLine.arguments.dropFirst()",
    "guard args.count >= 2 else { fputs(\"usage: merge inputs... dest\\n\", stderr); exit(1) }",
    "let dest = URL(fileURLWithPath: String(args.last!))",
    "let merged = PDFDocument()",
    "var at = 0",
    "for path in args.dropLast() {",
    "  guard let doc = PDFDocument(url: URL(fileURLWithPath: String(path))) else {",
    "    fputs(\"unreadable pdf\\n\", stderr); exit(1)",
    "  }",
    "  for i in 0..<doc.pageCount {",
    "    if let page = doc.page(at: i) {",
    "      merged.insert(page, at: at)",
    "      at += 1",
    "    }",
    "  }",
    "}",
    "let root = PDFOutline()",
    paste0("let labels = [", paste(sprintf("\"%s\"", outline_labels), collapse = ", "), "]"),
    paste0("let starts = [", paste(outline_starts, collapse = ", "), "]"),
    "for index in labels.indices {",
    "  let item = PDFOutline()",
    "  item.label = labels[index]",
    "  if let page = merged.page(at: starts[index]) {",
    "    let top = page.bounds(for: .mediaBox).height",
    "    item.destination = PDFDestination(page: page, at: NSPoint(x: 0, y: top))",
    "  }",
    "  root.insertChild(item, at: index)",
    "}",
    "merged.outlineRoot = root",
    "if !merged.write(to: dest) { fputs(\"could not write pdf\\n\", stderr); exit(1) }",
    "print(at)"
  ),
  merge_swift
)

out_dir <- file.path(root, "outputs")
dir.create(out_dir, showWarnings = FALSE)
pdf_path <- file.path(out_dir, "dobson-ranch-report.pdf")
legacy <- file.path(out_dir, "dobson-ranch-capacity-and-utilization.pdf")
message("Combining the introduction and ", length(chapter_pdfs), " chapters")
status <- system2("swift", c(merge_swift, front_pdf, chapter_pdfs, pdf_path))
if (!identical(status, 0L) || !file.exists(pdf_path) || file.info(pdf_path)$size == 0) {
  stop("The chapter PDFs were not combined.", call. = FALSE)
}
if (file.exists(legacy)) {
  unlink(legacy)
}

message(
  "Wrote ",
  pdf_path,
  " (",
  format(round(file.info(pdf_path)$size / 1e6, 1), nsmall = 1),
  " MB)"
)
