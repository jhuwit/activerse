#!/usr/bin/env Rscript

options(useFancyQuotes = FALSE)

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 1) {
    return(normalizePath(sub("^--file=", "", file_arg)))
  }

  normalizePath(file.path(getwd(), "scripts", "update-activerse.R"))
}

root_dir <- normalizePath(file.path(dirname(get_script_path()), ".."))
desc_path <- file.path(root_dir, "DESCRIPTION")
r_path <- file.path(root_dir, "R", "activerse.R")
readme_rmd_path <- file.path(root_dir, "README.Rmd")
readme_md_path <- file.path(root_dir, "README.md")

args <- commandArgs(trailingOnly = TRUE)
owner <- "jhuwit"
new_packages <- character()

for (arg in args) {
  if (startsWith(arg, "--owner=")) {
    owner <- sub("^--owner=", "", arg)
    next
  }

  if (arg == "--dry-run") {
    next
  }

  if (nzchar(arg)) {
    new_packages <- c(new_packages, arg)
  }
}

trim_ws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

parse_dcf_packages <- function(field) {
  if (is.null(field) || length(field) == 0 || is.na(field) || !nzchar(field)) {
    return(character())
  }

  entries <- unlist(strsplit(field, ",", fixed = TRUE), use.names = FALSE)
  entries <- trim_ws(entries)
  entries <- sub("\\s*\\(.*\\)$", "", entries)
  entries[nzchar(entries)]
}

parse_dcf_remotes <- function(field) {
  if (is.null(field) || length(field) == 0 || is.na(field) || !nzchar(field)) {
    return(character())
  }

  entries <- unlist(strsplit(field, ",", fixed = TRUE), use.names = FALSE)
  entries <- trim_ws(entries)
  entries[nzchar(entries)]
}

format_dcf_values <- function(values, indent = "    ") {
  if (!length(values)) {
    return(character())
  }

  if (length(values) == 1) {
    return(paste0(indent, values))
  }

  c(
    paste0(indent, values[-length(values)], ","),
    paste0(indent, values[length(values)])
  )
}

replace_dcf_field <- function(lines, field_name, values) {
  start <- grep(paste0("^", field_name, ":"), lines)
  if (length(start) != 1) {
    stop("Could not find DESCRIPTION field: ", field_name, call. = FALSE)
  }

  end <- start
  while (end < length(lines) && grepl("^[[:space:]]", lines[end + 1])) {
    end <- end + 1
  }

  replacement <- c(
    paste0(field_name, ":"),
    format_dcf_values(values)
  )

  suffix <- if (end < length(lines)) lines[(end + 1):length(lines)] else character()
  c(lines[seq_len(start - 1)], replacement, suffix)
}

oxford_list <- function(x) {
  if (!length(x)) {
    return("")
  }
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }

  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

build_intro_block <- function(packages, remotes) {
  remote_by_package <- setNames(remotes, sub("^.*/", "", remotes))
  repos <- unname(remote_by_package[packages])
  repos[is.na(repos) | !nzchar(repos)] <- packages[is.na(repos) | !nzchar(repos)]
  links <- sprintf("[%s](https://github.com/%s)", paste0("`", packages, "`"), repos)

  c(
    "<!-- activerse-intro:start -->",
    paste(
      "`activerse` is a lightweight helper package that attaches",
      oxford_list(links),
      "together."
    ),
    "<!-- activerse-intro:end -->"
  )
}

build_table_block <- function(packages, remotes) {
  remote_by_package <- setNames(remotes, sub("^.*/", "", remotes))
  repos <- unname(remote_by_package[packages])
  repos[is.na(repos) | !nzchar(repos)] <- packages[is.na(repos) | !nzchar(repos)]

  rows <- vapply(seq_along(packages), function(i) {
    repo <- repos[[i]]
    pkg <- packages[[i]]
    paste0(
      "| `", pkg, "` | [", repo, "](https://github.com/", repo, ") | ",
      "[![R CMD check](https://github.com/", repo, "/actions/workflows/R-CMD-check.yaml/badge.svg)](",
      "https://github.com/", repo, "/actions/workflows/R-CMD-check.yaml) |"
    )
  }, character(1))

  c(
    "<!-- activerse-packages:start -->",
    "| Package | Repository | R CMD check |",
    "| --- | --- | --- |",
    rows,
    "<!-- activerse-packages:end -->"
  )
}

build_load_block <- function(packages) {
  pattern <- paste(packages, collapse = "|")
  c(
    "# activerse-load:start",
    sprintf('search()[grepl("^package:(%s)$", search())]', pattern),
    "# activerse-load:end"
  )
}

replace_block <- function(lines, start_marker, end_marker, replacement) {
  start <- match(start_marker, lines)
  end <- match(end_marker, lines)

  if (is.na(start) || is.na(end) || end <= start) {
    stop("Could not find block markers: ", start_marker, " / ", end_marker, call. = FALSE)
  }

  suffix <- if (end < length(lines)) lines[(end + 1):length(lines)] else character()
  c(lines[seq_len(start - 1)], replacement, suffix)
}

description <- read.dcf(desc_path, all = TRUE)[1, ]
imports <- parse_dcf_packages(description[["Imports"]])
remotes <- parse_dcf_remotes(description[["Remotes"]])

if (length(new_packages)) {
  new_packages <- unique(new_packages)
  missing <- setdiff(new_packages, imports)
  if (length(missing)) {
    imports <- unique(c(imports, missing))
    remotes <- unique(c(remotes, paste0(owner, "/", missing)))

    desc_lines <- readLines(desc_path, warn = FALSE)
    desc_lines <- replace_dcf_field(desc_lines, "Imports", imports)
    desc_lines <- replace_dcf_field(desc_lines, "Remotes", remotes)
    writeLines(desc_lines, desc_path, sep = "\n")
    message("Updated DESCRIPTION with: ", paste(missing, collapse = ", "))
  } else {
    message("No new packages to add to DESCRIPTION.")
  }
}

if (!length(imports)) {
  stop("No imported activerse packages found in DESCRIPTION.", call. = FALSE)
}

r_lines <- readLines(r_path, warn = FALSE)
start <- match("activerse_packages <- function() {", r_lines)
if (is.na(start)) {
  stop("Could not locate activerse_packages() in R/activerse.R", call. = FALSE)
}
end_rel <- match("}", r_lines[(start + 1):length(r_lines)])
if (is.na(end_rel)) {
  stop("Could not locate the end of activerse_packages() in R/activerse.R", call. = FALSE)
}
end <- start + end_rel

pkg_block <- c(
  "activerse_packages <- function() {",
  "  c(",
  if (length(imports) == 1) {
    paste0('    "', imports, '"')
  } else {
    paste0('    "', imports, '"', c(rep(",", length(imports) - 1), ""))
  },
  "  )",
  "}"
)
suffix <- if (end < length(r_lines)) r_lines[(end + 1):length(r_lines)] else character()
r_lines <- c(r_lines[seq_len(start - 1)], pkg_block, suffix)
writeLines(r_lines, r_path, sep = "\n")
message("Updated R/activerse.R")

readme_rmd_lines <- readLines(readme_rmd_path, warn = FALSE)
readme_rmd_lines <- replace_block(
  readme_rmd_lines,
  "<!-- activerse-intro:start -->",
  "<!-- activerse-intro:end -->",
  build_intro_block(imports, remotes)
)
readme_rmd_lines <- replace_block(
  readme_rmd_lines,
  "<!-- activerse-packages:start -->",
  "<!-- activerse-packages:end -->",
  build_table_block(imports, remotes)
)
readme_rmd_lines <- replace_block(
  readme_rmd_lines,
  "# activerse-load:start",
  "# activerse-load:end",
  build_load_block(imports)
)
writeLines(readme_rmd_lines, readme_rmd_path, sep = "\n")
message("Updated README.Rmd")

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("rmarkdown is required to render README.md", call. = FALSE)
}

rmarkdown::render(
  input = readme_rmd_path,
  output_file = readme_md_path,
  quiet = TRUE
)
readme_html_path <- sub("\\.md$", ".html", readme_md_path)
if (file.exists(readme_html_path)) {
  file.remove(readme_html_path)
}
message("Rendered README.md")
