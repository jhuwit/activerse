activerse_packages <- function() {
  c(
    "actibase",
    "actiread",
    "actimetrics",
    "actisensorlog",
    "actiwalkability"
  )
}

activerse_attach <- function(packages = activerse_packages(), message = TRUE) {
  loaded <- character()
  missing <- character()

  for (pkg in packages) {
    if (paste0("package:", pkg) %in% search()) {
      loaded <- c(loaded, pkg)
      next
    }

    if (requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
      loaded <- c(loaded, pkg)
    } else {
      missing <- c(missing, pkg)
    }
  }

  if (message && length(loaded) > 0) {
    packageStartupMessage(activerse_attach_message(loaded))
  }

  conflicts <- activerse_conflicts()
  if (message && length(conflicts) > 0) {
    packageStartupMessage(activerse_conflict_message(conflicts))
  }

  if (message && length(missing) > 0) {
    packageStartupMessage(
      paste("Packages not available:", paste(missing, collapse = ", "))
    )
  }

  invisible(list(attached = loaded, missing = missing, conflicts = conflicts))
}

activerse_attach_message <- function(packages) {
  versions <- vapply(packages, function(pkg) {
    as.character(utils::packageVersion(pkg))
  }, character(1))

  lines <- paste0("- ", packages, " ", versions)
  paste(
    "Attaching activerse packages",
    paste(lines, collapse = "\n"),
    sep = "\n"
  )
}

activerse_conflicts <- function(only = NULL) {
  if ("package:conflicted" %in% search()) {
    return(character())
  }
  package_envs <- grep("^package:", search(), value = TRUE)

  package_envs <- setdiff(package_envs, "package:activerse")
  if (!is.null(only)) {
    only <- union(only, activerse_packages())
    package_envs <- package_envs[sub("^package:", "", package_envs) %in% only]
  }

  if (length(package_envs) == 0) {
    return(character())
  }

  objects <- lapply(package_envs, function(env) {
    ls(env, all.names = TRUE)
  })
  names(objects) <- package_envs

  conflicts <- activerse_invert(objects)
  conflicts <- conflicts[vapply(conflicts, length, integer(1)) > 1]
  if (length(conflicts) == 0) {
    return(character())
  }

  # Magrittr pipe operators are intentionally ignored in the attach message.
  conflicts <- conflicts[!names(conflicts) %in% c("%>%", "%<>%", "%T>%", "%$%")]
  if (length(conflicts) == 0) {
    return(character())
  }

  keep <- vapply(conflicts, function(envs) {
    any(sub("^package:", "", envs) %in% activerse_packages())
  }, logical(1))
  conflicts <- conflicts[keep]
  if (length(conflicts) == 0) {
    return(character())
  }

  vapply(names(conflicts), function(fun) {
    activerse_conflict_line(fun, conflicts[[fun]])
  }, character(1))
}

activerse_conflict_line <- function(fun, envs) {
  pkgs <- sub("^package:", "", envs)
  winners <- pkgs[pkgs %in% activerse_packages()]
  winner <- if (length(winners) > 0) winners[1] else pkgs[1]
  losers <- setdiff(pkgs, winner)

  if (length(losers) == 0) {
    return("")
  }

  paste0(
    "✖ ",
    winner,
    "::",
    fun,
    "() masks ",
    paste0(losers, "::", fun, "()", collapse = ", ")
  )
}

activerse_conflict_message <- function(conflicts) {
  if (length(conflicts) == 0) {
    return(NULL)
  }

  header <- paste0(
    "── Conflicts ────────────────────────────────────────── activerse ",
    activerse_package_version(),
    " ──"
  )
  hint <- "ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors"

  paste(c(header, conflicts, hint), collapse = "\n")
}

activerse_invert <- function(x) {
  out <- list()

  for (nm in names(x)) {
    for (value in x[[nm]]) {
      out[[value]] <- c(out[[value]], nm)
    }
  }

  out
}

activerse_package_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("activerse")),
    error = function(e) NULL
  )

  if (!is.null(version)) {
    return(version)
  }

  desc <- tryCatch(
    suppressWarnings(utils::packageDescription("activerse")),
    error = function(e) NULL
  )
  if (!is.null(desc)) {
    version <- tryCatch(as.character(desc[["Version"]]), error = function(e) NULL)
    if (!is.null(version) && length(version) == 1 && !is.na(version)) {
      return(version)
    }
  }

  "unknown"
}
