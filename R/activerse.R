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

  keep <- vapply(names(conflicts), function(fun) {
    activerse_conflict_should_report(fun, conflicts[[fun]])
  }, logical(1))
  conflicts <- conflicts[keep]
  if (length(conflicts) == 0) {
    return(character())
  }

  vapply(names(conflicts), function(fun) {
    activerse_conflict_line(fun, conflicts[[fun]])
  }, character(1))
}

activerse_conflict_should_report <- function(fun, envs) {
  pkgs <- sub("^package:", "", envs)
  activerse_pkgs <- pkgs[pkgs %in% activerse_packages()]
  if (length(activerse_pkgs) == 0) {
    return(FALSE)
  }

  # actisensorlog intentionally re-exports actiread helpers and defines
  # SensorLog-specific wrappers that overlap with upstream actimetrics names.
  # Those are expected and should not be surfaced as attach-time conflicts.
  if (activerse_conflict_is_intentional(fun, pkgs)) {
    return(FALSE)
  }

  TRUE
}

activerse_conflict_is_intentional <- function(fun, pkgs) {
  winner <- activerse_conflict_winner(pkgs)
  if (is.null(winner) || winner != "actisensorlog") {
    return(FALSE)
  }

  losers <- setdiff(pkgs, winner)
  if (!all(losers %in% c("actiread", "actimetrics"))) {
    return(FALSE)
  }

  fun %in% c(
    "acti_calculate_distance",
    "acti_check_duplicate_times",
    "acti_convert_sensorlogger_time",
    "acti_example_sensorlog_file",
    "acti_example_sensorlogger_file",
    "acti_example_sensorlogger_location_file",
    "acti_minute_sensorlog",
    "acti_process_sensorlog",
    "acti_read_sensorlog",
    "acti_read_sensorlogger",
    "acti_read_sensorlogger_general",
    "acti_read_sensorlogger_location",
    "acti_rewrite_sensorlog_csv",
    "acti_sensorlog_csv_colnames_mapping",
    "acti_sensorlog_csv_spec",
    "acti_sensorlog_process_time",
    "acti_sensorlogger_location_colnames_mapping",
    "acti_sensorlogger_location_spec",
    "acti_summarise_sensorlog",
    "acti_summarize_distance_sensorlog",
    "acti_summarize_sensorlog"
  )
}

activerse_conflict_winner <- function(pkgs) {
  winners <- activerse_packages()[activerse_packages() %in% pkgs]
  if (length(winners) == 0) {
    return(NULL)
  }

  winners[[length(winners)]]
}

activerse_conflict_line <- function(fun, envs) {
  pkgs <- sub("^package:", "", envs)
  winner <- activerse_conflict_winner(pkgs)
  if (is.null(winner)) {
    winner <- pkgs[1]
  }
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
