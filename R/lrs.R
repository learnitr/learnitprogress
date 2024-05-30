#' Get data from exercises or projects for one student
#'
#' @param collection The collection in the MongoDB database
#' @param query The JSON query on the MongoDB database
#' @param fields  The fields to return
#' @param url The url to access the MongoDB database
#' @param count Do we count items instead of getting them?
#'
#' @return A data frame or a list with the data
#' @export
get_data <- function(collection, query, fields = '{}',
  url = getOption("learnitr.lrs_url"), count = FALSE) {
  mdb <- try(mongolite::mongo(collection, url = url), silent = TRUE)
  if (inherits(mdb, "try-error")) {
    stop("Error: impossible to connect to the database")
  } else {
    if (isTRUE(count)) {
      res <- mdb$count(query)
    } else {
      res <- mdb$find(query, fields = fields)
    }
  }
  mdb$disconnect()
  res
}

#' @export
#' @rdname get_data
#' @param email The email of the student
#' @param login The GitHub login of the student as alternate way to get its
#'   profile (when it is not `NULL`)
get_profile <- function(email, login = NULL) {
  if (is.null(login)) {
    get_data("users", query =
        paste0('{"iemail" : { "$eq" : "', tolower(email), '" } }'))[1, ]
  } else {
    get_data("users", query =
        paste0('{"iemail" : { "$eq" : "', tolower(email),
          '" }, "user_login": { "$eq": "', login, '" } }'))[1, ]
  }
}

#' @export
#' @rdname get_data
#' @param icourse The course identifier
#' @param module  The module, or `NULL` for exercises in all modules
get_all_data <- function(email, icourse, module = NULL) {
  message("Email: '", email, "'")
  message("ICourse: '", icourse, "'")
  if (!is.null(module))
    message("Module: '", module, "'")
  if (is.null(email) || email == "")
    return()

  # TODO: allow for using this outside of SDD courses
  course <- "sdd1m" # Default for S-BIOG-006 & S-BIOG-027
  if (icourse == "S-BIOG-015" || icourse == "S-BIOG-061") course <- "sdd2m"
  if (icourse == "S-BIOG-025") course <- "sdd3m"
  if (icourse == "S-BIOG-921") course <- "sdd1c"
  if (icourse == "S-BIOG-937-958-959") course <- "sdd2c"
  message("Course: '", course, "'")
  aa <- paste0(course, "q1")
  # Special case for q2 courses
  if (icourse == "S-BIOG-027") aa <- "sdd1mq2"
  if (icourse == "S-BIOG-061") aa <- "sdd2mq2"
  class_logs <- class_logins(course, as.json = TRUE)
  message("Class query: ", class_logs)
  class_apps <- class_aa(aa, as.json = TRUE, module = module)
  user <- user_login(email)
  message("User login: '", user, "'")
  resh <- h5p_prog(user, class_logs, class_apps)
  #print(nrow(resh))
  #print(head(resh))
  resl <- learnr_prog(user, class_logs, class_apps)
  #print(nrow(resl))
  #print(head(resl))
  ress <- shiny_prog(user, class_logs, class_apps)
  #print(nrow(ress))
  #print(head(ress))
  # We deal with all the possible cases of missing data
  if (is.null(resh) && is.null(resl) && is.null(ress) )
    return(structure(list(), comment = "Pas encore d'enregistrements."))
  if (is.null(resh)) {
    if (is.null(resl)) {
      res <- ress
    } else {# resl not null
      if (is.null(ress)) {
        res <- resl
      } else {
        res <- rbind(resl, ress)
      }
    }
  } else {# resh not null
    res <- resh
    if (!is.null(resl))
      res <- rbind(res, resl)
    if (!is.null(ress))
      res <- rbind(res, ress)
  }

  res <- res[order(res$app), ] #res %<-% arrange(res, app)
  # If not any student finish an exercise, we got NA in different places
  # -> replace these by zero for the plot
  res$max[is.na(res$max)] <- 0
  res$progress_max[is.na(res$progress_max)] <- 0
  res$progress[is.na(res$progress)] <- 0
  res$raw_score_max[is.na(res$raw_score_max)] <- 0
  res$raw_score_avg[is.na(res$raw_score_avg)] <- 0
  res$raw_score[is.na(res$raw_score)] <- 0
  res$count[is.na(res$count)] <- 0
  res$activity[is.na(res$activity)] <- 0
  attr(res, "user") <- user
  res
}

#' @export
#' @rdname get_data
get_github_data <- function(email) {
  # Return NULL for now
  NULL
}

#' @export
#' @rdname get_data
get_grid_data <- function(email, icourse, module = NULL) {
  message("Email: '", email, "'")
  message("ICourse: '", icourse, "'")
  if (!is.null(module))
    message("Module: '", module, "'")
  if (is.null(email) || email == "")
    return()

  course <- "sdd1m" # Default for S-BIOG-006 & S-BIOG-027
  if (icourse == "S-BIOG-015" || icourse == "S-BIOG-061") course <- "sdd2m"
  if (icourse == "S-BIOG-025") course <- "sdd3m"
  if (icourse == "S-BIOG-921") course <- "sdd1c"
  if (icourse == "S-BIOG-937-958-959") course <- "sdd2c"
  aa <- paste0(course, "q1")
  # Special case for q2 courses
  if (icourse == "S-BIOG-027") aa <- "sdd1mq2"
  if (icourse == "S-BIOG-061") aa <- "sdd2mq2"
  message("Course aa: '", aa, "'")
  user <- user_login(email)
  grade_dir <- get_grade_dir()
  if (is.null(grade_dir))
    return()
  # Correction grids are supposed to be in <grade_dir>/<aa>/<user>/*.csv
  grid_dir <- fs::path(grade_dir, aa, user)
  message("Grids dir: '", grid_dir, "'")
  if (!dir_exists(grid_dir))
    return()
  grids <- dir_ls(grid_dir, glob = "*.csv")
  message("Number of grids: '", length(grids), "'")
  if (!length(grids))
    return()
  names(grids) <- sub("\\.csv$", "", basename(grids))
  # Do we return only grids for one module?
  if (!is.null(module)) {
    grids_modules <- substring(names(grids), 2L, 3L)
    grids <- grids[grids_modules == module]
    if (!length(grids))
      return()
  }
  grids
}

#' @export
#' @rdname get_data
get_grade_data <- function(email, icourse) {
  message("Email: '", email, "'")
  message("ICourse: '", icourse, "'")
  if (is.null(email) || email == "")
    return()

  course <- "sdd1m" # Default for S-BIOG-006 & S-BIOG-027
  if (icourse == "S-BIOG-015" || icourse == "S-BIOG-061") course <- "sdd2m"
  if (icourse == "S-BIOG-025") course <- "sdd3m"
  if (icourse == "S-BIOG-921") course <- "sdd1c"
  if (icourse == "S-BIOG-937-958-959") course <- "sdd2c"
  aa <- paste0(course, "q1")
  # Special case for q2 courses
  if (icourse == "S-BIOG-027") aa <- "sdd1mq2"
  if (icourse == "S-BIOG-061") aa <- "sdd2mq2"
  message("Course aa: '", aa, "'")
  user <- user_login(email)
  grade_dir <- get_grade_dir()
  if (is.null(grade_dir))
    return()
  # Data are supposed to be in <grade_dir>/<aa>/grade_<user>.rds
  grade_file <- fs::path(grade_dir, aa, paste0("grade_", user, ".rds"))
  message("Grade file: '", grade_file, "'")
  if (!file.exists(grade_file))
    return()
  readRDS(grade_file)
}

# TODO: allow to customise root_dir
#' @export
#' @rdname get_data
#' @param root_dir The root directory where the grades are stored.
#' @param acad_year The academic year to use, or `NULL` to use the latest one.
get_grade_dir <- function(root_dir = "/data1/grades", acad_year = NULL) {
  if (!dir_exists(root_dir))
    return(NULL)
  if (is.null(acad_year)) {
    # Look for the latest subdir in alphabetic order and use it
    sub_dirs <- dir_ls(root_dir,type = "directory")
    if (!length(sub_dirs))
      return(NULL)
    acad_year <- basename(sub_dirs[length(sub_dirs)])
  }
  # Check that the full directory exists
  grade_dir <- fs::path(root_dir, acad_year)
  if (!dir.exists(grade_dir))
    return(NULL)
  grade_dir
}


# Unused code for now -----------------------------------------------------

#correct <- function(x, name, old, new) {
#  x[x[[name]] == old, name] <- new
#  x
#}

#correct_by_id_version <- function(x, id, version, name, new) {
#  x[x$id == id & x$version == version, name] <- new
#  x
#}

#get_learnr_data <- function(email) {
#  res <- get_data("learnr",
#    query =
#      paste0('{"email" : { "$eq" : "', email, '" }, "max" : { "$gt" : 0 } }'),
#    fields = '{ "data" : false }'
#  )
#  #  # I need to correct XNNa_... -> XNNLa_...
#  #  res$app <- sub("^([A-E][0-9]{2})([a-z]_.+$)", "\\1L\\2", res$app)
#  #  # I need to correct B01La_rappel because it is indicated 22 questions,
#  #  # but there are 23 of them!!!
#  #  res[res$app == "B01La_rappel", "max"] <- 23
#  # Eliminate A00, B00, C00, ...
#  res <- res[!grepl("^.00", res$app), ]
#  # We keep only highest score for each ex (app_label)
#  # So, sort score descending, make app_label, and keep only unique items
#  res <- res[order(res$score, decreasing = TRUE), ]
#  app_label <- paste(res$app, res$label, sep = "_")
#  res <- res[!duplicated(app_label), ]
#  res
#}

#get_h5p_data <- function(email) {
#  res <- get_data("h5p",
#    query =
#      paste0('{"email" : { "$eq" : "', tolower(email),
#        '" }, "max" : { "$gt" : 0 } }'),
#    fields = '{ "data" : false }'
#  )
#  # Well, here names are a complete mess! Thus, lot of work to correct these!
#  # Replace NA by "" in version
#  res[is.na(res$version), "version"] <- ""
#  #res <- correct_by_id_version(res, "8", "437dae39-40f6-4251-a056-f747a9888778",
#  #  "app", "A02Hb_R markdown/diff script markdown")
#  #res <- correct_by_id_version(res, "8", "9d23d3a0-490c-450b-9cdd-e8d0927f28ed",
#  #  "app", "A02Hb_R markdown/chunk definition")
#  #res <- correct_by_id_version(res, "8", "21444f3a-efed-4c92-99d6-f870628611f4",
#  #  "app", "A02Hb_R markdown/why chunk")
#  #res <- correct_by_id_version(res, "8", "3f31e62e-9c9c-457f-9a7d-ad08060f3588",
#  #  "app", "") # This just collects results from chunk definition & why chunk
#  #res <- correct_by_id_version(res, "10", "dc224f3d-32a7-43c8-a92a-c3d3c0694cfd",
#  #  "app", "A02Ha_nuage/questions")
#  #res <- correct_by_id_version(res, "10", "",
#  #  "app", "A02Ha_nuage")
#  # ...
#  # Eliminate items where app == ""
#  res <- res[res$app != "", ]
#  # Sort by decreasing score, then eliminate duplicate app items
#  res <- res[order(res$score, decreasing = TRUE), ]
#  res <- res[!duplicated(res$app), ]
#  # Completed items are redundant with answered items
#  res <- res[res$verb != "completed", ]
#  res
#}

#get_shiny_data <- function(email) {
#  res <- get_data("shiny",
#    query =
#      paste0('{"email" : { "$eq" : "', tolower(email),
#        '" }, "max" : { "$gt" : 0 } }'),
#    fields = '{ "data" : false }'
#  )
#  #  # Unfortunately, corrections are required for app names
#  #  res <- correct(res, "app", "A02a_limits", "A02Sa_limits")
#
#  # Keep only highest score obtained for each item
#  res <- res[order(res$version, decreasing = TRUE), ]
#  res <- res[order(res$score, decreasing = TRUE), ]
#  res <- res[!duplicated(res$app), ]
#  res
#}
