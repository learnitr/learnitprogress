#' Progression in learnrs, H5P and Shiny apps
#'
#' @param user_login The GitHub login of one student
#' @param class_logins All the logins for the class
#' @param class_aa The AA identifier for this class
#' @param class_data The data for this class
#' @param url The url of the MongoDB data base
#'
#' @return A data frame with all the progression data for the user and the class
#' @export
learnr_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = getOption("learnitr.lrs_url")) {
  if (is.null(class_data)) {
    class_data <- learnr_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No Shiny data for this course
  user_data <- learnr_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(app = class_data$app, id = rep("", n),
      items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }

  # Merge statistics for user and class by app
  res <- dplyr::full_join(class_data, user_data)
  res <- dplyr::select(res, 'app', 'id', 'items', 'items_done', 'max',
    'progress_max', 'progress', 'raw_score_max', 'raw_score_avg', 'raw_score',
    'count_all', 'count_avg', 'count')
  res <- dplyr::mutate(res,
    activity = pmin(1, .data$count / .data$count_avg) *
      pmax(.data$max, na.rm = TRUE))
  as.data.frame(res)
}

#' @export
#' @rdname learnr_prog
h5p_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = getOption("learnitr.lrs_url")) {
  if (is.null(class_data)) {
    class_data <- h5p_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No Shiny data for this course

  user_data <- h5p_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(id = class_data$id,
      items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }

  # Merge statistics for user and class by app
  res <- dplyr::full_join(class_data, user_data)
  res <- dplyr::select(res, 'app', 'id', 'items', 'items_done', 'max',
    'progress_max', 'progress', 'raw_score_max', 'raw_score_avg', 'raw_score',
    'count_all', 'count_avg', 'count')
  res <- dplyr::mutate(res,
    activity = pmin(1, .data$count / .data$count_avg) *
      pmax(.data$max, na.rm = TRUE))
  as.data.frame(res)
}

#' @export
#' @rdname learnr_prog
shiny_prog <- function(user_login, class_logins, class_aa, class_data = NULL,
    url = getOption("learnitr.lrs_url")) {
  if (is.null(class_data)) {
    class_data <- shiny_class_prog(class_logins, class_aa, url = url)
  }
  if (is.null(class_data))
    return(NULL) # No Shiny data for this course
  user_data <- shiny_user_prog(user_login, class_aa, url = url)
  if (is.null(user_data)) {
    n <- nrow(class_data)
    user_data <- data.frame(app = class_data$app, id = rep("", n),
      items = class_data$app, items_done = rep("", n), count = rep(0, n),
      progress = rep(0, n), raw_score = rep(0, n))
  }

  # Merge statistics for user and class by app
  res <- dplyr::full_join(class_data, user_data)
  res <- dplyr::select(res, 'app', 'id', 'items', 'items_done', 'max',
    'progress_max', 'progress', 'raw_score_max', 'raw_score_avg', 'raw_score',
    'count_all', 'count_avg', 'count')
  res <- dplyr::mutate(res,
    activity = pmin(1, .data$count / .data$count_avg) *
      pmax(.data$max, na.rm = TRUE))
  as.data.frame(res)
}


# Internal functions ------------------------------------------------------

# Get Learnr data for a whole class
learnr_class_prog <- function(class_logins, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_learnr <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_learnr, "try-error"))
    stop("Error: impossible to connect to the learnr database")

  if (!mdb_learnr$count(paste0('{ "type" : "learnr", "login": ', class_logins, ',
    "app": ', class_aa, ', "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] } }')))
    return(NULL)

  part1 <- mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "type" : "learnr",
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "done": { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1] }, "$label", null ]
    } },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "app": "$_id",
    "items": "$done",
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "progress_max": { "$size": "$done" },
    "raw_score_max": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "max": "$max",
    "_id": false
  }
} ]'))

  # raw_score_avg
  part2 <- mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "type" : "learnr",
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": { "login": "$login", "app": "$app" },
    "done": { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1]}, "$label", null ]
    } }
  }
}, {
  "$project": {
    "login": "$_id.login",
    "app": "$_id.app",
    "raw_score_max_by_user": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "_id": false
  }
}, {
  "$group": {
    "_id": "$app",
    "raw_score_avg": { "$avg": "$raw_score_max_by_user" }
  }
}, {
  "$project": {
    "app": "$_id",
    "raw_score_avg": { "$ifNull": [ "$raw_score_avg", 0] },
    "_id": false
  }
} ]'))
  dplyr::full_join(part1, part2)
}

# Get learnrs progression for one student
learnr_user_prog <- function(user_login, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_learnr <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_learnr, "try-error"))
    stop("Error: impossible to connect to the learnr database")

  if (!mdb_learnr$count(paste0('{ "type" : "learnr", "login": "', user_login, '",
    "app": ', class_aa, ', "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] } }')))
    return(NULL)

  mdb_learnr$aggregate(paste0('[ {
  "$match": {
    "type" : "learnr",
    "login": "', user_login, '",
    "app": ', class_aa, ',
    "max": { "$gt": 0 },
    "verb": { "$in": ["answered", "submitted"] }
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "done" : { "$addToSet": "$label" },
    "succeeded": { "$addToSet": {
      "$cond": [ { "$eq": ["$score", 1]}, "$label", null ]
    } }
  }
}, {
  "$project": {
    "app": "$_id",
    "id": "",
    "items_done": { "$setDifference": [ "$succeeded", [null] ] },
    "count": "$count",
    "progress": { "$size": "$done" },
    "raw_score": { "$size": { "$setIntersection": ["$succeeded", "$done"] } },
    "_id": false
  }
} ]'))
}

# Get H5P data for a whole class
h5p_class_prog <- function(class_logins, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_h5p <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_h5p, "try-error"))
    stop("Error: impossible to connect to the H5P database")

  if (!mdb_h5p$count(paste0('{ "type" : "h5p", "login": ', class_logins, ',
    "app": ', class_aa, ' }')))
    return(NULL)

  part1 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "type" : "h5p",
    "login": ', class_logins, ',
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$id",
    "apps": { "$addToSet": {
      "$cond": [ { "$eq": ["$version", null] }, "$app", null ]
    } },
    "count": { "$sum": {
      "$cond": [ { "$in": ["$verb", ["attempted"]] }, 0, 1 ]
    } }
  }
}, {
  "$project": {
    "id": "$_id",
    "app": { "$first": { "$setDifference": [ "$apps", [null] ] } },
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "_id": false
  }
} ]'))

  part2 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "type" : "h5p",
    "login": ', class_logins, ',
    "app": ', class_aa, ',
    "verb": "answered",
    "app": { "$ne": "" },
    "max": { "$gt": 0 }
  }
}, {
  "$group": {
    "_id": { "id": "$id", "app": "$app" },
    "score_max": { "$max": "$score" },
    "score_avg": { "$avg": "$score" },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "id": "$_id.id",
    "app": "$_id.app",
    "progress": "$max",
    "raw_score_max": "$score_max",
    "raw_score_avg": "$score_avg",
    "max": "$max",
    "_id": false
  }
}, {
  "$group": {
    "_id": "$id",
    "items": { "$addToSet": "$app" },
    "progress": { "$sum": "$progress" },
    "raw_score_max": { "$sum": "$raw_score_max" },
    "raw_score_avg": { "$avg": "$raw_score_avg" },
    "max": { "$sum": "$max" }
  }
}, {
  "$project": {
    "id": "$_id",
    "items": "$items",
    "progress_max": "$progress",
    "raw_score_max": "$raw_score_max",
    "raw_score_avg": "$raw_score_avg",
    "max": "$max",
    "_id": false
  }
} ]'))

  res <- dplyr::full_join(part1, part2)
  # Eliminate entries where count_all is zero
  res <- dplyr::filter(res, .data$count_all > 0)
  res
}

# Get progression in H5P exercises for one student
h5p_user_prog <- function(user_login, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_h5p <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_h5p, "try-error"))
    stop("Error: impossible to connect to the H5P database")

  if (!mdb_h5p$count(paste0('{ "type" : "h5p", "login": "', user_login, '",
    "app": ', class_aa, ' }')))
    return(NULL)

  part1 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "type" : "h5p",
    "login": "', user_login, '",
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$id",
    "count": { "$sum": {
      "$cond": [ { "$in": ["$verb", ["attempted"]] }, 0, 1 ]
    } }
  }
}, {
  "$project": {
    "id": "$_id",
    "count": "$count",
    "_id": false
  }
} ]'))

  if (!mdb_h5p$count(paste0('{ "type" : "h5p", "login": "', user_login, '",
    "app": ', class_aa, ', "verb": "answered", "max": { "$gt": 0 } }'))) {
    # Fake data because the student did not answered to anything yet
    n <- nrow(part1)
    part2 <- data.frame(id = part1$id, items_done = rep("", n),
      progress = rep(0, n), raw_score = rep(0, n))
  } else {
    part2 <- mdb_h5p$aggregate(paste0('[ {
  "$match": {
    "type" : "h5p",
    "login": "', user_login, '",
    "app": ', class_aa, ',
    "verb": "answered",
    "max": { "$gt": 0 }
  }
}, {
  "$group": {
    "_id": { "id": "$id", "app": "$app" },
    "progress": { "$max": "$max" },
    "raw_score": { "$max": "$score" }
  }
}, {
  "$project": {
    "id": "$_id.id",
    "app": "$_id.app",
    "progress": "$progress",
    "raw_score": "$raw_score",
    "_id": false
  }
}, {
  "$group": {
    "_id": "$id",
    "items_done": { "$addToSet": "$app" },
    "progress": { "$sum": "$progress" },
    "raw_score": { "$sum": "$raw_score" }
  }
}, {
  "$project": {
    "id": "$_id",
    "items_done": "$items_done",
    "progress": "$progress",
    "raw_score": { "$ifNull": [ "$raw_score", 0] },
    "_id": false
  }
} ]'))
  }

  res <- dplyr::full_join(part1, part2)
  # Eliminate entries where count is zero
  res <- dplyr::filter(res, .data$count > 0)
  res
}

# Get Shiny apps data for a whole class
shiny_class_prog <- function(class_logins, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_shiny <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_shiny, "try-error"))
    stop("Error: impossible to connect to the shiny database")

  # Not used for now, but kept for future use...
  #"progress_avg": { "$avg": {
  #  "$cond": [ { "$in": ["$verb", ["evaluated"] ] }, "$max", null ]
  #} },

  if (!mdb_shiny$count(paste0('{ "type" : "shiny", "login": ', class_logins, ',
    "app": ', class_aa, ' }')))
    return(NULL)

  mdb_shiny$aggregate(paste0('[ {
  "$match": {
    "type" : "shiny",
    "login": ', class_logins, ',
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "raw_score_max": { "$max": "$score" },
    "raw_score_avg": { "$avg": "$score" },
    "max": { "$max": "$max" }
  }
}, {
  "$project": {
    "app": "$_id",
    "count_all": "$count",
    "count_avg": { "$divide": ["$count", ', attr(class_logins, "n"), '] },
    "progress_max": "$max",
    "raw_score_max": "$raw_score_max",
    "raw_score_avg": "$raw_score_avg",
    "max": "$max",
    "_id": false
  }
} ]'))
}

# Get the progression in Shiny apps for one student
shiny_user_prog <- function(user_login, class_aa,
    url = getOption("learnitr.lrs_url")) {
  mdb_shiny <- try(mongolite::mongo("events", url = url), silent = TRUE)
  if (inherits(mdb_shiny, "try-error"))
    stop("Error: impossible to connect to the shiny database")

  if (!mdb_shiny$count(paste0('{ "type" : "shiny", "login": "', user_login, '",
    "app": ', class_aa, ' }')))
    return(NULL)

  mdb_shiny$aggregate(paste0('[ {
  "$match": {
    "type" : "shiny",
    "login": "', user_login, '",
    "app": ', class_aa, '
  }
}, {
  "$group": {
    "_id": "$app",
    "count": { "$sum": 1 },
    "progress": { "$max": "$max" },
    "raw_score": { "$max": "$score" }
  }
}, {
  "$project": {
    "app": "$_id",
    "id": "",
    "items": "$_id",
    "items_done": {
      "$cond": [ { "$gt": ["$progress", 0] }, ["$_id"], [] ]
    },
    "count": "$count",
    "progress": "$progress",
    "raw_score": { "$ifNull": [ "$raw_score", 0] },
    "_id": false
  }
} ]'))
}
