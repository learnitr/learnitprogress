# sdd1m, sdd1c, sdd2m, sdd2c, sdd3m, sdd4m, sdd5m

#' Get a list of all user logins in a given class
#'
#' @param class The short identifier of the class
#' @param url The URL of the MongoDB database (LRS)
#' @param as.json If `TRUE`, return the result as a JSON string
#'
#' @return A character vector with the results
#' @export
class_logins <- function(class, url = getOption("learnitr.lrs_url"),
    as.json = FALSE) {
  # TODO: allow using this in a different context as SDD
  query <- switch(class,
    sdd1m = '{ "icflag": { "$in": ["S-BIOG-006", "S-BIOG-006,S-BIOG-015", "S-BIOG-027", "S-BIOG-027,S-BIOG-061"] }, "enrolled": true }',
    sdd1c = '{ "icourse": "S-BIOG-921", "enrolled": true }',
    sdd2m = '{ "icflag": { "$in": ["S-BIOG-015", "S-BIOG-006,S-BIOG-015", "S-BIOG-061", "S-BIOG-027,S-BIOG-061"] }, "enrolled": true }',
    sdd2c = '{ "icourse": "S-BIOG-937-958-959", "enrolled": true }',
    sdd3m = '{ "icourse": "S-BIOG-025", "enrolled": true }',
    sdd4mold = '{ "icourse": "S-BIOG-043", "enrolled": true }',
    sdd4m = '{ "icourse": "S-BIOG-077", "enrolled": true }',
    stop("Only 'sdd1m/c', 'sdd2m/c', 'sdd3m', or 'sdd4m(old)' are recognized classes"))
  mdb <- try(mongolite::mongo("users", url = url), silent = TRUE)
  if (inherits(mdb, "try-error"))
    stop("Error: impossible to connect to the users database")
  res <- mdb$find(query, '{ "login" : true, "iemail" : true, "_id" : false }')

  email <- res$iemail
  res <- res$login

  # Do we output the json string to be used in MongoDB directly?
  if (isTRUE(as.json)) {
    res2 <- paste(res, collapse = '", "')
    res <- structure(paste0('{ "$in": ["', res2, '"] }'),
      logins = res, n = length(res))
  } else {
    res <- structure(res, email = email)
  }
  res
}

# Get a selection of the apps that match a given aa
# (optionally restrict to a single module)
# tt <- paste0("A", formatC(1:12, width = 2, flag = "0")); tt
# grepl("^A0[1-6]", tt)
# grepl("^A(0[7-9])|(1[0-2])", tt)
#' @export
#' @rdname class_logins
#' @param aa The short identifier of the activity
#' @param module The module to restrict too, or `NULL` for all modules
class_aa <- function(aa, url = getOption("learnitr.lrs_url"), as.json = FALSE,
    module = NULL) {
  # This should be extracted from the database instead!
  # 2021-2022: we also exclude XnnA that correspond to excluded H5P exercices
  if (!is.null(module)) {
    # We only look for exercises in a specific module
    rx <- switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = ,
      sdd1mq2 = ,
      sdd1cq2 = ,
      sdd1mq3 = ,
      sdd1cq3 = '^A',
      sdd2mq1 = ,
      sdd2cq1 = ,
      sdd2mq2 = ,
      sdd2cq2 = ,
      #sdd2cq3 = ,
      sdd2mq3 = ,
      sdd2cq3 = '^B',
      sdd3mq1 = ,
      sdd3mq3 = '^C',
      sdd4mq1 = '^D',
      sdd5mq1 = '^E',
      stop("Not implemented for this aa"))
    rx <- paste0(rx, module, '[^A]')
    if (isTRUE(as.json))
      rx <- paste0('{ "$regex": "', rx, '", "$options": "" }')
    return(rx)
  }

  # prerequisite sections and should not be counted here
  if (isTRUE(as.json)) {
    switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = '{ "$regex": "^A0[1-5][^A]", "$options": "" }',
      sdd1mq2 = ,
      sdd1cq2 = '{ "$regex": "^A(0[6-9])|(10)[^A]", "$options": "" }',
      sdd1mq3 = ,
      sdd1cq3 = '{ "$regex": "^A(0[1-9])|(1[0-2])[^A]", "$options": "" }',
      sdd2mq1 = ,
      sdd2cq1 = '{ "$regex": "^B0[1-5][^A]", "$options": "" }',
      sdd2mq2 = ,
      sdd2cq2 = '{ "$regex": "^B(0[6-9])|(10)[^A]", "$options": "" }',
      #sdd2cq3 = '{ "$regex": "^B(09)|(1[0-2])[^A]", "$options": "" }',
      sdd2mq3 = ,
      sdd2cq3 = '{ "$regex": "^B(0[1-9])|(10)[^A]", "$options": "" }',
      sdd3mq1 = '{ "$regex": "^C0[1-5][^A]", "$options": "" }',
      sdd3mq3 = '{ "$regex": "^C0[1-5][^A]", "$options": "" }',
      sdd4mq1 = '{ "$regex": "^D0[1-5][^A]", "$options": "" }',
      #sdd5mq1 = '{ "$regex": "^E0[1-4][^A]", "$options": "" }',
      stop("Not implemented for this aa"))
  } else {
    switch(aa,
      sdd1mq1 = ,
      sdd1cq1 = "^A0[1-5][^A]",
      sdd1mq2 = ,
      sdd1cq2 = "^A(0[6-9])|(10)[^A]",
      sdd1mq3 = ,
      sdd1cq3 = "^A(0[1-9])|(1[0-2])[^A]",
      sdd2mq1 = ,
      sdd2cq1 = "^B0[1-5][^A]",
      sdd2mq2 = ,
      sdd2cq2 = "^B(0[6-9])|(10)[^A]",
      sdd2mq3 = ,
      sdd2cq3 = "^B(0[1-9])|(10)[^A]",
      sdd3mq1 = "^C0[1-5][^A]",
      sdd3mq3 = "^C0[1-5][^A]",
      sdd4mq1 = "^D0[1-5][^A]",
      #sdd5mq1 = "^E0[1-4][^A]",
      stop("Not implemented for this aa"))
  }
}

# Get the user login, given its email
#' @export
#' @rdname class_logins
#' @param email The email of the student
user_login <- function(email, url = getOption("learnitr.lrs_url")) {
  # Make sure email is in lowercase
  email <- tolower(email)
  # Get the login associated with a given email address
  # In case we have several login, return the most recent one
  mdb <- try(mongolite::mongo("users", url = url), silent = TRUE)
  if (inherits(mdb, "try-error"))
    stop("Error: impossible to connect to the users database")
  #query <- paste0('{ "iemail": "', email_moodle_case(email), '" }')
  query <- paste0('{ "iemail": "', tolower(email), '" }')
  fields <- '{ "login": true, "registered": true, "_id": false }'
  if (!mdb$count(query))
    return(NULL) # This email is not found
  res <- mdb$find(query, fields)
  try(mdb$disconnect, silent = TRUE) # Not all versions have this!
  # If there are several logins, return most recent one, and the list of all the
  # others as an attribute
  if (nrow(res) > 1) {
    res <- res[order(res$registered, decreasing = TRUE), ]
    logins <- res$login
    structure(res$login[1], logins = logins)
  } else {# Only one item
    res$login
  }
}
