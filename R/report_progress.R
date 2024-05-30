#' Show the progress report for one student
#'
#' @param email The email address of the student
#' @param second.course Should we display data for a secondary course that the
#'   student follows?
#' @param course The primary course
#' @param module Should we restrict data for one module, (or all with `NULL`)?
#' @param browse Should we browse to the provided URL?
#' @param url The url to connect to the MongoDB database
#' @param port The port where the Shiny application is serving. If 0, a version
#'   on a server (for instance, on Posit Connect) is used.
#'
#' @return The URL to the progress report
#' @export
#'
#' @examples
#' \dontrun{
#' # In a different R session, run the following line:
#' learnitprogress::run_progress_report()
#' # Then...
#' options(learnitr.lrs_url = "mongodb://127.0.0.1/sdd")
#' learnitprogress::report_progress("student.email@school.edu")
#' }
report_progress <- function(email, second.course = FALSE, course = NULL,
    module = NULL, browse = TRUE, url = getOption("learnitr.lrs_url"),
    port = 3260) {
  if (is.null(port))
    stop("The Shiny application progress report does not seem to be running")
  # Get info about that user
  # This is an alternate way to get user data that we use in SDD
  #users <- data.io::read(svMisc::pcloud_crypto("sdd_users", "all_users.csv"))
  #users <- drop_na(users, iemail)
  #  view_email <- tolower(email)
  #user <- dplyr::filter(users, iemail == view_email)
  mdb <- mongolite::mongo("users", url = url)
  user <- mdb$find(paste0('{"iemail" : { "$eq" : "', tolower(email), '" } }'))
  if (!NROW(user))
    stop("User not found!")

  # In user iemail, I have a lowercase version, but I need -
  # email instead
  user$iemail <- user$email
  encode_param <- function(param, data = user) {
    value <- URLencode(as.character(data[[param]]), reserved = TRUE)
    paste0(param, "=", value)
  }
  params <- c("iemail", "iid", "ifirstname", "ilastname", "institution",
    "icourse", "ictitle", "iurl", "iref")
  # Possibly use  first or second course from icflag
  # Note: we don't update ictitle here => may be wrong!
  if (!missing(second.course)) {
    courses <- trimws(strsplit(user$icflag, ",", fixed = TRUE)[[1]])
    if (length(courses) > 1) {
      if (isTRUE(second.course)) {
        user[["icourse"]] <- courses[2]
      } else {# First course
        user[["icourse"]] <- courses[1]
      }
    }
  }
  search <- character(0)
  for (param in params)
    search <- c(search, encode_param(param))

  # If course and module are specified, append them to the search string too
  if (!is.null(course))
    search <- c(search, paste0("course=", course))
  if (!is.null(module))
    search <- c(search, paste0("module=", module))

  if (port == 0) {
    # We use the server version
    # TODO: allow customizing this url
    url <- paste0("https://sdd.umons.ac.be/sdd-progress-report/?",
      paste(search, collapse = "&"))
  } else {
    # We use the local development version
    url <- paste0("http://127.0.0.1:", port, "?", paste(search, collapse = "&"))
  }

  if (isTRUE(browse))
    browseURL(url)
  invisible(url)
}
#' @export
#' @rdname report_progress
run_progress_report <- function(port = 3260) {
  shiny::runApp(system.file("shiny", "progress-report",
    package = "learnitprogress"), port = port, launch.browser = FALSE)
}
