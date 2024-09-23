# Check and getURL parameters ---------------------------------------------

#' Check student profile to identify it from URL's parameters
#'
#' @description
#' Check if a GitHub user profile exists by checking its homepage.
#'
#' @param profile Profile data gathered from the LRS
#' @param url_profile Profile data gathered from the URL
#'
#' @return `TRUE` if the profile is correct, `FALSE` otherwise.
#' @export
check_profile <- function(profile, url_profile) {
  # In case of a wrong profile, return FALSE with a comment indicating what is
  # wrong (stop at first error and don't check further)
  incorrect <- function(comment)
    structure(FALSE, comment = comment)

  # Github login not being in the URL params, it is not tested
  # also, course and url are not tested as they may vary (a student may follow
  # several courses, and the Moddle URL could change at any time)
  if (tolower(profile$iemail) != tolower(as.character(url_profile['iemail'])))
    return(incorrect("'iemail' incorrect"))
  if (profile$iid != as.character(url_profile['iid']))
    return(incorrect("'iid' incorrect"))
  if (tolower(profile$ifirstname) !=
      tolower(as.character(url_profile['ifirstname'])))
    return(incorrect("'fist name' incorrect"))
  if (tolower(profile$ilastname) !=
      tolower(as.character(url_profile['ilastname'])))
    return(incorrect("'last name' incorrect"))
  # Do not check institution because it is empty for a few students
  #if (profile$institution != as.character(url_profile['institution']))
  #  return(incorrect("'institution' incorrect"))
  # Note: we don't check icourse and ictitle here, because a student may be
  # registered to several courses => it may change from the stored profile
  # For iref (Moodle user's UUID), this item is not accessible outside of Moodle
  # (and our LRS once the user is registered). However, it makes problem for us,
  # because we cannot check the report before a user registers. The magic value
  # "teacher-special-access" for iref allows to by-pass that item (keep it?)
  # It can be used both in the profile file and the URL for debugging purposes
  #if (profile$iref != "teacher-special-access" &&
  #    as.character(url_profile['iref']) != "teacher-special-access" &&
  #    profile$iref != as.character(url_profile['iref']))
  #  return(incorrect("'iref' incorrect"))

  # All test corrects => OK
  TRUE
}

#' URL parameters used to identify a student
#'
#' @description
#' Get the list of required parameters to identify a student from the URL.
#'
#' @return A character vector with the list of parameters
#' @details
#' In Moodle, the URL variables must be set as follows:
#' - iemail        = Student's email
#' - iid           = Student' ID ("Nom d'utilisateur" in French in Moodle)
#' - ifirstname    = First name of the student
#' - ilastname     = Last name of the student
#' - institution  = Institution
#' - icourse      = Identifier for the course
#' - ictitle      = Title of the course
#' - iurl         = URL of the Moodle server
#' - iref         = Moodle ID
#'
#' @export
#' @rdname check_profile
#'
#' @examples
#' query_params_list()
query_params_list <- function() {
  c("iemail", "iid", "ifirstname", "ilastname",
    "institution", "icourse", "ictitle", "iurl", "iref")
}

#' Check if all required parameters are in the query URL
#'
#' @param query The query part of the URL (text after ?, like in
#' https://mysite.org?params1=1&param2=2). The query is a character string with
#' each value, named with the parameters names.
#'
#' @return `TRUE` if all required parameters are in the query, `FALSE` otherwise.
#' @export
#' @rdname check_profile
#'
#' @examples
#' is_correct_query(c(iemail = "student@uni.edu", iid = "1234",
#'   ifirstname = "John", ilastname = "Doe", institution = "University",
#'   icourse = "123", ictitle = "Course title", iurl = "https://moodle.uni.edu",
#'   iref = "2FAB3EE8-42B5-4B61-DA23-2167FACD7B569"))
#' is_correct_query(c(iemail = "student@uni.edu"))
is_correct_query <- function(query) {
  all(query_params_list() %in% names(query))
}

#
#' Reformat the query (eliminate possible other parameters) to get user profile
#'
#' @return A list with the required parameters
#' @export
#' @rdname check_profile
#'
#' @examples
#' profile_from_query(c(iemail = "student@uni.edu", iid = "1234", other = "a"))
profile_from_query <- function(query) {
  query[query_params_list()]
}
