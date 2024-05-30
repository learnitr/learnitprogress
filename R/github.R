# Check if a github user exists (just check if the URL exists, but could also
# be an organization and could not be indeed the user's account => check later)
#' Check a GitHub account
#'
#' @param user The user login to check on GitHub.
#'
#' @return `TRUE` if the user login exists on GitHub, `FALSE` otherwise.
#' @export
#'
#' @examples
#' is_github_user("phgrosjean")
#' is_github_user("nonexistinguser")
is_github_user <- function(user) {
  RCurl::url.exists(paste0("https://github.com/", user, "/"))
}
