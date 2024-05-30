#' @details
#' The learnitprogress package implements a Shiny application that summarizes
#' the progression of each of your course's students in their LearnIt::R
#' exercises and projects. It queries the MongoDB database that is collecting
#' your LearnIt::R activities (H5P, Shiny, learnr, GitHub projects...).
#'
#' See ...
#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr bind_rows filter full_join mutate select
#' @importFrom fs dir_exists dir_ls path
#' @importFrom ggplot2 aes coord_flip geom_col geom_hline geom_line geom_point geom_smooth geom_vline ggplot labs position_nudge scale_fill_brewer scale_y_discrete
#' @importFrom mongolite mongo
#' @importFrom RCurl url.exists
#' @importFrom rlang .data
#' @importFrom utils browseURL URLencode
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
