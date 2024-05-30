#' Progression plot for one student (exercises H5P, Shiny & Learnr)
#'
#' @param user The user login
#' @param data The data for this user
#'
#' @return A ggplot object
#'
#' @export
plot_progression <- function(user, data) {
  ggplot(data = data, aes(x = .data$app, y = .data$max)) +
    geom_hline(yintercept = c(0:max(data$max)), col = "gray") +
    # White background
    geom_col(fill = "white", col = "gray50", width = 0.9) +
    # Average and max progression (avg and max raw_score not shown here)
    geom_col(aes(x = .data$app, y = .data$progress_max), fill = "gray95",
      col = "gray50", width = 0.9) +
    geom_col(aes(x = .data$app, y = .data$raw_score_avg), fill = "gray85",
      col = "gray50", width = 0.9) +
    # User's progression and raw_score
    geom_col(aes(x = .data$app, y = .data$progress), fill = "red4",
      col = "black", width = 0.5) +
    geom_col(aes(x = .data$app, y = .data$raw_score), fill = "royalblue4",
      col = "black", width = 0.5) +
    # User's relative activity indicator
    geom_col(aes(x = .data$app, y = .data$activity), fill = "black",
      col = "black", width = 0.03, position = position_nudge(x = -0.45)) +
    coord_flip() +
    #scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    cowplot::theme_cowplot(font_size = 14) +
    labs(x = "", y = "")
  #labs(title = paste("Progression -", user), x = "", y = "")
}
