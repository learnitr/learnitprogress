#' Visualize details about the grade for one student
#'
#' @param data Grading data
#' @param show.name Indicate the name of the student in the title
#' @param give.note Indicate the final note in the title
#'
#' @return A ggplot object
#' @export
plot_grade <- function(data, show.name = TRUE, give.note = TRUE) {
  email <- data[1, ]$email
  name <- sub("\\.", " ", sub("@.+$", " ", email))
  login <- data[1, ]$login
  score <- round(data[1, ]$score, 1)
  max <- data[1, ]$max

  # First combine summary with details (leaving one empty line between the two)
  scores <- bind_rows(
    mutate(attr(data, "details"), app = paste0(" ", .data$app)),
    data.frame(app = "---------------"),
    mutate(attr(data, "summary"), score = .data$score / .data$max * 10,
      max = 10, app = ifelse(.data$type == "challenge", "challenge",
        paste(.data$type, "(all)")))
  )

  if (isTRUE(show.name)) {
    if (isTRUE(give.note)) {
      subtitle <- paste0(name, " - ", score, "/", max)
    } else {
      subtitle <- paste0(name, " - ", "(pas encore de note)")
    }
  } else {
    if (isTRUE(give.note)) {
      subtitle <- paste0("Note globale : ", score, "/", max)
    } else {
      subtitle <- paste0("Note globale : ", "(pas encore de note)")
    }
    subtitle <- paste0("Note globale : ", score, "/", max)
  }

  ggplot(data = scores, aes(x = .data$app, y = .data$max)) +
    geom_hline(yintercept = c(0:max(data$max)), col = "gray") +
    # White background
    geom_col(fill = "white", col = "gray50", width = 0.9) +
    # Add max
    geom_col(aes(x = .data$app, y = .data$max), fill = "gray95", col = "gray50",
      width = 0.9) +
    # User scores
    geom_col(aes(x = .data$app, y = .data$score, fill = .data$type),
      col = "black", width = 0.9) +
    coord_flip() +
    scale_y_discrete(expand = c(0, 0)) +
    cowplot::theme_cowplot(font_size = 10) +
    labs(subtitle = subtitle, x = "", y = "") +
    scale_fill_brewer(palette = "Dark2")
}
