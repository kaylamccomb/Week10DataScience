#' theme_learninglab()
#'
#' @returns Add onto a ggplot object for formatting.
#' @export
#'
#' @examples
#' ggplot(data, aes(x = independent)) +
#' geom_histogram() +
#' theme_learninglab()
#'
theme_learninglab <- function() {
  #make custom color palette
  lab_palette <- c("#176017", "#7D84B2", "#C9FBFF", "#031927", "#CE7DA5")

  list(
    ggplot2::theme(
      #make background transparent
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      #make titles larger & bold axes
      axis.text = element_text(size = 14),
      axis.title = element_text(face = "bold", size = 16),
      plot.title = element_text(face = "bold", size = 18),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12)
    ),
    ggplot2::scale_color_manual(values = lab_palette),
    ggplot2::scale_fill_manual(values = lab_palette)
  )
}
