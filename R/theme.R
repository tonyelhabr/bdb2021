
#' @export
theme_set_and_update_bdb <- function(...) {
  extrafont::loadfonts('win', quiet = TRUE)
  ggplot2::theme_set(ggplot2::theme_minimal())
  ggplot2::theme_update(
    ...,
    text = ggplot2::element_text(family = 'Karla'),
    title = ggplot2::element_text('Karla', size = 14, color = 'gray20'),
    plot.title = ggplot2::element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
    plot.title.position = 'plot',
    axis.text = ggplot2::element_text('Karla', size = 14),
    # axis.title = element_text(size = 24, face = 'bold'),
    axis.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.99),
    # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
    # axis.title.x = element_text(hjust = 0.95),
    # axis.title.y = element_text(hjust = 0.95),
    # axis.line = element_line(color = 'gray80'),
    axis.line = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = 'gray80'),
    panel.grid.minor = ggplot2::element_line(color = 'gray80'),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10),
    # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
    # panel.background = element_rect(fill = '#fffaf0', color = NA),
    # plot.background = element_rect(fill = '#fffaf0', color = NA),
    # plot.caption = element_text(size = 15, face = 'italic'),
    # plot.caption = element_text('Karla', size = 14, color = 'gray50', hjust = 1),
    plot.caption = ggplot2::element_text('Karla', size = 10, color = 'gray20', hjust = 0),
    plot.caption.position = 'plot',
    plot.tag = ggplot2::element_text('Karla', size = 12, color = 'gray20', hjust = 0), 
    # plot.tag.position = c(.01, 0.02),
    # legend.text = element_text(size = 14),
    legend.text = ggplot2::element_text(size = 14)
  )
  ggplot2::update_geom_defaults('text', list(family = 'Karla', size = 4, color = 'white'))
}
