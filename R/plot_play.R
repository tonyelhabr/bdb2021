
#' Save a plot
#' 
#' @inheritParams save_animation
#' @param height,width Height and width to use for saved image, in inches.
#' @export
save_plot <-
  function(object,
           dir = get_bdb_dir_figs(),
           file = deparse(substitute(object)),
           ext = 'png',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           width = height,
           ...) {
    ggplot2::ggsave(
      plot = object,
      filename = path,
      width = width,
      height = height,
      type = 'cairo',
      ...
    )
  }
