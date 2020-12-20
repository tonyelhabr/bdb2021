
.paste_collapse <-
  function(...,
           start = '(',
           end = ')',
           collapse = paste0(end, '|', start),
           sep = '') {
    paste0(start, paste(..., collapse = collapse, sep = sep), end)
  }

.paste_formula_x <-
  function(x, include = TRUE, sep = '') {
    backtick <- '`'
    sign <- ifelse(include, '+', '-')
    collapse <- paste0(backtick, ' ', sign, ' ', backtick)
    paste0(backtick, paste(x, collapse = collapse, sep = sep), backtick)
    # .paste_collapse(x, start = backtick, end = backtick, collapse = collapse, sep = sep)
  }

.get_from_rgx <-
  function(data, rgx = NULL) {
    if(!is.null(rgx)) {
      x <- data %>% names() %>% stringr::str_subset(rgx)
    }
    unique(x)
  }

#' Generate a model formula
#'
#' @param data data.frame
#' @param y character representing the response variable in the `data`
#' @param x character vector representing the independent variables to include in the resulting formula
#' @param x_include,x_exclude explicit variables (or columns) in `data` to include or not include in the model formula
#' @param rgx_x_include,rgx_x_exclude Like `x_include` and `x_exclude`, but regular expressions
#' @param intercept Boolean indicating whether to include an intercept term
#' @export
generate_formula <-
  function(data,
           y,
           x = NULL,
           x_include = NULL,
           x_exclude = NULL,
           rgx_x_include = NULL,
           rgx_x_exclude = NULL,
           intercept = TRUE) {

    nms <- data %>% names()
    assertthat::assert_that(is.character(y), length(y) == 1L, (y %in% nms))

    if (is.null(x)) {
      if (!is.null(x_include) | !is.null(rgx_x_include)) {
        if(is.null(x_include)) {
          x_include <- .get_from_rgx(data = data, rgx = rgx_x_include)
        }
        # stopifnot(all(x_include %in% names(data)))
        x_include <- intersect(names(data), x_include)
        vars_x <- .paste_formula_x(x = x_include, include = TRUE)
      } else {
        vars_x <- '.'
      }
      if (!is.null(x_exclude) | !is.null(rgx_x_exclude)) {
        if(is.null(x_exclude)) {
          x_exclude <- .get_from_rgx(data = data, rgx = rgx_x_exclude)
        }
        # stopifnot(all(x_exclude %in% names(data)))
        x_exclude <- intersect(names(data), x_exclude)
        vars_x_exclude <- .paste_formula_x(x = x_exclude, include = FALSE)
        vars_x <- paste0(vars_x, ' - ', vars_x_exclude)
      }
    } else {
      assertthat::assert_that(is.character(x))
      vars_x <- x
    }

    if (!intercept) {
      suffix <- ' + 0'
    } else {
      suffix <- ' + 1'
    }

    vars_x <- paste0(vars_x, suffix)

    formula_chr <- paste0(y, ' ~ ', vars_x)
    stats::as.formula(formula_chr)
  }
