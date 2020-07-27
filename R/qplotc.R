
#' @title Spectra plotting with \pkg{ggplot2}: `qplotc()`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in the future.
#'
#' @param object hyperSpec object
#' @param mapping see  [ggplot2::geom_point()]
#' @param ... handed to [ggplot2::geom_point()]
#' @export
#' @param func function to summarize the wavelengths, if `NULL`, only the first
#'   wavelength is used
#' @param func.args arguments to `func`
#' @param map.pointonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_point()] instead of [ggplot2::ggplot()].
#' @return a [ggplot2::ggplot()] object
#' @author Claudia Beleites
#' @concept ggplot2
#' @concept plots
#' @seealso [hyperSpec::plotc()]
#'
#'   [ggplot2::ggplot()], [ggplot2::geom_point()]
#' @examples
#' qplotc(flu)
#' qplotc(flu) + geom_smooth(method = "lm")
qplotc <- function(object, mapping = aes_string(x = "c", y = "spc"), ...,
                   func = NULL, func.args = list(),
                   map.pointonly = FALSE) {
  chk.hy(object)
  validObject(object)

  if (!is.null(func)) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  ## allow to plot against the row number
  object$.row <- seq(object, index = TRUE)

  ## find out whether the wavelengths are needed individually,
  ## if not, use only the first wavelength and issue a warning

  if (any(grepl("spc", sapply(mapping, as_label))) && # use intensities
    nwl(object) > 1 && # has > 1 wavelength
    is.null(func) && # no stats function
    !any(grepl("[.]wavelength", sapply(mapping, as_label)))) {
    object <- object[, , 1, wl.index = TRUE]
    warning("Intensity at first wavelengh only is used.")
  }

  ## produce fancy y label
  ylabel <- labels(object, as_label(mapping$y))
  if (is.null(ylabel)) {
    ylabel <- as_label(mapping$y)
  }

  if (!is.null(func)) {
    ylabel <- make.fn.expr(substitute(func), c(ylabel, func.args))
  }
  ylabel <- as.expression(ylabel)

  ## expand the data.frame
  df <- as.long.df(object, rownames = TRUE, wl.factor = TRUE)

  ## if plots should be grouped, faceted, etc. by wavelength, it is better to
  ## have a factor
  if (any(grepl(
    "[.]wavelength",
    sapply(mapping[!names(mapping) %in% c("x", "y")], as_label)
  ))
  ) {
    df$.wavelength <- as.factor(df$.wavelength)
  }

  if (map.pointonly) {
    p <- ggplot(df) +
      geom_point(mapping = mapping, ...)
  } else {
    p <- ggplot(df, mapping = mapping) +
      geom_point(...)
  }

  xlabel <- labels(object)[[as_label(mapping$x)]]
  if (is.null(xlabel)) {
    xlabel <- as_label(mapping$x)
  }

  p + ylab(ylabel) +
    xlab(xlabel)
}
