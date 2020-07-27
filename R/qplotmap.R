
#' @title Spectra plotting with \pkg{ggplot2}: `qplotmap().`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in future.
#'
#' Note that `qplotmap()` will currently produce the wrong scales if x or y are
#' discrete.
#'
#' @param object  hyperSpec object
#' @param mapping see  [ggplot2::geom_tile()]
#' @param ... handed to [ggplot2::geom_tile()]
#' @param func function to summarize the wavelengths
#' @param func.args arguments to `func`
#' @param map.tileonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].
#' @return a [ggplot2::ggplot()] object
#' @export
#' @md
#' @author Claudia Beleites
#' @concept ggplot2
#' @concept plots
#' @seealso [plotmap()]
#'
#'   [ggplot2::ggplot()], [ggplot2::geom_tile()]
#' @examples
#' qplotmap(faux_cell)
#' qplotmap(faux_cell) + scale_fill_gradientn(colours = alois.palette())
#' @importFrom utils tail
#' @importFrom rlang as_label
qplotmap <- function(object,
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     func = mean, func.args = list(),
                     map.tileonly = FALSE) {
  chk.hy(object)
  validObject(object)

  if (nwl(object) > 1 & !is.null(func)) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  if (map.tileonly) {
    p <- ggplot(as.long.df(object)) +
      geom_tile(mapping = mapping)
  } else {
    p <- ggplot(as.long.df(object), mapping = mapping) +
      geom_tile()
  }

  p <- p + coord_equal()

  ## set expand to c(0, 0) to suppress the gray backgroud
  if (is.factor(with(p$data, eval(p$mapping$x)))) {
    p <- p + scale_x_discrete(expand = c(0, 0))
  } else {
    p <- p + scale_x_continuous(expand = c(0, 0))
  }

  if (is.factor(with(p$data, eval(p$mapping$y)))) {
    p <- p + scale_y_discrete(expand = c(0, 0))
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0))
  }

  ## generate axis/scale labels
  ## TODO: own function
  x <- as_label(mapping$x)
  xlabel <- labels(object)[[tail(x, 1)]]
  if (is.null(xlabel)) xlabel <- x

  y <- as_label(mapping$y)
  ylabel <- labels(object)[[tail(y, 1)]]
  if (is.null(ylabel)) ylabel <- y

  f <- as_label(mapping$fill)
  flabel <- labels(object)[[tail(f, 1)]]
  if (is.null(flabel)) flabel <- f

  p + labs(x = xlabel, y = ylabel, fill = flabel)
}
