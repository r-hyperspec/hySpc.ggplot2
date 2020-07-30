
#' @title Quick False-Colour Map with \pkg{ggplot2}: `qplotmap()`
#'
#' @description
#' This function plots false-colour map by using \pkg{ggplot2} functions.
#'
#' Note that `qplotmap()` will currently produce wrong scales if `x` or `y` are
#' discrete.
#'
#' **Note:** The function is still experimental and may change in the future.
#'
#' @param object A [`hyperSpec`][hyperSpec::hyperSpec-class] object.
#' @param mapping See [ggplot2::geom_tile()].
#' @param ... Further arguments handed to [ggplot2::geom_tile()].
#' @param func Function to summarize the wavelengths.
#' @param func.args A named list with arguments to `func`.
#' @param map.tileonly If `TRUE`, `mapping` will be handed to
#'        [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @importFrom utils tail
#' @importFrom rlang as_label
#'
#' @export
#'
#' @concept ggplot2
#' @concept quick plots
#'
#' @seealso
#' - [hyperSpec::plotmap()];
#' - [ggplot2::ggplot()], [ggplot2::geom_tile()].
#'
#' @author Claudia Beleites
#'
#' @examples
#' set.seed(1)
#' faux_cell <- generate_faux_cell()
#'
#' qplotmap(faux_cell[ , , 1500])
#'
#' qplotmap(faux_cell[ , , 800])
#'
#' qplotmap(faux_cell[ , , 1200]) +
#'    scale_fill_gradientn(colours = alois.palette())

qplotmap <- function(object,
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     func = mean,
                     func.args = list(),
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

# Unit tests -----------------------------------------------------------------
#' @import hySpc.testthat
hySpc.testthat::test(qplotmap) <- function() {
  context("qplotmap")
  # To update reference data for visual unit tests, run:
  # vdiffr::manage_cases()

  test_that("qplotmap() works", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_map     <- hyperSpec:::generate_hy_map())

    # Regular tests: warnings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests: errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_error(qplotmap(), 'argument "object" is missing, with no default')
    expect_silent(gg <- qplotmap(hy_map))
    expect_is(gg, "gg")
    expect_is(gg, "ggplot")

    expect_silent(gg2 <- qplotmap(hy_map, map.tileonly = TRUE))
    expect_is(gg2, "gg")
    expect_is(gg2, "ggplot")

    # With no labels
    hy_map_2 <- hy_map
    labels(hy_map_2) <- lapply(labels(hy_map), function(x) NULL)

    expect_silent(gg3 <- qplotmap(hy_map_2, map.tileonly = TRUE))
    expect_is(gg3, "gg")
    expect_is(gg3, "ggplot")

    # Visual tests
    # vdiffr::expect_doppelganger("qplotspc-01",       gg)
  })
}
