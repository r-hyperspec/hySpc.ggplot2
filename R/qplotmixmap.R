
#' @title Plot False-Colour Map with Colour Mixing for Multivariate Overlay (\pkg{ggplot2})
#'
#' @description
#' This function plots false-colour map with colour mixing for multivariate
#' overlay by using \pkg{ggplot2} functions.
#'
#' **Note:** The function is still experimental and may change in the future.
#'
#' @param object A [`hyperSpec`][hyperSpec::hyperSpec-class] object.
#' @param ... Further arguments handed to [qmixlegend()] and [qmixtile()].
#'
#' @return An invisible list with [ggplot2::ggplot()] objects for map and legend.
#'
#' @importFrom grid pushViewport viewport popViewport grid.layout unit
#' @importFrom lazyeval f_rhs
#'
#' @export
#'
#' @concept ggplot2
#' @concept quick plots
#'
#' @seealso [qmixtile()]
#'
#' @author Claudia Beleites
#'
#' @examples
#' set.seed(1)
#' faux_cell <- generate_faux_cell()
#'
#' # Original data at certan wavelengths
#' qplotmixmap(faux_cell[, , c(800, 1200, 1500)],
#'   purecol = c(matrix = "red", cell = "green", nucleus = "blue")
#' )
#'
#' # With baseline removed
#' faux_cell_2 <- faux_cell - spc_fit_poly_below(faux_cell)
#'
#' qplotmixmap(faux_cell_2[, , c(800, 1200, 1500)],
#'   purecol = c(matrix = "red", cell = "green", nucleus = "blue")
#' )
#'
#' # With further pre-processing
#' faux_cell_3 <- faux_cell_2
#' faux_cell_3 <- sweep(faux_cell_3, 1, apply(faux_cell_3, 1, mean), "/")
#' faux_cell_3 <- sweep(faux_cell_3, 2, apply(faux_cell_3, 2, quantile, 0.05), "-")
#'
#' qplotmixmap(faux_cell_3[, , c(800, 1200, 1500)],
#'   purecol = c(matrix = "red", cell = "green", nucleus = "blue")
#' )

qplotmixmap <- function(object, ...) {
  p <- qmixtile(object@data, ...) +
    coord_equal()

  xlabel <- labels(object)[[as_label(p$mapping$x)]]
  if (is.null(xlabel)) {
    xlabel <- as_label(p$mapping$x)
  }

  ylabel <- labels(object)[[as_label(p$mapping$y)]]
  if (is.null(ylabel)) {
    ylabel <- as_label(p$mapping$y)
  }

  p <- p +
    xlab(xlabel) +
    ylab(ylabel)

  l <- qmixlegend(object@data$spc, ...)

  legend_right(p, l)

  invisible(list(map = p, legend = l))
}

# Unit tests -----------------------------------------------------------------
#' @import hySpc.testthat
hySpc.testthat::test(qplotmixmap) <- function() {
  # To update reference data for visual unit tests, run:
  # vdiffr::manage_cases()

  test_that("qplotmixmap() works", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_map <- hyperSpec:::generate_hy_map())

    # Regular tests: warnings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests: errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_error(qplotmixmap(), 'argument "object" is missing, with no default')
    expect_warning(
      gg <- qplotmixmap(
        hy_map[, , c(5000, 6500, 8000)],
        purecol = c(colg = "red", Phe = "green", Lipid = "blue")
      )
    )
    expect_type(gg, "list")
    expect_length(gg, 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Visual tests
    # vdiffr::expect_doppelganger("qplotspc-01",       gg)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  test_that("qplotmixmap() parameter `normalize` works", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    expect_silent(hy_map <- hyperSpec:::generate_hy_map())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_warning(
      gg <- qplotmixmap(
        hy_map[, , c(5000, 6500, 8000)],
        purecol = c(colg = "red", Phe = "green", Lipid = "blue"),
        normalize = normalize_colrange
      )
    )
    expect_type(gg, "list")
    expect_length(gg, 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_warning(
      gg <- qplotmixmap(
        hy_map[, , c(5000, 6500, 8000)],
        purecol = c(colg = "red", Phe = "green", Lipid = "blue"),
        normalize = normalize_range
      )
    )
    expect_type(gg, "list")
    expect_length(gg, 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_warning(
      gg <- qplotmixmap(
        hy_map[, , c(5000, 6500, 8000)],
        purecol = c(colg = "red", Phe = "green", Lipid = "blue"),
        normalize = normalize_minmax
      )
    )
    expect_type(gg, "list")
    expect_length(gg, 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # FIXME: fails, if `normalize = normalize_null`

    # expect_warning(
    #   gg <- qplotmixmap(
    #     purecol = c(colg = "red", Phe = "green", Lipid = "blue"),
    #     normalize = normalize_null,
    #     hy_map[, , c(5000, 6500, 8000)]
    #   )
    # )
    # expect_type(gg, "list")
    # expect_length(gg, 2)
  })
}
