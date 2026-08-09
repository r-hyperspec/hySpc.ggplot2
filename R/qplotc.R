
#' @title Plot Spectroscopic Profile with \pkg{ggplot2}
#'
#' @description
#' This function plots spectroscopic concenration, depth, time-series, etc.
#' profiles by using \pkg{ggplot2}.
#'
#' **Note:** The function is still experimental and may change in the future.
#'
#' @param object A [`hyperSpec`][hyperSpec::hyperSpec-class] object.
#' @param mapping See [ggplot2::geom_point()].
#' @param ... Further arguments handed to [ggplot2::geom_point()].
#' @param func Function to summarize the wavelengths. If `NULL`, only the first
#'        wavelength is used.
#' @param func.args A named list with arguments to `func`.
#' @param map.pointonly If `TRUE`, `mapping` will be handed to
#'        [ggplot2::geom_point()] instead of [ggplot2::ggplot()].
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @export
#'
#' @concept ggplot2
#' @concept quick plots
#'
#' @seealso
#' - [hyperSpec::plot_c()];
#' - [ggplot2::ggplot()], [ggplot2::geom_point()].
#'
#' @author Claudia Beleites
#'
#' @examples
#' qplotc(flu) # Notice the warning
#'
#' qplotc(flu[ , , 410])
#'
#' qplotc(flu[ , , 410]) + geom_smooth(method = "lm", formula = y ~ x)
#'
#' qplotc(flu, func = mean)

qplotc <- function(object, mapping = aes_string(x = "c", y = "spc"), ...,
                   func = NULL, func.args = list(),
                   map.pointonly = FALSE) {
  assert_hyperSpec(object)
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
    ylabel <- make_fn_expr(substitute(func), c(ylabel, func.args))
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

# Unit tests -----------------------------------------------------------------
#' @import hySpc.testthat
hySpc.testthat::test(qplotc) <- function() {
  # To update reference data for visual unit tests, run:
  # vdiffr::manage_cases()

  test_that("qplotc() works", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_spectra <- hyperSpec:::generate_hy_spectra())
    expect_silent(hy_profile <- hyperSpec:::generate_hy_profile())

    # Regular tests: warnings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests: errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_error(qplotc(), 'argument "object" is missing, with no default')

    expect_silent(gg <- qplotc(hy_profile))
    expect_s3_class(gg, "gg")
    expect_s3_class(gg, "ggplot")

    expect_silent(gg2 <- qplotc(hy_profile, map.pointonly = TRUE))
    expect_s3_class(gg2, "gg")
    expect_s3_class(gg2, "ggplot")

    expect_silent(gg3 <- qplotc(hy_spectra, func = mean))
    expect_s3_class(gg3, "gg")
    expect_s3_class(gg3, "ggplot")

    expect_warning(
      gg4 <- qplotc(hy_spectra),
      "Intensity at first wavelengh only is used"
    )
    expect_s3_class(gg4, "gg")
    expect_s3_class(gg4, "ggplot")

    # Visual tests
    # vdiffr::expect_doppelganger("qplotspc-01",       gg)
  })
}


