
#' @title Spectra plotting with \pkg{ggplot2}: `qplotspc()`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in future.
#'
#' @param x `hyperSpec` object
#' @param wl.range wavelength ranges to plot
#' @param ... handed to [ggplot2::geom_line()]
#' @param mapping see  [ggplot2::geom_line()]
#' @param spc.nmax maximum number of spectra to plot
#' @param map.lineonly if `TRUE`, `mapping` will be handed to
#' [ggplot2::geom_line()] instead of [ggplot2::ggplot()].
#' @param debuglevel if > 0, additional debug output is produced
#' @return a [ggplot2::ggplot()] object
#' @author Claudia Beleites
#' @concept ggplot2
#' @concept plots
#' @export
#'
#' @seealso
#' [hyperSpec::plotspc()]
#'
#' [ggplot2::ggplot()], [ggplot2::geom_line()]
#'
#' @examples
#' qplotspc(faux_cell)
#'
#' qplotspc(paracetamol, c(2800 ~ max, min ~ 1800)) +
#'   scale_x_reverse(breaks = seq(0, 3200, 400))
#'
#' qplotspc(aggregate(faux_cell, faux_cell$region, mean),
#'   mapping = aes(x = .wavelength, y = spc, colour = region)
#' ) +
#'   facet_grid(region ~ .)
#'
#' qplotspc(aggregate(faux_cell, faux_cell$region, mean_pm_sd),
#'   mapping = aes(x = .wavelength, y = spc, colour = region, group = .rownames)
#' ) +
#'   facet_grid(region ~ .)
#'
qplotspc <- function(x,
                     wl.range = TRUE, ...,
                     mapping = aes_string(
                       x = ".wavelength", y = "spc", group = ".rownames"
                     ),
                     spc.nmax = hy.getOption("ggplot.spc.nmax"),
                     map.lineonly = FALSE,
                     debuglevel = hy.getOption("debuglevel")) {
  chk.hy(x)
  validObject(x)

  ## cut away everything that isn't asked for before transforming to data.frame
  if (nrow(x) > spc.nmax) {
    if (debuglevel >= 1L) {
      message(
        "Number of spectra exceeds spc.nmax. Only the first ", spc.nmax,
        " are plotted."
      )
    }
    x <- x[seq_len(spc.nmax)]
  }

  wl.range <- wl2i(x, wl.range, unlist = FALSE)

  x <- x[, , unlist(wl.range), wl.index = TRUE]

  df <- as.long.df(x, rownames = TRUE, na.rm = FALSE) # with na.rm trouble with
  # wl.range

  ## ranges go into facets
  if (length(wl.range) > 1L) {
    tmp <- wl.range
    for (r in seq_along(tmp)) {
      tmp[[r]][TRUE] <- r
    }

    df$.wl.range <- rep(unlist(tmp), each = nrow(x))
  }


  df <- df[!is.na(df$spc), , drop = FALSE]
  if (map.lineonly) {
    p <- ggplot(df) +
      geom_line(mapping = mapping, ...)
  } else {
    p <- ggplot(df, mapping = mapping) +
      geom_line(...)
  }

  p <- p + xlab(labels(x, ".wavelength")) + ylab(labels(x, "spc"))

  if (!is.null(df$.wl.range)) {
    p <- p + facet_grid(. ~ .wl.range,
      labeller = as_labeller(rep(NA, nlevels(df$.wl.range))),
      scales = "free", space = "free"
    ) +
      theme(strip.text.x = element_text(size = 0))
  }

  p
}


# Unit tests -----------------------------------------------------------------
#' @import hySpc.testthat
hySpc.testthat::test(qplotspc) <- function() {
  context("qplotspc")
  # To update reference data for visual unit tests, run:
  # vdiffr::manage_cases()

  test_that("qplotspc() works", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_spectra <- hyperSpec:::generate_hy_spectra())
    # expect_silent(hy_profile <- hyperSpec:::generate_hy_profile())
    # expect_silent(hy_map     <- hyperSpec:::generate_hy_map())

    # Regular tests: warnings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests: errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_error(qplotspc(), 'argument "x" is missing, with no default')
    expect_silent(gg <- qplotspc(hy_spectra))
    expect_is(gg, "gg")
    expect_is(gg, "ggplot")

    expect_silent(gg2 <- qplotspc(hy_spectra, map.lineonly = TRUE))
    expect_is(gg2, "gg")
    expect_is(gg2, "ggplot")


    # Visual tests
    # vdiffr::expect_doppelganger("qplotspc-01",       gg)
  })
}


