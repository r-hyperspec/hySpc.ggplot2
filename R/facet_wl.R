#' [!] Control facetting parameters in `ggplot` of `hyperSpec` object
#'
#' Control facetting parameters in plot made with function `ggplot`
#' of [`hyperSpec()`][hyperSpec-class] (`ggplot.hyperSpec` method)
#' in which parameter `wl.range` is explicitly specified.
#'
#' @details
#' This function is a wrapper for [ggplot2::facet_grid()] and
#' `facet_grid(.~.wl.range)` can be used instead of `facet_wl(".")`.
#'
#' @param rows Name of factor variable (quoted string) to be used for making
#' rows of facets. The dot "." (default) is used to indicate that there should
#' be no faceting on this dimension.
#'
#' @inheritParams ggplot2::facet_grid
#' @param ... Further parameters to be passed to [ggplot2::facet_grid()].
#'
#' @template ggplot-updated
#' @export
#'
#' @examples
#' library(spPlot)
#'
#' p <- ggplot(Spectra2, wl.range = c(min ~ 350, 500~600)) +
#'      geom_line(aes(color = gr))
#'
#' p
#' p + facet_wl(space = "fixed")
#' p + facet_wl("class")
#' p + facet_wl("gr")
#'
#' # Alternative to `facet_wl()` is:
#' p + facet_grid(. ~ .wl.range, scales = "free")
#'
#' \donttest{\dontrun{
#' # If `wl.range` is not specified, ERROR message is displayed:
#' p2 <- ggplot(Spectra2) + geom_line(aes(color = gr))
#' p2 + facet_wl()
#' }}
#'
#'
#' @author Vilmantas Gegzna
#'
#' @seealso Function [ggplot2::facet_grid()] from package \pkg{ggplot2}.
#' @family \pkg{spPlot} functions for spectroscopy and \pkg{hyperSpec}
#' @family \pkg{spPlot} functions for \pkg{ggplot2}
#'
facet_wl <- function(rows = ".", space = "free", scales = "free", ...){
    force(rows)
    facet <- as.formula(paste(rows , "~ .wl.range"))
    facet_grid(facet, space = space, scales = scales, ...)
}
