
#' @rdname qplotmix
#'
#' @title Plot Multivariate Data into Colour Channels
#'
#' @description
#' **Note:** These functions are still experimental and may change in the future.
#'
#' - `legendright()` plots graph with legend right to it.
#'
#' @param p Plot object.
#' @param l Legend object.
#' @param legend.width,legend.unit Size of legend part.
#'
#' @return
#' - `legendright()`: invisible `NULL`.
#'
#' @seealso [qplotmixmap()]
#'
#' @export
#'
#' @concept ggplot2
#' @concept helpers
#'
#' @author Claudia Beleites

legendright <- function(p, l, legend.width = 8, legend.unit = "lines") {
  plot.new()
  pushViewport(viewport(layout = grid.layout(1, 2,
    widths = unit(c(1, legend.width), c("null", legend.unit))
  )))
  print(p, viewport(layout.pos.col = 1), newpage = FALSE)
  print(l, viewport(layout.pos.col = 2), newpage = FALSE)
  popViewport()
}


#' @rdname qplotmix
#'
#' @description
#' - `qmixtile()` plots multivariate data into colour channels using
#'   [ggplot2::geom_tile()].
#'
#' @param object Matrix to be plotted with mixed colour channels.
#' @param purecol Pure component colours, names determine legend labels.
#' @param mapping See [ggplot2::geom_tile()].
#' @param ...
#'
#' - `qmixtile()`: handed to [colmix_rgb()].
#' - `qmixlegend()` and `colmix_rgb()` hand further arguments to the `normalize`
#'    function.
#' @param map.tileonly If `TRUE`, `mapping` will be handed to
#'        [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].

qmixtile <- function(object,
                     purecol = stop("pure component colors needed."),
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     map.tileonly = FALSE) {

  ## calculate fill colours
  fill <- colmix_rgb(object[[f_rhs(mapping$fill)]], purecol, ...)
  object[[f_rhs(mapping$fill)]] <- fill

  if (map.tileonly) {
    p <- ggplot(object) +
      geom_tile(mapping = mapping, data = object)
  } else {
    p <- ggplot(object, mapping = mapping) +
      geom_tile()
  }

  p + scale_fill_identity() + theme(legend.position = "none")
}


#' @rdname qplotmix
#'
#' @description
#' - `normalize_colrange()` normalizes the range of each column to \[0, 1\].
#'
#' @param na.rm See [base::min()].
#' @param legend (logical)
#'        Should a legend be produced instead of normalized values?
#' @param n (integer) Number of colours to produce in legend.
#'
#' @return
#' - `normalize_colrange()` (and other `normalize_*()` functions):
#'   list with components `ymin`, `max` and `fill` to specify value and fill
#'   colour value (still *numerical*!) for the legend, otherwise the normalized
#'   values.
#'
#' @export

normalize_colrange <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  ## legend
  if (legend) {
    y <- apply(x, 2, function(x) seq(min(x), max(x), length.out = n))
    dy2 <- abs(y[2, ] - y[1, ]) / 2

    list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    ## normalized values
    x <- sweep(x, 2, apply(x, 2, min, na.rm = na.rm), `-`)
    sweep(x, 2, apply(x, 2, max, na.rm = na.rm), `/`)
  }
}


#' @rdname qplotmix
#'
#' @description
#' - `normalize_range()` normalizes the range of all columns to \[0, 1\].
#'
#' @export

normalize_range <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  if (legend) {
    y <- matrix(seq(min(x), max(x), length.out = n), nrow = n, ncol = ncol(x))
    dy2 <- abs(y[2, ] - y[1, ]) / 2

    list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    x <- x - min(x, na.rm = na.rm)
    x / max(x, na.rm = na.rm)
  }
}


#' @rdname qplotmix
#'
#' @description
#' - `normalize_null()` does not touch the values.
#'
#' @export

normalize_null <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  if (legend) {
    y <- apply(x, 2, function(x) seq(min(x), max(x), length.out = n))

    list(
      ymin = sweep(y, 2, min),
      ymax = sweep(y, 2, max),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    x
  }
}


#' @rdname qplotmix
#'
#' @description
#' - `normalize_minmax()` normalizes the range of each column j to \[min_j, max_j\].
#'
#' @param min numeric with value corresponding to "lowest" colour for each column.
#' @param max numeric with value corresponding to "highest" colour for each column.
#'
#' @export

normalize_minmax <- function(x, min = 0, max = 1, legend = FALSE, n = 100, ...) {
  if (legend) {
    y <- matrix(seq(0, 1, length.out = n), nrow = n, ncol = ncol(x))
    y <- sweep(y, 2, max - min, `*`)
    y <- sweep(y, 2, min, `+`)

    dy2 <- abs(y[2, ] - y[1, ]) / 2

    l <- list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      ymax = y + dy2,
      fill = matrix(seq(0, 1, length.out = n), nrow = n, ncol = ncol(x))
    )

    l$ymin[1, ] <- pmin(l$ymin[1, ], apply(x, 2, min, na.rm = TRUE))
    l$ymax[n, ] <- pmax(l$ymax[n, ], apply(x, 2, max, na.rm = TRUE))

    l
  } else {
    x <- sweep(x, 2, min, `-`)
    sweep(x, 2, max, `/`)
  }
}


#' @rdname qplotmix
#'
#' @description
#' - `qmixlegend()` legends for mixed colour plots.
#'
#' @param dx Width of label bar.
#' @param ny Number of colours in legend.
#' @param labels Component names.
#'
#' @return
#' - `qmixlegend()`: [ggplot2::ggplot()] object with legend.
#'
#' @export

# @author Claudia Beleites

qmixlegend <- function(x, purecol, dx = 0.33, ny = 100, labels = names(purecol),
                       normalize = normalize_colrange, ...) {
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  if (is.null(labels)) {
    labels <- colnames(x)
  }
  if (is.null(labels)) {
    labels <- seq_len(ncol(x))
  }

  if (!is.null(normalize)) {
    l <- normalize(x, ..., legend = TRUE)
  } else {
    l <- x
  }

  df <- data.frame()
  for (column in seq_along(purecol)) {
    tmp <- colmix_rgb(l$fill[, column, drop = FALSE], purecol[column],
      normalize = NULL, ...
    )

    df <- rbind(df, data.frame(
      column = labels[column],
      col = tmp,
      ymin = l$ymin[, column],
      ymax = l$ymax[, column]
    ))
  }
  df$column <- as.factor(df$column)
  df$xmin <- as.numeric(df$column) - dx
  df$xmax <- as.numeric(df$column) + dx

  l <- ggplot(df, aes(x = column), col = col) +
    geom_point(aes(x = column, y = 1), col = NA) +
    ylab("") +
    xlab("")
  l <- l + geom_rect(aes_string(
    xmin = "xmin", xmax = "xmax", ymin = "ymin", ymax = "ymax",
    fill = "col", colour = "col"
  ))

  l <- l + theme(
    plot.margin = unit(c(0.5, 0, 0, 0), "lines"),
    legend.position = "none"
  ) +
    scale_fill_identity() + scale_colour_identity()

  l
}


#' @rdname qplotmix
#'
#' @description
#' - `colmix_rgb()`: multi-channel colour mixing.
#'
#' @param x Matrix with component intensities in columns.
#' @param against Value to mix against
#'                (for `sub = TRUE` only, 1 = white, 0 = black)
#' @param sub Subtractive colour mixing?
#' @param normalize Function to normalize the values.
#'
#' @return
#' - `colmix_rgb()`: character vector with colours.
#'
#' @importFrom grDevices col2rgb rgb
#' @export

# @author Claudia Beleites

colmix_rgb <- function(x, purecol, against = 1, sub = TRUE,
                       normalize = normalize_colrange, ...) {
  if (!is.null(normalize)) {
    x <- normalize(x, ...)
  }

  if (is.character(purecol)) {
    purecol <- t(col2rgb(purecol)) / 255
  }

  if (sub) {
    x <- against - x %*% (against - purecol)
  } else {
    x <- x %*% purecol
  }

  x[x < 0] <- 0
  x[x > 1] <- 1

  cols <- rep(NA, nrow(x))
  cols[!is.na(x[, 1])] <- rgb(x[!is.na(x[, 1]), ])

  cols
}
