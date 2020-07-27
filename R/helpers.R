
make.fn.expr <- function(fn, l = list()) {
  if (length(fn) > 1L) {
    fn <- "f"
  }

  l <- lapply(l, function(x) if (is.logical(x)) as.character(x) else x)

  if (is.null(names(l))) {
    names(l) <- rep("", length(l))
  }

  tmp <- mapply(
    function(x, y) if (nzchar(x) > 0L) bquote(.(x) == .(y)) else y,
    names(l), l
  )

  e <- expression(f(x))
  e[[1]][[1]] <- fn

  if (length(tmp) > 0L) {
    e[[1]][seq_along(tmp) + 1] <- tmp

  } else {
    e[[1]][2] <- NULL
  }

  e
}
