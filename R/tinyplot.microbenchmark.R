#' Tinyplot method for microbenchmark objects
#'
#' @description Uses the `tinyplot` package to produce prettier base graphics
#'   for microbenchmark timings. Note that `tinyplot` needs to be installed and
#'   loaded separately. 
#'
#' @param x A microbenchmark object.
#' @param type String giving the type of plot representation. One of `"violin"`
#'   (the default), `"boxplot"`, or `"jitter"`.
#' @param order Names of output column(s) to order the results.
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param unit The unit to use for graph labels.
#' @param ylim Numeric vector of length 2, giving the limits of the timings in 
#'   the unit automatically chosen for for the time axis. (Note that, in the
#'   default case where `flip = TRUE`, the timings will appear on the x-axis.)
#'   If no argument is provided, then will revert to the minimum and maximum
#'   recorded values.
#' @param main Title of the plot.
#' @param flip If \code{TRUE} the plot will be flipped on its side (default).
#' @param trim If \code{TRUE} the violin plots will be trimmed.
#' @param joint.bw If \code{TRUE} use a joint bandwidth for violin plots.
#' @param ... Additional arguments passed to [`tinyplot`].
#' @return None. Called for side effect of producing a plot.
#'
#' @examples
#' if (requireNamespace("tinyplot", quietly = TRUE)) {
#'     library(tinyplot)
#'     
#'     tm <- microbenchmark(rchisq(100, 0),
#'                          rchisq(100, 1),
#'                          rchisq(100, 2),
#'                          rchisq(100, 3),
#'                          rchisq(100, 5), times=1000L)
#'     
#'     tinyplot(tm)
#'     
#'     # aesthetic tweak example with custom title
#'     tinytheme("classic")
#'     tinyplot(tm, fill = "transparent",
#'              main = "Impressive benchmarks",
#'              sub = "Brought to you by tinyplot")
#' 
#'     # we can use the tinyplot scaffolding to add layers to our plot
#'     tinyplot_add(type = "jitter", pch = ".", alpha = 0.3)
#'     
#'     # reset theme
#'     tinytheme()
#' }
#' @author Grant McDermott
#' @export
tinyplot.microbenchmark = function(
   x,
   type = c("violin", "boxplot", "jitter"),
   order = NULL,
   log = TRUE,
   unit = NULL,
   ylim = NULL,
   main = "microbenchmark timings",
   flip = TRUE,
   trim = TRUE,
   joint.bw = FALSE,
   ...
) {
# browser()

  type <- match.arg(type)

  y_min <- 0
  
  unit <- determine_unit(x, unit)
  x$ntime <- convert_to_unit(x, unit)
  y_max <- max(x$ntime)
  if (!is.null(order)) {
    s <- summary(x)
    x_colnames <- colnames(s)
    order <- match.arg(order, x_colnames, several.ok=TRUE)
    new_order <- do.call("order", c(s[, order, drop=FALSE], decreasing=TRUE))
    x$expr <- factor(x$expr, levels = levels(x$expr)[new_order])
  }
  
  y_label <- sprintf("Time (%s) for neval = %d",
                     attr(x$ntime, "unit"),
                     nrow(x) / length(levels(x$expr)))
  if (log) {
    y_min <- if (min(x$time) == 0) 1 else min(x$ntime)
    log = "y"
  } else {
    log = NULL
  }

  if (is.null(ylim)) {
    ylim = c(y_min, y_max)
  } else if (length(ylim) != 2 || !is.numeric(ylim)) {
    warning("`ylim` must be a numeric vector of length 2; reverting to defaults.")
    ylim = c(y_min, y_max)
  }
  
  tinyplot::tinyplot(
    ntime ~ expr,
    data = x,
    type = type,
    ylab = y_label,
    main = main, 
    log = log,
    trim = trim,
    flip = flip,
    joint.bw = joint.bw,
    ylim = ylim,
    ...
  )
  
}
