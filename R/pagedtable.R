#' main function for pagedtable
#' @export
#' @importFrom htmlwidgets createWidget sizingPolicy
#' @importFrom htmltools attachDependencies
#' @importFrom utils head
#'
#' @param x data.frame to be passed to pagedtable
#' @param ... arguments to be passed to \code{\link{pagedtable_json}}
#' @inheritParams htmlwidgets::createWidget
#'
#' @return pagedtable htmlwidget
#'
#' @examples
#' if(interactive()){
#'   pagedtable(mtcars)
#' }
#'
#' @rdname pagedtable
#'
pagedtable <-
  function(x,
           ...,
           width = "100%",
           height = "auto") {

  stopifnot(inherits(x, "data.frame"))

  x <- head(x, n = getOption("pagedtable.max.print", 1000))

  pagedtable_list <-
    pagedtable_json(
      x,
      ...
    )

  # create the widget
  createWidget(
    name = "pagedtable",
    x = pagedtable_list,
    height = height,
    width = width,
    sizingPolicy = htmlwidgets::sizingPolicy(
      knitr.figure = FALSE, knitr.defaultWidth = "100%", knitr.defaultHeight = "auto",
      padding = 10
    ),
    package = "pagedtable")
}





#' Shiny bindings for pagedtable
#'
#' Output and render functions for using %s within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a pagedtable
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name pagedtable-shiny
#'
#' @export
#' @importFrom htmlwidgets shinyWidgetOutput
pagedtableOutput <- function(outputId, width = "100%", height = 'auto') {
  shinyWidgetOutput(outputId, "pagedtable", width = width, height = height, package = "pagedtable")
}

#' @rdname pagedtable-shiny
#' @export
#' @importFrom htmlwidgets shinyRenderWidget
renderPagedTable <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {expr <- substitute(expr)}
  shinyRenderWidget(expr, pagedtableOutput, env, quoted = TRUE)
}
