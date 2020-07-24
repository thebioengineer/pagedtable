#' main function for pagedtable
#' @export
#' @importFrom htmlwidgets createWidget sizingPolicy
#' @importFrom htmltools attachDependencies
#' @importFrom utils head
#'
#' @param x data.frame to be passed to pagedtable
#' @param ... arguments to be passed to \code{\link{format}}
#' @param use_rownames should rownames be displayed
#' @param pagerows rows to display
#' @param shadowDOM to or not use shadowDOM. Default to TRUE
#' @inheritParams htmlwidgets::createWidget
#'
#' @return pagedtable htmlwidget
#'
#' @examples
#' if(interactive()){
#'   pagedtable(mtcars)
#' }
#'
pagedtable <-
  function(x,
           ...,
           width = "100%",
           height = "auto",
           shadowDOM = TRUE) {

  stopifnot(inherits(x, "data.frame"))

  output <- match.arg(output)

  x <- head(x, n = getOption("pagedtable.max.print", 1000))

  pagedtable_list <-
    pagedtable_json(
      x,
      ...,
      use_rownames = use_rownames,
      shadowDOM = shadowDOM
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


#' @export
#' @importFrom htmlwidgets shinyWidgetOutput

pagedtableOutput <- function(outputId, width = "100%") {
  shinyWidgetOutput(outputId, "pagedtable", width, package = "pagedtable")
}

#' @export
#' @importFrom htmlwidgets shinyRenderWidget
renderPagedTable <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {expr <- substitute(expr)}
  shinyRenderWidget(expr, pagedtableOutput, env, quoted = TRUE)
}
