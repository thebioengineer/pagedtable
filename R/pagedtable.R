#' main function for pagedtable
#' @export
#'
#' @param x data.frame to be passed to pagedtable
#' @param ... arguments to be passed to \code{\link{format}}
#' @param use_rownames should rownames be displayed
#' @param pagerows rows to display
#' @param shadowDOM to or not use shadowDOM. Default to TRUE
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
           use_rownames = getOption("pagedtable.rownames.print"),
           pagerows = 10,
           shadowDOM = TRUE) {

  stopifnot(inherits(x, "data.frame"))

  x <- head(x, n = getOption("pagedtable.max.print", 1000))

  pagedtable_list <-
    pagedtable_json(
      x,
      ...,
      use_rownames = use_rownames,
      pagerows = pagerows,
      shadowDOM = shadowDOM
    )

  # create the widget
  htmlwidgets::createWidget(
    name = "pagedtable",
    x = pagedtable_list,
    height = height,
    width = width,
    package = "pagedtable")

}


#' @export
pagedtableOutput <- function(outputId, width = "100%") {
  shinyWidgetOutput(outputId, "pagedtable", width, package = "pagedtable", )
}

#' @export
renderPagedTable <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {expr <- substitute(expr)}
  shinyRenderWidget(expr, pagedtableOutput, env, quoted = TRUE)
}



