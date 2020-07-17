#' main function for pagedtable
#' @export
#'
#' @param x data.frame to be passed to pagedtable
#' @param ... arguments to be passed
#' @param pagerows rows to display
#' @param shadowDOM to or not use shadowdom
#'
pagedtable<- function(x, ..., width = "100%", height = "400px", pagerows = 10, shadowDOM = TRUE){
  # read the data into json format
  quiet <- capture.output({
    dat_out <- data.frame(lapply(x,format,...))
    rownames(dat_out) <- rownames(x)
    data <- jsonlite::toJSON(dat_out)
  })

  columns <- lapply(colnames(x), function(c_name){
    c(name = c_name, type = class(x[[c_name]]), html = ifelse(class(x) == "color_vctr",TRUE,FALSE))
  })

  if (!is.null(rownames(x))) {
    columns <- c(list(c(
      label = "",
      name = "_row",
      type = "",
      align = "left"
    )),
    columns)
  }

  options = list(
    "rows" =  c(min = pagerows),
    "shadowDOM" = shadowDOM
  )

  # pass the data and settings using 'x'
  x <- list(
    data = data,
    columns = columns,
    options = options
  )

  # create the widget
  htmlwidgets::createWidget("pagedtable", x, width = width, height = height)

}


#' @export
pagedtableOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "pagedtable",width, height, package = "pagedtable")
}

#' @export
renderPagedTable <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {expr <- substitute(expr)}
  shinyRenderWidget(expr, pagedtableOutput, env, quoted = TRUE)
}



