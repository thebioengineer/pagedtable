#' Define how to summarize objects that are contents of list cols
#'
#' @description Table columns can have a class of type "list" which can contain multiple
#' types. This is an option for how to summarize those object types.
#'
#' @param x list element to be parsed
#' @param ... arguments to be passed to internal functions. not used right now
#'
#' @export
#' @exportMethod obj_summary
#' @return string that summarizes the list contents
#' @examples
#' list_example <- list(
#'     val1 = LETTERS,
#'     val2 = "1.02e23",
#'     val2 = data.frame(x = 1)
#'    )
#'
#' ## summarize various types
#' obj_summary(list_example[[1]])
#' obj_summary(list_example[[2]])
#' obj_summary(list_example[[3]])
#'
obj_summary <- function(x, ...) {
  UseMethod("obj_summary")
}

#' @describeIn  obj_summary default method, reports object type and dimensions
#' @export
obj_summary.default <- function(x, ...){
  summary <- paste0(type_sum(x), size_sum(x))
  list(
    value = summary,
    deps = NULL
  )
}

#' @export
#' @describeIn obj_summary POSIXlt method, reports number of POSIXlt
obj_summary.POSIXlt <- function(x, ...){
  summary <- rep("POSIXlt", length(x))
  list(
    value = summary,
    deps = NULL
  )
}

#' @export
#' @describeIn obj_summary list method, calculates the typesum and size, summarizes
obj_summary.list <- function(x,...){
  summary <- vapply(x, function(x){paste0(type_sum(x), size_sum(x))}, character(1L))
  list(
    value = summary,
    deps = NULL
  )
}

