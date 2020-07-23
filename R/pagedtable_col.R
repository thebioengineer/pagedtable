#' Set pagedtable behavior for columns
#'
#' @description Table column behavior may need to be defined for new/custom s3 vector types.
#' This s3 method allows for that to be defined!
#'
#' @param x vector to be parsed
#' @param name name to set for the column. By default it will be the column name in the data.frame
#' @param ... arguments to be passed to \code{\link{format}}
#'
#' @export
#' @exportMethod pagedtable_col
#' @return list containing two elements, the formatted column, and a series of options
#'  as defined by pagedtable.js that can change the behavior of the column.
#' @examples
#'
#' ## normal vectors
#' vector_columns <- pagedtable_col(LETTERS)
#'
#' ## cases like list_col's that are in tibbles
#' my_list_col <- list(list(1234),"myvalue")
#' list_columns <- pagedtable_col(my_list_col)
#'
pagedtable_col <- function(x, name = substitute(x), ...){
  UseMethod("pagedtable_col", x)
}

#' @export
#' @describeIn  pagedtable_col default method for printing column content
pagedtable_col.default <- function(x, name = substitute(x), ...){

  col_values <- format(x, ...)
  baseType <- class(x)[[1]]
  type <- type_sum(x)
  align = if (baseType == "character" || baseType == "factor") "left" else "right"

  list(
    content = col_values,
    columns = list(
      name  = name,
      label = name,
      type  = type,
      align = align
      )
  )
}

#' @export
#' @describeIn pagedtable_col list method for printing out list-cols
pagedtable_col.list <- function(x, name = substitute(x), ...) {

  summaries <- lapply(x, obj_summary, ...)

  col_values <- do.call('c',lapply(summaries, `[[`,"value"))

  baseType <- class(x)[[1]]
  type <- type_sum(x)
  align = if (baseType == "character" || baseType == "factor") "left" else "right"

  list(
    content = col_values,
    columns = list(
      name  = name,
      label = name,
      type  = type,
      align = align
    )
  )
}
