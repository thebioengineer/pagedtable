#' Set PagedTable behavior for columns
#'
#' @description Table column behavior may need to be defined for new/custom s3 vector types.
#' This s3 method allows for that to be defined!
#'
#' @param x vector to be parsed
#' @param ... arguments to be passed to \code{\link{format}}
#'
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
  UseMethod("pagedtable_col")
}

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

pagedtable_col.list <- function(x, name = substitute(x), ...) {
  col_values <- do.call('c',
    lapply(x, function(x_list) {
      summary <- obj_sum(x_list)
      paste0("<", summary, ">")
    }))

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
