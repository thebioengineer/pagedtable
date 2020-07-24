#' Generate JSON for pagedtable
#' @export
#'
#' @description pagedtable accepts a json that defines the behavior of pagedtable
#' with the data including: nrows, column names, labels, etc. This function
#' accepts the data.frame to print and generates the json for paged table
#'
#' @param x data.frame to show in the pagedtable
#' @param ... arguments to be passed to \link{\code{format}}
#' @param use_rownames boolean value. should rownames be used?
#' @param pagerows integer value defining the number of rows to display for
#' each page of the pagedtable
#' @param shadowDOM boolean value, should the shadowDOM be used? defaults
#' to yes.
#'
#' @return list object with three entries: data, columns, and options
#' @examples
#' pagedtable_input <- pagedtable_json(mtcars)
#'
#' @importFrom jsonlite toJSON
pagedtable_json <- function(x,
                            ...,
                            use_rownames = getOption("pagedtable.rownames.print"),
                            pagerows_min = 10,
                            pagerows_max = 100,
                            pagecols_min = 1,
                            pagecols_max = 100,
                            shadowDOM = TRUE) {


  # read the data into json format
  formatted_contents <- lapply(colnames(x),
                               function(name, x){
                                 pagedtable_col(
                                   x = x[[name]],
                                   name = name,
                                   ...
                                 )
                               },x)

  names(formatted_contents) <- colnames(x)


  if(use_rownames){
    formatted_contents <- c(list("_rn_" = list(
      content = rownames(x),
      columns = list(
        label = "",
        name = "_rn_",
        type = "",
        align = "left"
      )
    )),
    formatted_contents)
  }

  data <- toJSON(
    data.frame(lapply(formatted_contents, `[[`, 'content'),check.names = FALSE)
  )

  columns <- lapply(formatted_contents, `[[`, 'columns')
  names(columns) <- NULL

  options = list(
    "cols" = list(
      min = pagecols_min,
      max = pagecols_max
    ),
    "rows" = list(
      min = pagerows_min,
      max = pagerows_max
      ),
    "shadowDOM" = shadowDOM
  )

  list(
    data = data,
    columns = columns,
    options = options
  )
}
