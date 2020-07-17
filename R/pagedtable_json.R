pagedtable_json <- function(x,
                            ...,
                            use_rownames = getOption("pagedtable.rownames.print"),
                            pagerows = 10,
                            shadowDOM = TRUE) {


  # read the data into json format
  formatted_contents <- lapply(colnames(x),
                               function(name, x){
                                 pagedtable_col(
                                   x = x[[name]],
                                   name = name
                                 )
                               },x)

  names(formatted_contents) <- colnames(x)


  if(use_rownames){
    formatted_contents <- c(list("_rn_" = list(
      content = rownames(x),
      columns = c(
        label = "",
        name = "_rn_",
        type = "",
        align = "left"
      )
    )),
    formatted_contents)
  }

  data <- jsonlite::toJSON(
    data.frame(lapply(formatted_contents, `[[`, 'content'),check.names = FALSE)
  )

  columns <- lapply(formatted_contents, `[[`, 'columns')
  names(columns) <- NULL

  options = list(
    "rows" =  c(min = pagerows),
    "shadowDOM" = shadowDOM
  )

  # pass the data and settings using 'pagedtable_list'
  list(
    data = data,
    columns = columns,
    options = options
  )
}
